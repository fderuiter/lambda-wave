# **Lambda-Wave Development Roadmap & Validation Plan**

**Project:** Lambda-Wave (Haskell Radar SGRT)

**Compliance Target:** IEC 62304 Class C / ISO 14971

**Status:** Phase 1 (In Progress)

## **Phase 1: Infrastructure & High-Assurance Setup**

**Goal:** Establish a deterministic runtime environment and rigid CI/CD pipeline capable of supporting Class C safety claims.

* \[ \] **1.1. Toolchain & RTS Locking**  
  * \[ \] **Task:** Configure GHC Runtime System (RTS) flags in cabal.project and Main.hs to lock capabilities to specific cores.  
  * \[ \] **Requirement:** SR-SOUP-001  
  * \[ \] **Implementation:** Use \-N2 (min) and \-qa (affinity). Implement Control.Concurrent.setNumCapabilities.  
  * \[ \] **Validation:**  
    * \[ \] Run threadscope on the binary.  
    * \[ \] Verify GC pause times are \< 5ms under load using \+RTS \-s.  
* \[ \] **1.2. CI/CD Strictness**  
  * \[ \] **Task:** Update .github/workflows/build-and-test.yml to fail on *any* compiler warning (-Werror).  
  * \[ \] **Requirement:** IEC 62304 (Code Standards)  
  * \[ \] **Implementation:** Add ghc-options: \-Wall \-Werror to all stanza in .cabal.  
  * \[ \] **Validation:**  
    * \[ \] Submit a PR with an unused variable; verify CI fails.  
* \[ \] **1.3. Docker Determinism**  
  * \[ \] **Task:** Finalize Dockerfile to use a specific SHA-256 digest for the GHC base image (reproducible builds).  
  * \[ \] **Implementation:** Lock haskell:9.4.7 (or similar) digest.  
  * \[ \] **Validation:**  
    * \[ \] Build image on two different machines; verify binary checksums match (if possible) or environment variables are identical.

## **Phase 2: Hardware Abstraction Layer (Ingestion)**

**Goal:** Achieve reliable, zero-copy data ingestion from the TI IWR6843ISK.

* [x] **2.1. C++ Ring Buffer Completion**
  * [x] **Task:** Complete the implementation of cbits/src/ring_buffer.cpp to handle atomic write pointers.
  * [x] **Requirement:** FR-DAQ-001, FR-DAQ-004
  * [x] **Implementation:** Ensure std::atomic<size_t> is used for head/tail.
  * [x] **Validation:**
    * [x] **Unit Test:** test/FFI/RingBuffer/IOSpec.hs - Write 1M items, read 1M items, ensure 0 drops.
    * [x] **Memcheck:** Run valgrind to ensure no leaks in C++ layer.
* [x] **2.2. UDP Packet Parser**
  * [x] **Task:** Implement the packet parser in src/Hardware/Consumer.hs to handle TI TLV (Type-Length-Value) formats.
  * [x] **Requirement:** FR-DAQ-003
  * [x] **Implementation:** Parse "Magic Word" 0x0102030405060708 and TLV headers.
  * [x] **Validation:**
    * [x] **Fuzz Testing:** Feed random ByteStrings; ensure system does not crash but logs "Corrupt Packet".
    * \[ \] **Integration:** Replay a captured .bin file from TI mmWave Studio and verify frame count matches.  
* \[ \] **2.3. Sensor Configuration**  
  * \[ \] **Task:** Implement serial port writer to send .cfg to the sensor.  
  * \[ \] **Requirement:** FR-DAQ-002  
  * \[ \] **Implementation:** Use System.Hardware.Serialport (or equivalent) in src/Hardware/Control.hs.  
  * \[ \] **Validation:**  
    * \[ \] Connect sensor, run init; verify console output from sensor says "Done".

## **Phase 3: Signal Processing Core (Physics Engine)**

**Goal:** Convert raw radar ADC data/Point Clouds into sub-millimeter respiratory signals.

* \[ \] **3.1. Background Subtraction**  
  * \[ \] **Task:** Implement static clutter removal in src/SignalProcessing/FMCW.hs.  
  * \[ \] **Requirement:** FR-DSP-001  
  * \[ \] **Implementation:** ![][image1].  
  * \[ \] **Validation:**  
    * \[ \] **Scenario:** Place static metal object (trihedral reflector). Run algorithm. Resulting signal amplitude should be \~0.  
* \[ \] **3.2. Phase Extraction & Unwrapping**  
  * \[ \] **Task:** Implement atan2(Q, I) and the unwrap logic to handle jumps ![][image2].  
  * \[ \] **Requirement:** FR-DSP-002, FR-DSP-004  
  * \[ \] **Implementation:** SignalProcessing.FMCW.unwrapPhase.  
  * \[ \] **Validation:**  
    * \[ \] **Math Test:** Feed synthetic sine wave with phase wrap; verify output is a smooth continuous sine wave.  
* \[ \] **3.3. Kalman Filter Integration**  
  * \[ \] **Task:** Implement the State Estimation vector ![][image3] in src/SignalProcessing/Regression.hs.  
  * \[ \] **Requirement:** FR-DSP-003  
  * \[ \] **Implementation:** Standard linear Kalman filter (Matrix operations).  
  * \[ \] **Validation:**  
    * \[ \] **Simulation:** Generate noisy sine wave (SNR 10dB). Compare Filter output vs. Ground Truth. RMSE must be ![][image4]mm.

## **Phase 4: Safety & Control (The "Class C" Core)**

**Goal:** Guarantee fail-safe operation and deterministic beam control.

* \[ \] **4.1. Watchdog Thread**  
  * \[ \] **Task:** Implement the "Heartbeat" monitor in src/Safety/Watchdog.hs.  
  * \[ \] **Requirement:** SR-WD-001, SR-WD-002  
  * \[ \] **Implementation:** TVarying map of thread timestamps. If now \- last\_seen \> 100ms, kill.  
  * \[ \] **Validation:**  
    * \[ \] **Fault Injection:** Manually insert threadDelay 200000 (200ms) in the processing loop. Verify Watchdog kills app and logs error.  
* \[ \] **4.2. Gating Logic & Latency**  
  * \[ \] **Task:** Link Kalman State to IO Triggers (GPIO/TTL).  
  * \[ \] **Requirement:** FR-GAT-001, FR-GAT-002  
  * \[ \] **Implementation:** Control.Gating.evaluateGating \-\> Hardware.Control.setBeam.  
  * \[ \] **Validation:**  
    * \[ \] **Latency Bench:** Run bench/LatencyBench.hs. Mean processing time must be ![][image5]ms. 99th percentile ![][image6]ms.  
* \[ \] **4.3. Audit Logging**  
  * \[ \] **Task:** Finalize immutable logging to disk.  
  * \[ \] **Requirement:** SR-AUDIT-001  
  * \[ \] **Implementation:** Ensure logs flush to disk immediately on "Beam Hold" events.  
  * \[ \] **Validation:**  
    * \[ \] Pull power plug (simulated crash). Check log file on reboot. Last event must be recorded.

## **Phase 5: User Interface & Visualization**

**Goal:** Provide clinical situational awareness (Non-Critical, Class A/B).

* \[ \] **5.1. Real-Time Plotting**  
  * \[ \] **Task:** Connect Gloss or OpenGL renderer to the Data Stream.  
  * \[ \] **Requirement:** FR-UI-001  
  * \[ \] **Validation:**  
    * \[ \] Visual check: Does the wave move smoothly? (Update rate \> 30Hz).  
* \[ \] **5.2. Visual Alerts**  
  * \[ \] **Task:** Implement Green/Red background state based on Gating Decision.  
  * \[ \] **Requirement:** FR-UI-002  
  * \[ \] **Validation:**  
    * \[ \] Simulate "Cough" (high velocity). Screen must flash Red instantly.

## **Phase 6: System Validation (Verification)**

**Goal:** Final "Black Box" testing against physical reality.

* \[ \] **6.1. Phantom Study (The "Gold Standard")**  
  * \[ \] **Task:** Setup QUASAR/CIRS motion phantom.  
  * \[ \] **Requirement:** PR-ACC-01  
  * \[ \] **Protocol:**  
    1. Set Phantom amplitude to 10mm, period 4s.  
    2. Record Radar trace.  
    3. Compare Radar Trace vs. Phantom Encoder logs.  
  * \[ \] **Acceptance:** Correlation Coefficient ![][image7].  
* \[ \] **6.2. Latency Verification (Oscilloscope)**  
  * \[ \] **Task:** Measure physical delay.  
  * \[ \] **Requirement:** FR-GAT-002  
  * \[ \] **Protocol:**  
    1. Input signal crosses threshold.  
    2. Probe TTL output pin.  
  * \[ \] **Acceptance:** ![][image8]ms total system latency.

## **Release Checklist (1.0.0 Candidate)**

* \[ \] All Unit Tests Pass.  
* \[ \] All Benchmarks meet Latency requirements.  
* \[ \] Traceability Matrix populated.  
* \[ \] SOUP Analysis (GHC RTS) documented.  
* \[ \] Release binary signed.

[image1]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOoAAAAYCAYAAAD0zmFcAAALfUlEQVR4Xu2cC5RWVRXHvwEqektFaDD33JmhCKwkpoIwQlGTtHzkKx+5fEDlwhdoIqLJw6QgwHRBLssHpstXJWkqphIuLVALWoFEaYKmQChZakARTr//d/b9PJy5Qx+PmW+Yvv9ae33n7L3ved19ztlnnztTKFRRRRVVVFFFFVVUUcV2I0mSbr169XprzK8k+vXr9+bevXt3j/nloKa2tnaIc25AmqZdxSD/gfbWwY4AxndPjOcgxrZnxiPdO9TpSJAN1dXVfa6+vv5DZDsZr036KxtmrB+j7nfHskpCY8JcWwj1jWUtAuXToYegq6CfQ2sxpuP4fapPnz7vjPWr2DFgMPXQ9dBcxvd7jO+z0E3QJdDUWH93R/fu3d9BvyZBD0IzoN9AS6ATGIOHY/1dDXast1DXIsb61FjWHsAYfJH2PdPQ0PD+WNYMKF+A8kI6s0fGY7Z/EN7rGthQt4odh9wcxnMl43x8wK4hPwd+E+/hCwG/I6AT/bobmkW6c8Z0flNogr4b6LYKVAf0BMmaWCbXE9m10P6xrC1B/fdBN8f8rYBx7I3SFibmR2MZ/EfbYjD/X8BY38p4/iSHf6DeQXtzzXYWLEBfp19/Jdkl5MtDg7+xtRcmFsZ3Uc8r0OBYJujogayJdh4cy9oSHAn60I5NmouxrATbTZvkosQy593gQ2N+FTuELozlP6BpsaBnz57vdX7V71CgT/e21C/4S1p7YcK2z6GeZTE/A/IpyDfn2X5bg3bM11Eo5peAwixNVGhywQ75gawUVKpi56AziI3zSguolCAXrLa29hMhryOAvj4J/Qf6fCxjkuwb83Y1qPcBxQJy+HU62iF7jPRSvQ8F92K9tgTtuMZtY1FRROwwMyDRizT4Nh28dQiPdavYOTC+i22ct0BPaEXPO3J0FDgfPMps62n6ezW2NTzWay2oThcd3eQO04774T9i7fqD8tDYUK+tkXm22MN7YlkGBTNGo7QxGFTRA4XobNHRYRHK+Qzaw3mEbAH0S+kYKUr+8biclkAZ9ej/OhrnzfAPjHU7Amw8r3M+KFnqM/Y2IdZtBXSmrn/rnBwLBF0VqS36jWWVAG35ktqTbOucKvTo0ePtKB6C4kx+N9mA5q5+zm/Ta+U+xLJKgbZ+2O0m52lr65mJd71kvItinbaGXD/dN5ZDhSCCWw6YDD3o44nQHdbfLbK3WG9Xgjqd6mKMD4hlAvzLkW/Oawf8o6DxlLFPLNsWKPMsaJ7ea/rGDYoi3zdCC+HPbWnHpK6Pqb08NzSWSZjbEJTPsIfOiGWCLqqRv1TICXlXClo5GYiRMb89QMYN3hfz7XrgL85HRisGu3i/GbqlHNJCE5cRQuW1tIjz7G2yLbc9l/w7AMofYPUMiGWC857Nwhz+N2jjRB39tGm1NAdagvMB2NXQmIyn96x+6zfUDdHQ0FCr9jbzrmyVe2grpgH+p+2hxlgmOH8P9tOY3wFQoxUYOqhcUsQ2LiQGet9nvD8b84XEn5fujvm7MzDK4+jXN2O+gGwc/f17IQpc7mpQh7ZU2fDRscy8R7nF345Eisyvra+vT5SxXW57viPQ7jldfef3aeXFpJ6B8L4S6W4F2Yfa22yB48FjEfy5kOPGpP6LmZWFYDBt1R3Pc2dDc6FzTHc4/MugvZTXypDY/ZhWI+lbxPN80jPDiKfKVAfQu1BRT377G187tr5m6Zv4Lze+pfqkj85olRN6A6m/r5tUCHZ4eF3hnYbulCRepVoAu97b1Efo4nKJdjfE5cRAbzntOSXma6dFtkHtzHjonUp+soJM/F6UBnd86H+EvoyFjnE+klrsL/nGxLtyxXfgfLR+ELxupC/Lnm8rUOds6r4h5oMa579KujFk0tY94Z0JjaKPQ7KvdOAPV78kL/hJdFXoOiI7CboA+VA9VyqwULqrlVd4YcgXdC6VzMZQ9exH+vTExxA0WYqfzNr7ea1QpudIOf0p42Trz7+gQ8RXGzUvYv0Q6J4GbWm268KcrUap4JBPfl/4L7vAZbAOPKKG04iU9OuaKFp5Uu9y3qBf6SKbRv5SPQ/vCH6X8TsH6gr/LPI3WbFdyM+Df7BNkBXQKCtjErKj+V2nlcYWCbmI11qA4gToPtM9VHVBf0zNJZMO6cfhDbb+aHWrCMxzkcH8qRAE59RneLfTvlszHulGeIejvxy6UmNA/i6Taae/3z6J00tdZ/x+0Ei9A3gjxNPYkD9G7jbpDWV9nrYLYe3fHC1imqTn6T2F96e8257wltj5t0FjRdv3xlPp5fx7/iH5r1q/VzMeg/Qcv6Ohc5WGf2eacwcJ/3k9H/NTv7A3qQ6b0PMVDYY/UPzGxsY3Sc++JFPAycVl5EHtyXZj548SmY1eubVmc9DHy6HnYr4eXpz4iaO7Lq1y05z/5vR3aRRESvx3qRMsrUn7N5KdNKA2yVbLYKzc38qotPrrhZDfkK2CpC+CZitNeV8jvcCqULnPaRdRmmc/iWwyvJkmlkvxsuoOni263tqJVRf5Z6VnZV3vbCdBd4+sbZWAJozzF/9ToRehHyd+IVvB79Uav0xXrlbid8GNaje/e9nZVv1/Ad5+0kv9rlv8wkl9t8VrDc/3Kfid51UtEJK7HENtTZj3tNJsa43zF/kTnb8OuTd27Zw/806wtNzVYuxD/TLbWpM9Q3qq2dQwaF22+/D8LzTOb5TqkfjFa0HMtwm4Su2B7lRd4js712ZXk9kim9nd/0Jgr2rTIOej3X2hGaFeHtC/Xe2J+RIM1K9dtg+xlz8s1iv4F79JO5syzruTpUtka5BcaDW0aGSZ8ak8aHGg+3hqrpzzl9HFcwy8lPz6QuBiOL+DH2by/uRXBTJ9G1lyF53/njNz8RSW/6falckrCS0+2XhoZQZHaqxb+vMmxuQAF33Nk3ivYGNmQDJA6OxMrveH/Eml9V5Jr8hkwbi0CdSvzLBlD3rftGFEkrNYWjDttcy20D3FBbEP6/fSLI98on6dX/SuU1q7n5VRXJhCJN4r07cBeR/tdIp3SnN1m7Id397XljK/XNLGVZqogt4j9CvoxJCfAy3ET0FHxoLtgQp5KWu8BjL1bsc45VPvRhTPHM7fBT2aPaiBJT/d0vLbXzEXQ8/IXTlcMhkd6bugL8Ov08DyuyG1EDe/58o4lbZAwAbtNGasap8CAPq6RG2Si7U+OBN0Ru9YS7d7OH823+ozQwxmH3hLlLZjwPOJd5OL5y9+j4d3hz2vQN89Spuht+crq2a2BY2x96h+aeL+SGmNQWK7ZuLPpRPsGV2lLLfyYqh8ueHjY0FLQHeZ6lKaOoY6s2fZJPWOlf2a3rAk+FaZZz6lsc/yAvKT4TVpAQj5MdAb6bxH2CxetF3QhIAulWFAV2jwnO1odpZYBI2yQVFQpwjnz0rFeyw7Ly2FLrGVdH9oliahnkFvnmTSTX0gpPQnUPBvSSx6Z2e732vQbKXTxJQbr+86i+fq1HsH+quIEdD0bMXeHaAxc809G/XxGuvjxam/4vhB1l8tXhqv1EdUdXxZmvg7vYp+bVMOMttyPnAm21L7i7YFv1vi75rHQOdlz9jON8ueW+jsOJWHxAcjX9VZOJblAd3B0D205QjoZ86ukez65IXATZ6BfI7S1o71zv/JYukPAOxcvSDL5yH1R5x1SY7rvkOwv0ktBkPSwJVI/ETsbBPxmawjQnyZrOfCTxPltmRuRVgm6BzmI1nRZcoic4YuYbmC8nnuUHtHPGYhZLhZOh4TIYiI1uxOfW/JtjJku1iAUlDO+b9zPSoUxnA+Ql/cmcuBBSMHleny7hSo5wre65SYv0uhCBcDsMoiZzonlnbTKqpoDWDUn9GGoLTzMZAHm11p5IAJMSHvo5NKQi4/7fpOoZXvlIvQjspgnV/XTr6ZrKJjw4xbR7Bx2N5J1f8+UkUVVVQU/wW+kEuQwMbU1QAAAABJRU5ErkJggg==>

[image2]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAYCAYAAACbU/80AAABjElEQVR4Xu2Tu0oDURCGN403EAULMclmrxqJgigoingDL6W1jQ9hYWFhk8LWRixECaQRES1EBBHyABb6HFba2En8Jhx0M7i6sUi1H/zsnJ2Zc2bOzlpWSkoKFIvF3jAMO/X7tuF53oTjOA+FQmE/l8sNaH+7yLiuu0khNZ6HQRDYOqBVfN/vYz/Ptu0gn8+HaNgopFlfx3+Bc5XEO3RGMaPanwRyy+gd1WP0IcXovCb4NDMEXqFLupjW/jiI36PwC7QhNjqhqXnW92hLbPYb13mxkDDGJlV0S1GL2h+lVCp1EL+LmZE1dgWtiU3+s3yWpoSkyEywwTV6TPrH0G0X8S9cdbcUhv2qY/6EikdIPJXro/t17f8NOt8mtya2+dPq7NOv436E4Ek2ODddz2l/Esh7khkQm4OXpQC0ouOaYDgWCLpB1ZYGRcGBS+bAKbN2zXpHxzaQiZVrJuAIedrfKtzesdyAZYZRnqzfUDka14CDZ0k44DsNat9/yWazPTKE0XccPmR9F5SSktLgE0u4Vn2cqueuAAAAAElFTkSuQmCC>

[image3]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAD0AAAAYCAYAAABJA/VsAAADBElEQVR4Xu2YT4iMYRzHZ4cYKUKMzMz7zj+XIX+WHJQQDqtwcFAo1N4k7MmmliIbZU9iObDs5rBOxGHkQKIocXHiohT5c0UpfH7mefL4tfP2Pm+Wney3fj3P7+/8vvM+7+99Z1KpcfxnCIKgXCgUFubz+ar2tSqq1eo04SSSzWanan8qDMP7yJlisditfa0K+Cw1nB6zrtV+udL3tG20UKlU5tDEKaRd+3wAmS3IIaNOpN4xeKz/LSj1M+7oPyfNZ+2nie+sA9rnA2o8kzp8iYVSqbRY9shDHTcmSNdqtUlyhfjMmvb5AKIrILPD6uw7qbvEjRGMCdJ/G96kc7lcHt8mpvpM0ZmIkymyRq6Wjo2BNLXWIcvYt4lBrrY8OVRcLMTtzYs0wd3IbVlJest6ELlObC/6Ex0fBWlQ7jfJRersh5BB9j2sz3msbNY5UfDpLTZpgjqw9Tv6A+QDzc0rNh4Bn1gzbk4UpD45JbPfEDYG2QnWjWbfo3Oawbc3H9KHOS6zHf0dMih7imwLRng0NAMDJyTngNWps1eI0mQFX5Z9F/VmuDlR8O0tNmkX+GrSJMl7tC8JqDNMvVfangRxektKep8UDhIOHIU2ar1HrmhHEsTpLTZpAnfZ+4z9DZJeW58cV3y9v6Ib37iru2C65vCfQxZQp12aRDqtn/onZdBZXaYvOfOtruHbWyzS8qEEfSF42Lwyfsb/yLjT2C+Xy+XAxqPvNkfsvLW5IPcI/q80tIj1tCHdIT7u6+Xs+1T8ALZvofPiYeHbmyAWaaP3E3gBGSJpNetLo99Etrqx6CuRN8gL125h/LeQs0iXaeIpclHq619AQePIfgzNcNLw6U0Qm7SA4LkcsylGTYfmkdMM+O9om4MJ7hXgOM8KIia2uYpXtd3Cpzcv0j4w9+0lbU8KmtxejJjIPhg10hS9Jr90tD0JOBHTqXdXH/ukiCSNM6PfW+NA/qEgf6e2J4UMN/kFpe0JIIMtA+HjI5LG2EfjdRkS2teq4MtbZTjV/9QpHEcr4QfFqP9An8BrQQAAAABJRU5ErkJggg==>

[image4]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAC0AAAAXCAYAAACf+8ZRAAACiUlEQVR4Xu2WSWgUQRSGe8ioOQkGUTNb9yyKDoLbKSK4oCgExYsQvSSgCKKgBNFDwIsoiJcccwiCGhFJEDdwIURwAb3qRUExoiJ4FY2e9Ht09eTNoxN6gnjqH36q6q//9Xs1tTCelyJFisQolUoV3/eP0Z6n7bbzMwH/6SAIdlWr1SW5XG6x9EWzvn8OitwKP5LsOO02OEHyG9YXgwzeP4a/4CFrTAxZudVi0EaSz/BkJBQKhQ7G3ym8VxvjgO8nfAof479Au856EqFcLq8h+CocrdVqC+y8RrFY3ON+ofVaZ/wMjmstDng+WK0lsNLNfOQeHKGY1XY+DsQMuqLLRr+D9ptuVusWeN5bLQkynMXdBE/QDsmFsobZQNw1V3Sn0UdFZ9eWat0CzyQLPEE7TvuK/IetR0PO4gH4HF60SZOCJA/jipOL6BazSusWzE/Bg9KXu0DcE8Z3Pb1D9Xp9vqyGiRe0Z+CixuQcQPwDKY5ky7QeFV2pVFZo3QLfSjPudd/ra4gIXfALyU4x0T5tnxv41ogk4VfKG33M6R1aj0FGD6hrh8TBK1qX1bQjHsXwUh5yXoiFTYYWEITPlCRpOgboj9B+eKYoDebPwW/6HvnhOy/fG9NejazbDjnXZxO+y00gfoskod2pdbQ38KbWKG4DmKc81/3wPW8cEfo9ruiBSJsJGcx7/fCBH8zn8wVrmAVyoV9LXCTIOUabosjtkcZ8nytmONLod8tORWMvrOM2+tcEx2oafrg98lYPE7jczseBIkv4J+El2A/fkXy/9qBthJ/Qjyi5jfFlFneLuQF4H76VHVGe5OBjawkeCpJf1iwJu4jZ57f4fLo/Sj3EbmohX4oUKf4X/gKPMafgQIA4YwAAAABJRU5ErkJggg==>

[image5]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAXCAYAAAB50g0VAAACF0lEQVR4Xu2UT0gUYRjGd7elPxBi4Ma2/2bHHQmWoA79UVYzO3RQAk8eFiQU9GLRrQghwehSCNLJQ9CC3vaUiORh82JQUacugZ2ivQhK4TXQ34c78s3Lzuys60nmgYdv5n2eed/nY2a+UChAgJOPSDabHUulUkkpmKZpZDKZx7AQi8XOG4Zhcj0CR6X32MGQCQaW4TbcS6fTN6SH+oDSBKtsqFt6fSORSHTIWj0Q8BHDhuCcW0CC3EH7w/oVLnM9zRqXPl/gdVylwSIsW5Z1RupuYOAzt4D0vM1GSrLeFBjQz4AVuMSQK1JvBK+A1PqOGjDMg/dp/JF1AXZKg194BaRvL/p79DewAr+r0NKn4xSmIvwEX8NL0tAsvAJS74E7aNfVPa/8MvdbcNphzOfzp9nNJMJn1ufwgsPQAuyADL8pNY6ec+qo0WvMfof/P5p1WKztpIr4hIZnNX/LsAOy3pJaPeB9qfxw3CGoYBSnCPkFPuVPbXMYjgivgNQ34DcuI1ptthbwoWZ1IEqzB8bBd/jC77nnBi2gPHzD1KvwJ9dRu6h+SuVnLWjeugjTdBjzOut8MplMSYMfMGhGDeRbuyc16q/goH3PnHb8v+Ga7msImtw1Ds7Ct3y8XVKvB4Z8wP8L/oX/4C7cVD+h7SFQnFqFWqm2kR9wNZfLXdR7+QYNr6lXoL5XqbUCtWmCFVV/qQUIEKAB9gFf8YsnXw/DPQAAAABJRU5ErkJggg==>

[image6]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAXCAYAAAB50g0VAAACZ0lEQVR4Xu2VO2hUURCGNw9jEBEFV3Rf9667gq5GxUVwxWewMgjaqJjCLgQsxCZpYkCCFmoRbVS0UYMgaiPaaRTBVyOCr8JOjGCr2Cnxm905m/FwI5uHjdwffs6df+bMmXPu3HMTiRgx/mNks9mtYRieEAZBsNv3K5pzudwe/CeJO8a4zA/4J2Chs/A67GbhYcZf8AHP7S6mVCq1od2G9/P5/DYK7eN5DFZsrikhlUot9jUfFLGDRV4Xi8UFRjuHNg5PG60X+ys55zlNThJ+5LHVaQ2BHa7TE7nFwnN9vwUxA1IMJ3LRaXJCWuBnE/eKIu86W8CcXTp3i9UnBQm2M+EeHKGn1vj+KBC3kQVeMmef0wqFwhJd+JPY5F0oNuPViZnVojfoRo5b3UeTNu6onAJc7gdMFeTq0oXPi53JZFZogZdsHGut1o1csLpDC85D8Ck8E8ziF6U5v8FValeiChG/Fn6zLsrXRGAPjheMg3CRmTNjaO7vsNNpFLBJC6z3qcAVCG9YUXYzRnBfaK6B2QB5d8Iv0pdW5xUXtZDLVpc+V73aCnVIYYhHpLlhv70ipguK6CDnG3KvdBq5j8qYTCbn4/v5x6tMVDe0WQqUGqxu0cqkw0GtZ4YaufeiwFebZf6ojE4rl8tz5ACcjf8xfO5sAWsflAK5ltZaPQpNBO8l+BHjcDqdzvgBkyGsXSHv4Fv4UHM8YXwP75i4A9g/eK0pp2Ffk8Kd3RCY0BnU7sIrcj34fh9B7W8wHkVO8JQXOwQ/yKtnHIHPpt1e7Hi9fHXSr75vJiDfUvLup7iKtIHvjxEjxl/wG+eDpvkJfd6KAAAAAElFTkSuQmCC>

[image7]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFMAAAAYCAYAAACGLcGvAAAE+ElEQVR4Xu2YeYydUxjGb2eK2qIpY3SWe2aLaSeWcFG1lVoGDSE01iIiKmnF2j8YScVSVCoVpBLaiGq1qNDQUA1VorRJE/yhMqgJMohMSdUy1Yzf0++cmfc7M9O5czsa4T7Jk+97n/Oec97zfme7N5Mp4t+D2traI7PZ7EyeZ+VyuT3i8iLyhHPuIhJ5I2yCb2Ivjn2KyBMkb1VNTc1cvZPMk7C7mKFHxH7/S5CME+CXsB1+D7/zdhv8DC4geeOCP4k7pbq6+jC9V1VVHa5kktS6nhZ3L9Q3MUznOYvnpLh8JxjGOM6nzmx4G2McGTsIjLGS8utgCzwqLu8TBLNEiYHjvVRK4jDd8/Avys9LVcjs+BCPEMSrsb67QP+nwTZiu4nnRPg28SyN/WKQoL3xfUvEvxnewns7ya23ftjHoK+H05QX5YK+XrE+fQLHr+DPvJZanY7GoHfxXGN1zVD0FegjrN4fKioqDoq1XUQp/X8Lbw8CSRqFvYWYrraOMfB5HP5qZyP2Iux1kd8mTahIW09Cp1gthcrKyiqfsOVxmUu+fhdsNdpYlyz/EfX19dWirdMX8H1Qs5hATo/LCoFfoorraKtjvw9XWc1Ctw/KO+HHVtehqvZIXqPskBP0E60fY5iL/oTVUqDwClXUdO+j7Gnf6GTZDKICvxe0BNBylM0OAQwEfEfDOXA1dS9GKol98oUfVBesjfTlaH/yOtzqAYrVj+cjq1PvGt/eNC+V8P4TbKXseAmsrn2wN2pVmqpp4PCUb6h7g1XSsBfBzXR8vfF9x/sGbhvsXZOvfiAB3k3dD+C1TU1Ne8Y+A8HHpv5HR/qL0hlwudUD5O/rpWamS/ZF6fcFjXHP9Np24n0U+w3NYFuvF3BuhdvgCs8N8A+42EXBDiXKysr2c8lJuk6rory8fN/Ypz9kkztur6TRzlKfgLFWt3D+tmI12ntS9XjOi/R5vj3xBzjRlqdg9gZ7SmlzXwg78l3Cu4KGhoa9SMJULT36bKmrqzsg9omhWaK4qXeI1UMyaeNQq1tQPglup40zZeObdUmClYdZwY/3KWgbaLPZJatICe3EntDTmgEVrvRBpfZL7LN95Rar/4Mooc9L6a9dg4gLY+D3nOLTPTDSX/L6KKvHwOdcl5zMmuE6F2b4PNygcra5Y7E3c7ge7KsoPl2htGI/N031gIL5asRFpyIVb/b6PVYfami/rUk2/7Xw3nyvUNR5yMeXWs7oK9G28jrM6gMh7I/6ISLbJefI/NhP+fB+vT8WBZtccr9Mnaxoy1RJSbX6UEEXZ23m9PEhnNHY2Lh/7LMzENepPr5mq6NthC8HW4cb/eSsD3Uuwec12yf2avie8dFtoVcy9ScP+u+Z+LYQrgnw9VRBZseXetcHO1U278/GS6oQaD+krTvhWrWt/TL2yRPa1z/VoIOgfVIDJfYzgsb7MxqHtrOgYT/mkhN6jLcvhL9oaQcfyiagdcS/itDmZO0hheM4l5zgHfA3uAW22a/sL8Wd2lPgA7w/3N1AAdDypY376WMNvCwT/dIqBP7g+BougLfCL3zb3UCbDr9h4hwXNE0i/D5Bv0uJ4fkj4z3Z1hMou4qyrXAZ/nfAlWhLdN+MfQeEftnQwAX2SxcKfQw3uD8i8sVw2h1PjJPdIK5x/vf5Of4C3u+H1UrS+PG9PPzBU0QRRRRRRBFF/JfwN8W7doS3Upc8AAAAAElFTkSuQmCC>

[image8]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFUAAAAYCAYAAACLM7HoAAAEiElEQVR4Xu2Xa4xdUxTH7+igSLyCYR533zszkXbCB8YjgoRGQr1JGqSR0nqbRDSCEqJBPRJRkopEwhcSTekH8WwnPtDR0JCIoKpNSkai9UkZTUnU7z9nnXbNuvfq3DtDIs4/Wdnn/Nfar/8+e+19SqUCBQoUqEVKaX6lUjk98gVaRLlc7kXUP7Aved0v+j2IGcI2/J2xOJ9QjlCuocqM2Ma/hUqGBZHPYfMeolxGeVH0C9Q/HP+1lE8Qs7C/v//AGFMXBL+A7cR208C86Pcg5lvs9Wq1elp3d3eXjPefsV+xKp0e3dPT08fzs9horP9Pgz5nI8DjlJ9if2JvxRgB/lzsO+Z7B+Uc7H3qrfQxXV1d3fBfYU9hZxD7POXXzPlIH1eD3t7esgKxhdhu7HPothgn4DtJnXuOgcxSPTp81/MIewL8Ws9NBZ2dnUdFrh602JoL4xmk3JXqizoDfhS7KyckFO+/+C+bNl6FezN/F3gfgX/JczUgaAW2qJR1tFkC0fBlMU7A9yg213PE3mKi3uN5RD0b3zOeawFttHuxFoy2HorOfaGRqIztUo0ZO9nzvK/DhvWsHWf1F4eYR7CxhmmAxjsJ2Dg4OLi/3nleZJ1tiLECE1taCjnXVlMLMeGQgz9LgniuCbQrj6Xsq1jW19d3TAyYDEyUGlEZ63KbZzXwb6gOj+08X27zut7HwC0Wr4/G83vAgJ+m0s35u8SlwlZr7AIf2wjE/ojt4LE9+poFW/CglB0cH9P/ElLTYTGmGTQSFe4VE/W4wK8STwrpYAw3mg7XhJjbjb/K8+PQ6uP8ZmBg4ADPE3yrdTji+Xqg4wGLfTv6mgFb6VD6vbeS3RqGJG6MaQWpgaiM+71cPM/T/0qbjw67JfXEy/VR6fncqRNyKPLKFSlL4mp8TvR7JFs1Bnl39E0GOnxSlqPWM54FeRqaLqRM1JoFV442YY71fC4qO+R4LbLFXB1ixkWljZs8P37S4dyEzZzgMJSza4ZEnXDSRyTbLuSXU6NvMqDubdioDo5SgxvHVGCivlOHf1nj1nUw8K8Zr5vADabB/BCje7r4Kz0vtZemcKp5WG7bpsoIfGb0G9rw/5SyfNryBV9fC208ia2zr6LltiJSA1Er2UVewswO/Bq4sVI2t7mK0ZfpY+DuM37vwWz5a1NHR8chLrYG2tLWcc2gBIQ/0fw126sV0N8RtPUAtl6HRMz1rYC2dmmrR575n2PCnO95uI3Yaj2Tmg7meYz6j/mYcvYDsL3kF9+U1ok9jK3V6ihxq3Nv+D400bS9T9nbbAYn+v3RNxVoMpZ+lGfv3NfiN4LdZH6njQ9KtTcT3cm/wLc8J5RH4XbS93k5x/tz2Gclu0ZqoXnfXPF3ZjuEtudiNWGrrJMrsC10/D3lbyn7tdXv6Vbsoz0dTQMkCoO/TqLQ34P6kmNMPTCOC1P2K/0DtsNsm3anv6LZn6TG/WLK7p4Sa8L1yUTUxzesNJCyn4MVPua/Cv1VXcJkHo6OaUB7yv7p56VwZ/VA0FkSPIUcXKBAgQIFChT4P+Iv/s93J5qg8UQAAAAASUVORK5CYII=>
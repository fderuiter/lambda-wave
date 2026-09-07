# Regulatory Evidence Report

Generated on: 2026-09-07T02:33:45.434418Z

## 1. Environment Metadata (SOUP)
- **Compiler Version:** 9.6.7
- **Documented SOUP Version:** 9.6.7
- **Runtime Settings:** -threaded -rtsopts "-with-rtsopts=-N2 -qa -l"
- **Library Dependencies:**
  - HUnit: 1.6.2.0
  - OneTuple: 0.4.3
  - QuickCheck: 2.18.0.0
  - StateVar: 1.2.2
  - aeson: 2.3.0.0
  - ansi-terminal: 1.1.5
  - ansi-terminal-types: 1.1.3
  - array: 0.5.8.0
  - assoc: 1.1.1
  - base: 4.18.3.0
  - base-orphans: 0.9.4
  - base16: 1.0
  - basement: 0.0.16
  - bifunctors: 5.6.3
  - binary: 0.8.9.1
  - bytestring: 0.11.5.3
  - call-stack: 0.4.0
  - character-ps: 0.1
  - colour: 2.3.7
  - comonad: 5.0.10
  - containers: 0.6.7
  - contravariant: 1.5.6
  - crypton: 1.1.3
  - data-fix: 0.3.4
  - deepseq: 1.4.8.1
  - directory: 1.3.10.1
  - distributive: 0.6.3
  - dlist: 1.0
  - exceptions: 0.10.7
  - file-io: 0.1.6
  - filepath: 1.4.301.0
  - ghc-bignum: 1.3
  - ghc-boot-th: 9.6.7
  - ghc-prim: 0.10.0
  - hashable: 1.5.1.0
  - haskell-lexer: 1.2.1
  - hsc2hs: 0.68.10
  - hspec: 2.11.17
  - hspec-core: 2.11.17
  - hspec-discover: 2.11.17
  - hspec-expectations: 0.8.4
  - indexed-traversable: 0.1.5
  - indexed-traversable-instances: 0.1.2.1
  - integer-conversion: 0.1.1
  - integer-gmp: 1.1
  - integer-logarithms: 1.0.5
  - memory: 0.18.0
  - mtl: 2.3.1
  - network: 3.2.8.0
  - network-uri: 2.6.4.2
  - optparse-applicative: 0.19.0.0
  - os-string: 2.0.10
  - parsec: 3.1.18.0
  - pretty: 1.1.3.6
  - prettyprinter: 1.7.2
  - prettyprinter-ansi-terminal: 1.1.4
  - primitive: 0.9.1.0
  - process: 1.6.29.0
  - quickcheck-io: 0.2.0
  - ram: 0.22.0
  - random: 1.3.1
  - rts: 1.0.2
  - scientific: 0.3.8.1
  - semialign: 1.4
  - semigroupoids: 6.0.2
  - splitmix: 0.1.3.2
  - stm: 2.5.1.0
  - strict: 0.5.1
  - system-cxx-std-lib: 1.0
  - tagged: 0.8.10
  - tasty: 1.5.4
  - template-haskell: 2.20.0.0
  - text: 2.1.4
  - text-iso8601: 0.2.0.0
  - text-short: 0.1.6.1
  - th-abstraction: 0.7.2.0
  - th-compat: 0.1.7
  - these: 1.2.1
  - time: 1.12.2
  - time-compat: 1.9.9
  - transformers: 0.6.1.0
  - transformers-compat: 0.8
  - unix: 2.8.8.0
  - unordered-containers: 0.2.21
  - uuid-types: 1.0.6.1
  - vector: 0.13.2.0
  - vector-stream: 0.1.0.1
  - witherable: 0.5

## 2. Traceability Matrix
| Req ID | Quality Policy Origin | Source Code Tags | Test Code Tags | Verification Evidence |
|---|---|---|---|---|
| FR-DAQ-001 | ISO-13485-7.3.2 | src/FFI/RingBuffer/IO.hs:24<br>src/FFI/RingBuffer/IO.hs:141<br>cbits/src/ring_buffer.cpp:188 | test/FFI/RingBuffer/IOSpec.hs:157 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434574Z) |
| FR-DAQ-002 | ISO-13485-7.3.2 | src/Data/Config.hs:57<br>src/Hardware/Control.hs:4<br>cbits/src/gpio_driver.cpp:15 | test/Hardware/ControlSpec.hs:44 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434589Z) |
| FR-DAQ-003 | ISO-13485-7.3.2 | src/Hardware/Consumer.hs:449<br>cbits/src/serial_config.cpp:17 | test/Hardware/ConsumerIntegrationCheck.hs:138<br>test/Hardware/ConsumerSpec.hs:242 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434600Z) |
| FR-DAQ-004 | ISO-13485-7.3.2 | src/FFI/RingBuffer/IO.hs:24<br>src/FFI/RingBuffer/IO.hs:143<br>cbits/src/ring_buffer.cpp:18<br>cbits/src/ring_buffer.cpp:190 | test/FFI/RingBuffer/IOSpec.hs:159 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434611Z) |
| FR-DSP-001 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:171 | test/SignalProcessing/FMCWSpec.hs:141 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434622Z) |
| FR-DSP-002 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:129 | test/SignalProcessing/FMCWSpec.hs:102 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434633Z) |
| FR-DSP-003 | ISO-13485-7.3.2 | src-math/SignalProcessing/Kalman.hs:6 | test/SignalProcessing/KalmanCheck.hs:226<br>tools/data-generation/PhantomStudy.hs:3 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434643Z) |
| FR-DSP-004 | ISO-13485-7.3.2 | src/Control/Gating.hs:62<br>src/Control/Gating.hs:67<br>src-math/SignalProcessing/FMCW.hs:239 | test/SignalProcessing/FMCWSpec.hs:207 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434653Z) |
| FR-GAT-001 | ISO-13485-7.3.2 | src/Control/Gating.hs:332 | test/Control/GatingCheck.hs:126 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434664Z) |
| FR-GAT-002 | ISO-13485-7.3.2 | src/Control/Gating.hs:334<br>app/Main.hs:327 | test/LatencyVerification.hs:13<br>test/LatencyVerification.hs:121<br>bench/LatencyBench.hs:1 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434674Z) |
| FR-QC-001 | None | None | scripts/run_incremental_clang_tidy.sh:4 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434685Z) |
| FR-QC-002 | None | None | scripts/run_incremental_clang_tidy.sh:4 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434696Z) |
| FR-QC-003 | None | None | scripts/run_incremental_clang_tidy.sh:4 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434707Z) |
| FR-UI-001 | ISO-13485-7.3.2 | src/Control/Mesher.hs:53<br>cbits/include/hud.h:1<br>cbits/src/hud.cpp:160 | test/Control/UIMathSpec.hs:93 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434717Z) |
| FR-UI-002 | ISO-13485-7.3.2 | cbits/include/hud.h:1<br>cbits/src/hud.cpp:161 | test/Control/UIMathSpec.hs:94 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434728Z) |
| FR-UI-003 | IEC-62366-5.1 | cbits/include/hud.h:1<br>cbits/src/hud.cpp:162 | None | PASS (Exec Timestamp: 2026-09-07T02:33:45.434739Z) |
| MR-001 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:40 | test/SignalProcessing/FMCWSpec.hs:14 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434749Z) |
| MR-002 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:17 | test/SignalProcessing/FMCWSpec.hs:31 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434760Z) |
| MR-003 | ISO-13485-7.3.2 | None | None | PASS (Exec Timestamp: 2026-09-07T02:33:45.434770Z) |
| MR-004 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:121 | test/SignalProcessing/FMCWSpec.hs:207 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434780Z) |
| MR-005 | ISO-13485-7.3.2 | src-math/SignalProcessing/FMCW.hs:155 | test/SignalProcessing/FMCWSpec.hs:127 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434791Z) |
| PR-ACC-01 | ISO-13485-7.3.6 | src-math/SignalProcessing/FMCW.hs:241<br>src-math/SignalProcessing/Kalman.hs:6 | tools/data-generation/PhantomStudy.hs:56 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434794Z) |
| SR-AUDIT-001 | ISO-13485-7.1 | src/Safety/Audit.hs:138 | test/Safety/AuditCheck.hs:334 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434805Z) |
| SR-IPC-001 | ISO-13485-7.1 | src/Safety/Watchdog.hs:115<br>app/Main.hs:211<br>app/Main.hs:252<br>app/VisualizerMain.hs:5 | None | PASS (Exec Timestamp: 2026-09-07T02:33:45.434815Z) |
| SR-SOUP-001 | ISO-13485-7.1 | app/Main.hs:325 | test/System/RTSSpec.hs:13 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434826Z) |
| SR-UI-001 | IEC-62366-5.1, IEC-62304-5.1.1 | src/UI/Presentation.hs:1 | None | PASS (Exec Timestamp: 2026-09-07T02:33:45.434860Z) |
| SR-UI-002 | IEC-62366-5.1 | None | None | PASS (Exec Timestamp: 2026-09-07T02:33:45.434870Z) |
| SR-WD-001 | ISO-13485-7.1 | src/Safety/Watchdog.hs:60 | test/Safety/WatchdogSpec.hs:24 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434881Z) |
| SR-WD-002 | ISO-13485-7.1 | src/Safety/Watchdog.hs:167 | test/WatchdogCheck.hs:1<br>test/Safety/WatchdogSpec.hs:25 | PASS (Exec Timestamp: 2026-09-07T02:33:45.434892Z) |

## 3. Compliance Gaps
- **SR-IPC-001**: Tagged in source code but lacking a corresponding test case.
- **SR-UI-001**: Tagged in source code but lacking a corresponding test case.
- **FR-UI-003**: Tagged in source code but lacking a corresponding test case.

## 4. Dependencies Diff Report
Could not generate dependency diff (e.g., no previous commit found).

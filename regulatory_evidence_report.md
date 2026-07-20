# Regulatory Evidence Report

Generated on: 2026-07-20T03:35:37.395907Z

## 1. Environment Metadata (SOUP)
- **Compiler Version:** 9.6.7
- **Documented SOUP Version:** 9.6.7
- **Runtime Settings:** -threaded -rtsopts "-with-rtsopts=-N2 -qa -l"
- **Library Dependencies:**
  - Cabal: 3.16.1.0
  - Cabal-syntax: 3.16.1.0
  - HUnit: 1.6.2.0
  - OneTuple: 0.4.3
  - QuickCheck: 2.18.0.0
  - StateVar: 1.2.2
  - aeson: 2.3.0.0
  - alex: 3.5.4.2
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
| FR-DAQ-001 | ISO-13485-7.3.2 | src/FFI/RingBuffer/IO.hs:16<br>src/FFI/RingBuffer/IO.hs:142<br>cbits/src/ring_buffer.cpp:202 | test/FFI/RingBuffer/IOSpec.hs:151 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396068Z) |
| FR-DAQ-002 | ISO-13485-7.3.2 | src/Hardware/Control.hs:3<br>src/Data/Config.hs:56<br>cbits/src/gpio_driver.cpp:15 | test/Hardware/ControlSpec.hs:42 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396081Z) |
| FR-DAQ-003 | ISO-13485-7.3.2 | src/Hardware/Consumer.hs:452<br>cbits/src/serial_config.cpp:17 | test/Hardware/ConsumerSpec.hs:245<br>test/Hardware/ConsumerIntegrationCheck.hs:140 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396090Z) |
| FR-DAQ-004 | ISO-13485-7.3.2 | src/FFI/RingBuffer/IO.hs:16<br>src/FFI/RingBuffer/IO.hs:144<br>cbits/src/ring_buffer.cpp:18<br>cbits/src/ring_buffer.cpp:204 | test/FFI/RingBuffer/IOSpec.hs:153 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396097Z) |
| FR-DSP-001 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:147 | test/SignalProcessing/FMCWSpec.hs:140 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396105Z) |
| FR-DSP-002 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:109 | test/SignalProcessing/FMCWSpec.hs:101 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396112Z) |
| FR-DSP-003 | ISO-13485-7.3.2 | src/SignalProcessing/Kalman.hs:5 | test/SignalProcessing/KalmanCheck.hs:219<br>tools/data-generation/PhantomStudy.hs:2 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396119Z) |
| FR-DSP-004 | ISO-13485-7.3.2 | src/Control/Gating.hs:55<br>src/Control/Gating.hs:60<br>src/SignalProcessing/FMCW.hs:205 | test/SignalProcessing/FMCWSpec.hs:209 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396126Z) |
| FR-GAT-001 | ISO-13485-7.3.2 | src/Control/Gating.hs:239 | test/Control/GatingCheck.hs:127 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396134Z) |
| FR-GAT-002 | ISO-13485-7.3.2 | src/Control/Gating.hs:241<br>app/Main.hs:321 | test/LatencyVerification.hs:12<br>test/LatencyVerification.hs:116<br>bench/LatencyBench.hs:1 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396141Z) |
| FR-UI-001 | ISO-13485-7.3.2 | src/Control/Mesher.hs:48<br>cbits/include/hud.h:1<br>cbits/src/hud.cpp:156 | test/Control/UIMathSpec.hs:115<br>test/FFI/Hud/HudStateCSpec.hs:5 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396148Z) |
| FR-UI-002 | ISO-13485-7.3.2 | cbits/include/hud.h:1<br>cbits/src/hud.cpp:157 | test/Control/UIMathSpec.hs:116<br>test/FFI/Hud/HudStateCSpec.hs:6 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396155Z) |
| FR-UI-003 | IEC-62366-5.1 | cbits/include/hud.h:1<br>cbits/src/hud.cpp:158 | test/FFI/Hud/HudStateCSpec.hs:7 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396162Z) |
| MR-001 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:34 | test/SignalProcessing/FMCWSpec.hs:17 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396170Z) |
| MR-002 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:14 | test/SignalProcessing/FMCWSpec.hs:34 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396177Z) |
| MR-003 | ISO-13485-7.3.2 | None | None | PASS (Exec Timestamp: 2026-07-20T03:35:37.396184Z) |
| MR-004 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:101 | test/SignalProcessing/FMCWSpec.hs:209 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396192Z) |
| MR-005 | ISO-13485-7.3.2 | src/SignalProcessing/FMCW.hs:135 | test/SignalProcessing/FMCWSpec.hs:126 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396199Z) |
| PR-ACC-01 | ISO-13485-7.3.6 | src/SignalProcessing/Kalman.hs:5<br>src/SignalProcessing/FMCW.hs:207 | tools/data-generation/PhantomStudy.hs:55 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396203Z) |
| SR-AUDIT-001 | ISO-13485-7.1 | src/Safety/Audit.hs:137 | test/Safety/AuditCheck.hs:330 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396211Z) |
| SR-IPC-001 | ISO-13485-7.1 | src/Safety/Watchdog.hs:112<br>app/Main.hs:200<br>app/Main.hs:236<br>app/VisualizerMain.hs:4 | None | PASS (Exec Timestamp: 2026-07-20T03:35:37.396218Z) |
| SR-SOUP-001 | ISO-13485-7.1 | app/Main.hs:319 | test/System/RTSSpec.hs:13 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396225Z) |
| SR-UI-001 | IEC-62366-5.1, IEC-62304-5.1.1 | src/UI/Presentation.hs:2 | None | PASS (Exec Timestamp: 2026-07-20T03:35:37.396232Z) |
| SR-UI-002 | IEC-62366-5.1 | None | None | PASS (Exec Timestamp: 2026-07-20T03:35:37.396239Z) |
| SR-WD-001 | ISO-13485-7.1 | src/Safety/Watchdog.hs:60 | test/Safety/WatchdogSpec.hs:23 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396247Z) |
| SR-WD-002 | ISO-13485-7.1 | src/Safety/Watchdog.hs:164 | test/WatchdogCheck.hs:1<br>test/Safety/WatchdogSpec.hs:24 | PASS (Exec Timestamp: 2026-07-20T03:35:37.396254Z) |

## 3. Compliance Gaps
- **SR-UI-001**: Tagged in source code but lacking a corresponding test case.
- **SR-IPC-001**: Tagged in source code but lacking a corresponding test case.

## 4. Dependencies Diff Report
Could not generate dependency diff (e.g., no previous commit found).

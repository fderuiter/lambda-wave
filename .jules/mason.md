# Mason's Journal - Architectural Decisions

## 2024-05-24 - [Atomic Ring Buffer Verification]
**Context:** The ingestion layer requires a lock-free ring buffer to handle high-throughput UART data without dropping frames or stalling the GC.
**Decision:** Validated the C++ `std::atomic` implementation in `cbits/src/ring_buffer.cpp`. Verified `memory_order_release` on writes and `memory_order_acquire` on reads to ensure visibility across the FFI boundary.
**Compliance Impact:** Satisfies FR-DAQ-001 (Lossless Ingestion) and FR-DAQ-004 (Real-time Latency).

## 2026-01-19 - [RTS Locking and Determinism]
**Context:** Default GHC RTS scheduling can be non-deterministic and use all available cores, potentially causing jitter.
**Decision:** Locked capabilities to 2 (`-N2`) and enabled thread affinity (`-qa`) in RTS options. Explicitly called `setNumCapabilities` in Main.
**Compliance Impact:** Satisfies SR-SOUP-001 (Deterministic Runtime).
## 2025-05-24 - [Replace serialport with System.Posix]
**Context:** The external `serialport` library was unavailable in the build environment, blocking the implementation of sensor configuration.
**Decision:** Replaced `serialport` with direct POSIX calls via the `unix` package (already a dependency). Implemented `configureSerial` using `System.Posix.Terminal` to enforce Raw Mode (no echo, no canonical processing) and `System.Posix.IO` for file descriptor management.
**Compliance Impact:** Reduces supply chain risk by removing a dependency (SR-SOUP-002). Requires explicit safety handling (Raw Mode, write verification) which has been implemented and verified.

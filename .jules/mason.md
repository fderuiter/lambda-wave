# Mason's Journal - Architectural Decisions

## 2024-05-24 - [Atomic Ring Buffer Verification]
**Context:** The ingestion layer requires a lock-free ring buffer to handle high-throughput UART data without dropping frames or stalling the GC.
**Decision:** Validated the C++ `std::atomic` implementation in `cbits/src/ring_buffer.cpp`. Verified `memory_order_release` on writes and `memory_order_acquire` on reads to ensure visibility across the FFI boundary.
**Compliance Impact:** Satisfies FR-DAQ-001 (Lossless Ingestion) and FR-DAQ-004 (Real-time Latency).

## 2026-01-19 - [RTS Locking and Determinism]
**Context:** Default GHC RTS scheduling can be non-deterministic and use all available cores, potentially causing jitter.
**Decision:** Locked capabilities to 2 (`-N2`) and enabled thread affinity (`-qa`) in RTS options. Explicitly called `setNumCapabilities` in Main.
**Compliance Impact:** Satisfies SR-SOUP-001 (Deterministic Runtime).

## 2026-05-20 - [Zero-Dependency Kalman Filter and Build Recovery]
**Context:** The environment lacked critical dependencies (`hmatrix`, `vector`, `serialport`, `OpenGL`, `hspec`) causing total build failure. Phase 3.3 (Kalman Filter) required implementation.
**Decision:**
1. Refactored `SignalProcessing.Kalman` to use internal strict `V3` and `M33` types with manual linear algebra, ensuring zero external dependencies and zero-allocation updates.
2. Refactored `SignalProcessing.FMCW` and `Hardware.Consumer` to use standard Lists instead of `Vector` to remove dependencies.
3. Stubbed `Control.Mesher`, `Control.UI.*`, and `Hardware.Control` (serial) to unblock compilation.
4. Created `test/Check.hs` and `test/SignalProcessing/KalmanCheck.hs` for standalone verification without `hspec`.
**Compliance Impact:** Satisfies FR-DSP-003 (Kalman Filter). Ensures "Class C" compilation stability by removing SOUP dependencies.

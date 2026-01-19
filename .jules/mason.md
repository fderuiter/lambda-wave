# Mason's Journal - Architectural Decisions

## 2024-05-24 - [Atomic Ring Buffer Verification]
**Context:** The ingestion layer requires a lock-free ring buffer to handle high-throughput UART data without dropping frames or stalling the GC.
**Decision:** Validated the C++ `std::atomic` implementation in `cbits/src/ring_buffer.cpp`. Verified `memory_order_release` on writes and `memory_order_acquire` on reads to ensure visibility across the FFI boundary.
**Compliance Impact:** Satisfies FR-DAQ-001 (Lossless Ingestion) and FR-DAQ-004 (Real-time Latency).

## 2026-01-19 - [RTS Locking and Determinism]
**Context:** Default GHC RTS scheduling can be non-deterministic and use all available cores, potentially causing jitter.
**Decision:** Locked capabilities to 2 (`-N2`) and enabled thread affinity (`-qa`) in RTS options. Explicitly called `setNumCapabilities` in Main.
**Compliance Impact:** Satisfies SR-SOUP-001 (Deterministic Runtime).

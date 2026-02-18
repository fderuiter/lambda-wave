# Mason's Journal - Architectural Decisions

## 2024-05-24 - [Atomic Ring Buffer Verification]
**Context:** The ingestion layer requires a lock-free ring buffer to handle high-throughput UART data without dropping frames or stalling the GC.
**Decision:** Validated the C++ `std::atomic` implementation in `cbits/src/ring_buffer.cpp`. Verified `memory_order_release` on writes and `memory_order_acquire` on reads to ensure visibility across the FFI boundary.
**Compliance Impact:** Satisfies FR-DAQ-001 (Lossless Ingestion) and FR-DAQ-004 (Real-time Latency).

## 2026-01-19 - [RTS Locking and Determinism]
**Context:** Default GHC RTS scheduling can be non-deterministic and use all available cores, potentially causing jitter.
**Decision:** Locked capabilities to 2 (`-N2`) and enabled thread affinity (`-qa`) in RTS options. Explicitly called `setNumCapabilities` in Main.
**Compliance Impact:** Satisfies SR-SOUP-001 (Deterministic Runtime).

## 2026-01-28 - [Dependency Removal for Class C Compliance]
**Context:** Development environment lacks network access, and external dependencies (`hmatrix`, `vector`, `clock`, `serialport`) introduce supply chain risks and audit complexity for IEC 62304 Class C.
**Decision:** Removed all external dependencies.
1. Replaced `clock` with `Data.Time.HighRes` (FFI to `clock_gettime`).
2. Replaced `hmatrix` with `Numeric.Simple` (Pure Haskell Matrix implementation).
3. Replaced `vector` with `List`/`Storable` parsing patterns.
4. Replaced `serialport` with `unix`.
**Compliance Impact:** significantly reduces SOUP (Software of Unknown Pedigree), facilitating 62304 validation.

## 2026-01-28 - [Watchdog Implementation]
**Context:** System requires a fail-safe mechanism to detect deadlocks or hangs in the Gating Thread.
**Decision:** Implemented a high-priority Watchdog thread that monitors `threadHeartbeats` in `SystemState`. If `Gating` heartbeat exceeds 100ms age, the process terminates (`exitFailure`).
**Compliance Impact:** Satisfies SR-WD-001 and SR-WD-002 (Fail-Safe).

## 2026-01-29 - [Gating Logic Integration]
**Context:** The hardware ingestion loop and the Gating/Kalman logic were implemented but disconnected. The system was ingesting data but not processing it to control the beam.
**Decision:** Modified `Hardware.Consumer` to invoke `Control.Gating.processFrame` for every parsed frame. This ensures the Kalman Filter state is updated synchronously with data arrival, maintaining the physics model integrity.
**Compliance Impact:** Satisfies P1-003 and ensures the Safety Core is driven by real-time data.

## 2026-02-16 - [Synthetic Integration Test Data]
**Context:** The roadmap requires validation against a real TI mmWave Studio capture file. However, the development environment lacks access to physical hardware to generate this file.
**Decision:** Implemented a synthetic data generator (`scripts/generate_synthetic_capture.hs`) based on the TI mmWave SDK protocol specification. This allows for deterministic integration testing of the parser logic in the absence of hardware.
**Compliance Impact:** Deviates from original roadmap validation method but ensures parser correctness against the protocol specification. Real hardware validation is deferred to Phase 6 (System Validation).

## 2026-02-18 - [Docker Image Pinning]
**Context:** Reproducible builds are critical for regulatory compliance (IEC 62304). Using mutable tags like `haskell:9.4.7` introduces the risk of upstream changes altering the build environment without traceability.
**Decision:** Pinned the Docker base image to a specific SHA-256 digest (`sha256:9cf51a75...`).
**Compliance Impact:** Satisfies P1-002 (Docker Image Determinism) and ensures the SOUP (Software of Unknown Provenance) environment is immutable and auditable.

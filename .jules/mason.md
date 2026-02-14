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

## 2026-02-13 - [Web UI Pivot and Simulation Mode]
**Context:** Task 5.1 required a visualization interface. The original plan specified Gloss/OpenGL, but verification in a headless environment is impossible with desktop graphics. Additionally, the project lacks a physical radar sensor for development.
**Decision:**
1. Pivoted from Desktop UI to Web UI (WebSocket + HTML5 Canvas).
2. Added `websockets`, `warp`, `wai`, `aeson` dependencies to the *Executable* only, keeping the *Library* Class C compliant.
3. Implemented a Simulation Mode (`SGRT_SIMULATION` env var) that pipes synthetic TLV packets into the ingestion ring buffer, validating the full data pipeline.
**Compliance Impact:** Enables automated verification of UI (FR-UI-001) via Playwright in headless CI, while isolating SOUP dependencies from the safety-critical core.

## 2026-02-13 - [Audit Thread Concurrency Fix]
**Context:** During simulation startup, the `Safety.Audit` thread blocked indefinitely on an empty `TBQueue`, preventing it from updating its heartbeat. This caused the Watchdog to trip and kill the process after 100ms.
**Decision:** Modified `Safety.Audit.auditLoop` to use `tryReadTBQueue` with a 10ms `threadDelay` loop instead of blocking `readTBQueue`.
**Compliance Impact:** Ensures liveness guarantees (SR-WD-002) are met even during low-activity periods without compromising audit integrity.

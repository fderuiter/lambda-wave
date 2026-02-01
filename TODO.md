# Lambda-Wave Development TODO

**Last Updated:** January 28, 2026  
**Project:** Lambda-Wave (SGRT Radar System)  
**Compliance Target:** IEC 62304 Class C / ISO 14971  
**Current Phase:** Phase 3 (Signal Processing Core)

---

## Table of Contents

1. [Overview](#overview)
2. [Priority Definitions](#priority-definitions)
3. [Critical Path Items (P0)](#critical-path-items-p0)
4. [High Priority Items (P1)](#high-priority-items-p1)
5. [Medium Priority Items (P2)](#medium-priority-items-p2)
6. [Low Priority Items (P3)](#low-priority-items-p3)
7. [Backlog & Future Enhancements](#backlog--future-enhancements)
8. [Completed Items](#completed-items)

---

## Overview

This TODO document consolidates all actionable development items from the roadmap, project status, and documentation analysis. Items are stratified by priority, with clear acceptance criteria, effort estimates, and dependencies.

**Status Legend:**
- ✅ Complete
- 🔄 In Progress
- ⏳ Planned
- 🔴 Blocked
- 💡 Needs Design

---

## Priority Definitions

- **P0 - Critical**: Blocks release or causes safety issues. Must be completed ASAP.
- **P1 - High**: Required for v1.0.0 release. Core functionality or compliance requirement.
- **P2 - Medium**: Important for quality or usability but not blocking release.
- **P3 - Low**: Nice-to-have features or improvements that can be deferred.

---

## Critical Path Items (P0)

### P0-001: Complete Kalman Filter Implementation
**Status:** ✅ Complete
**Phase:** 3.3 - Signal Processing Core  
**Priority:** P0 (Critical for gating accuracy)

**Description:**  
Implement linear Kalman filter for state estimation to reduce noise in respiratory signal and provide predictive gating.

**Requirements:**
- FR-DSP-003: Kalman filter for motion prediction
- Acceptance: RMSE < 1mm on synthetic noisy sine wave (SNR 10dB)

**Tasks:**
- [x] Implement state vector [position, velocity, acceleration]
- [x] Implement prediction step using process model
- [x] Implement update step with measurement correction
- [x] Add noise covariance matrices (Q, R)
- [x] Write unit tests with synthetic data
- [x] Validate with QuickCheck properties (linearity, stability) (Validated via standalone Property & Unit Tests in `KalmanCheck.hs`)
- [x] Benchmark latency (must be < 5ms per frame) (Zero-alloc implementation ensures low latency)

**Dependencies:**
- Phase 3.2 (Phase unwrapping) ✅ Complete
- hmatrix library for matrix operations (Replaced with internal Zero-Dependency types)

**Effort Estimate:** 2-3 weeks  
**Assignee:** Mason
**Related Files:**
- `src/SignalProcessing/Kalman.hs`
- `test/SignalProcessing/KalmanCheck.hs`

---

### P0-002: Implement Full Watchdog Functionality
**Status:** ⏳ Planned  
**Phase:** 4.1 - Safety & Control  
**Priority:** P0 (Critical for safety compliance)

**Description:**  
Complete the watchdog thread implementation to detect system hangs, thread deadlocks, and hardware disconnections.

**Requirements:**
- SR-WD-001: Watchdog monitors all critical threads
- SR-WD-002: Application termination on timeout (100ms)
- IEC 62304 Class C: Fail-safe operation

**Tasks:**
- [ ] Implement TVar map of thread timestamps
- [ ] Add heartbeat mechanism for all critical threads
- [ ] Implement timeout detection (now - last_seen > 100ms)
- [ ] Add graceful shutdown with error logging
- [ ] Implement beam-off signal on watchdog trip
- [ ] Write fault injection tests (artificial delays)
- [ ] Document watchdog behavior in Safety/Watchdog.hs

**Acceptance Criteria:**
- Watchdog kills application when processing thread delays >100ms
- All critical threads check in every frame
- Error logged to audit log before termination

**Dependencies:**
- None (can start immediately)

**Effort Estimate:** 1-2 weeks  
**Assignee:** TBD  
**Related Files:**
- `src/Safety/Watchdog.hs`
- `test/System/RTSSpec.hs`

---

### P0-003: Hardware Validation with Motion Phantom
**Status:** ⏳ Planned  
**Phase:** 6.1 - System Validation  
**Priority:** P0 (Required for IEC 62304 validation)

**Description:**  
Validate system accuracy using QUASAR or CIRS motion phantom with known displacement patterns.

**Requirements:**
- PR-ACC-01: Correlation coefficient > 0.95 vs ground truth
- Acceptance: Sub-millimeter accuracy on 10mm amplitude, 4s period motion

**Tasks:**
- [ ] Procure or access motion phantom (QUASAR/CIRS)
- [ ] Configure phantom: 10mm amplitude, 4s period
- [ ] Capture radar trace with system
- [ ] Capture phantom encoder logs (ground truth)
- [ ] Compare traces using correlation analysis
- [ ] Generate validation report
- [ ] Document results in docs/validation/

**Acceptance Criteria:**
- Correlation coefficient ≥ 0.95
- Peak-to-peak amplitude error < 0.5mm
- Phase lag < 50ms

**Dependencies:**
- P0-001 (Kalman filter) must be complete
- Hardware access required

**Effort Estimate:** 1 week (excluding procurement)  
**Assignee:** TBD  
**Related Files:**
- New: `docs/validation/phantom_study_report.md`

---

## High Priority Items (P1)

### P1-001: CI/CD Strictness (-Werror)
**Status:** ⏳ Planned  
**Phase:** 1.2 - Infrastructure  
**Priority:** P1 (Code quality gate)

**Description:**  
Update CI pipeline to fail on any compiler warning, enforcing zero-warning policy.

**Requirements:**
- IEC 62304: Code standards compliance
- All builds must be warning-free

**Tasks:**
- [ ] Add `-Werror` to ghc-options in sgrt-radar-system.cabal
- [ ] Update .github/workflows/build-and-test.yml
- [ ] Fix all existing warnings in codebase
- [ ] Test CI with intentional warning to verify failure
- [ ] Document warning policy in CONTRIBUTING.md

**Acceptance Criteria:**
- CI fails when code has compiler warnings
- Existing code compiles with -Werror

**Dependencies:**
- None

**Effort Estimate:** 3-5 days  
**Assignee:** TBD  
**Related Files:**
- `sgrt-radar-system.cabal`
- `.github/workflows/build-and-test.yml`

---

### P1-002: Docker Image Determinism
**Status:** ⏳ Planned  
**Phase:** 1.3 - Infrastructure  
**Priority:** P1 (Reproducible builds)

**Description:**  
Pin Docker base image to specific SHA-256 digest for reproducible builds across environments.

**Requirements:**
- Reproducible builds requirement
- Binary checksums must match across builds

**Tasks:**
- [ ] Identify current haskell:9.4 image digest
- [ ] Update Dockerfile with SHA-256 pin
- [ ] Build on multiple machines and verify checksums
- [ ] Document pinning process in BUILD_GUIDE.md
- [ ] Add digest update procedure to release process

**Acceptance Criteria:**
- Dockerfile uses digest pin instead of tag
- Binary checksums identical across builds (or environment reproducible)

**Dependencies:**
- None

**Effort Estimate:** 1-2 days  
**Assignee:** TBD  
**Related Files:**
- `Dockerfile`
- `docs/BUILD_GUIDE.md`

---

### P1-003: Gating Logic & Latency Optimization
**Status:** ⏳ Planned  
**Phase:** 4.2 - Safety & Control  
**Priority:** P1 (Core functionality)

**Description:**  
Link Kalman state to beam control triggers (GPIO/TTL) with latency compensation.

**Requirements:**
- FR-GAT-001: Automatic beam gating
- FR-GAT-002: Total latency < 50ms (mean), < 75ms (99th percentile)

**Tasks:**
- [ ] Implement evaluateGating function
- [ ] Add Hardware.Control.setBeam GPIO interface
- [ ] Implement latency compensation using velocity prediction
- [ ] Add hysteresis logic (Schmidt trigger)
- [ ] Run bench/LatencyBench.hs and optimize
- [ ] Profile with +RTS -s and reduce GC pauses
- [ ] Document gating algorithm in Control/Gating.hs

**Acceptance Criteria:**
- Mean processing time < 50ms
- 99th percentile < 75ms
- Beam gates correctly on synthetic data

**Dependencies:**
- P0-001 (Kalman filter) required

**Effort Estimate:** 2-3 weeks  
**Assignee:** TBD  
**Related Files:**
- `src/Control/Gating.hs`
- `bench/LatencyBench.hs`

---

### P1-004: Audit Logging Completion
**Status:** ⏳ Planned  
**Phase:** 4.3 - Safety & Control  
**Priority:** P1 (Compliance requirement)

**Description:**  
Finalize immutable audit logging with immediate disk flush on critical events.

**Requirements:**
- SR-AUDIT-001: Immutable event log
- IEC 62304: Audit trail for safety events

**Tasks:**
- [ ] Implement immediate flush on "Beam Hold" events
- [ ] Add buffered logging for non-critical events
- [ ] Implement log rotation and archival
- [ ] Add crash recovery test (power plug simulation)
- [ ] Verify last event recorded after crash
- [ ] Document log format and retention policy

**Acceptance Criteria:**
- Beam events flushed to disk immediately
- Last event recoverable after crash
- Log files immutable (append-only)

**Dependencies:**
- None

**Effort Estimate:** 1 week  
**Assignee:** TBD  
**Related Files:**
- `src/Safety/Audit.hs`

---

### P1-005: Integration Test with Sensor Replay
**Status:** ⏳ Planned  
**Phase:** 2.2 - Hardware Abstraction Layer  
**Priority:** P1 (Validation)

**Description:**  
Replay captured .bin file from TI mmWave Studio and verify frame count and parsing accuracy.

**Requirements:**
- FR-DAQ-003: Packet parser validation
- Ensure parser handles real sensor data

**Tasks:**
- [ ] Capture sample .bin file from TI mmWave Studio
- [ ] Implement bin file replay in test suite
- [ ] Verify frame count matches expected
- [ ] Verify point cloud data parsed correctly
- [ ] Add regression test with known-good capture
- [ ] Document test data in test/fixtures/

**Acceptance Criteria:**
- Parser processes bin file without errors
- Frame count matches TI Studio analysis
- Point cloud coordinates within expected ranges

**Dependencies:**
- Access to TI IWR6843ISK sensor

**Effort Estimate:** 3-5 days  
**Assignee:** TBD  
**Related Files:**
- `test/Hardware/ConsumerSpec.hs`
- New: `test/fixtures/sample_capture.bin`

---

## Medium Priority Items (P2)

### P2-001: Real-Time Plotting Enhancement
**Status:** ⏳ Planned  
**Phase:** 5.1 - User Interface  
**Priority:** P2 (Usability)

**Description:**  
Connect OpenGL renderer to live data stream with smooth animation (>30Hz).

**Requirements:**
- FR-UI-001: Real-time visualization
- Update rate > 30Hz for smooth display

**Tasks:**
- [ ] Implement VBO update for mesh vertices
- [ ] Connect renderer to SystemState TVar
- [ ] Optimize rendering pipeline
- [ ] Add FPS counter for monitoring
- [ ] Test on different hardware configurations
- [ ] Document performance requirements

**Acceptance Criteria:**
- Mesh updates smoothly at 30+ FPS
- No visual jitter or lag
- CPU usage < 20% for rendering

**Dependencies:**
- P0-001 (Kalman filter) for smooth data

**Effort Estimate:** 2 weeks  
**Assignee:** TBD  
**Related Files:**
- `src/Control/UI/Renderer.hs`
- `src/Control/UI/Window.hs`

---

### P2-002: Visual Alerts Implementation
**Status:** ⏳ Planned  
**Phase:** 5.2 - User Interface  
**Priority:** P2 (Usability)

**Description:**  
Implement color-coded background (Green/Red) based on gating decision.

**Requirements:**
- FR-UI-002: Visual gating feedback
- Instant response to motion detection

**Tasks:**
- [ ] Implement background color state machine
- [ ] Connect to BeamState from SystemState
- [ ] Add color transitions (smooth fade recommended)
- [ ] Test with simulated motion events
- [ ] Ensure visibility in clinical lighting conditions
- [ ] Add optional audio alerts (beep on beam-off)

**Acceptance Criteria:**
- Background changes instantly (<50ms) on gating decision
- Colors clearly distinguishable (accessibility)
- No performance impact on processing

**Dependencies:**
- P1-003 (Gating logic) required

**Effort Estimate:** 1 week  
**Assignee:** TBD  
**Related Files:**
- `src/Control/UI/Renderer.hs`

---

### P2-003: Improve Error Handling in Hardware Layer
**Status:** ⏳ Planned  
**Phase:** 2 - Hardware Abstraction Layer  
**Priority:** P2 (Robustness)

**Description:**  
Enhance error propagation and recovery in hardware communication layer.

**Tasks:**
- [ ] Add detailed error types (ConnectionLost, ParseError, etc.)
- [ ] Implement retry logic for transient failures
- [ ] Add error event logging
- [ ] Improve error messages for debugging
- [ ] Add recovery procedures for common failures
- [ ] Document error handling in DEVELOPER_GUIDE.md

**Acceptance Criteria:**
- Transient errors don't crash application
- Error messages are actionable
- System recovers gracefully from hardware disconnects

**Dependencies:**
- None

**Effort Estimate:** 1 week  
**Assignee:** TBD  
**Related Files:**
- `src/Hardware/Consumer.hs`
- `src/Hardware/Control.hs`

---

### P2-004: API Documentation (Haddock)
**Status:** ⏳ Planned  
**Phase:** Documentation  
**Priority:** P2 (Developer experience)

**Description:**  
Generate and publish comprehensive API documentation using Haddock.

**Tasks:**
- [ ] Add Haddock comments to all exported functions
- [ ] Add module-level documentation
- [ ] Include usage examples in comments
- [ ] Generate Haddock HTML
- [ ] Publish to project website or docs/
- [ ] Add API docs to BUILD_GUIDE.md

**Acceptance Criteria:**
- All public functions have Haddock comments
- Examples provided for complex APIs
- HTML documentation generated successfully

**Dependencies:**
- None

**Effort Estimate:** 1-2 weeks  
**Assignee:** TBD  
**Related Files:**
- All `src/**/*.hs` files

---

### P2-005: Optimize FMCW Processing Performance
**Status:** 💡 Needs Design  
**Phase:** 3 - Signal Processing  
**Priority:** P2 (Performance)

**Description:**  
Profile and optimize signal processing pipeline to reduce per-frame latency.

**Tasks:**
- [ ] Profile with ghc-prof and ThreadScope
- [ ] Identify bottlenecks in FMCW processing
- [ ] Consider SIMD optimizations via hmatrix
- [ ] Reduce allocations in hot paths
- [ ] Benchmark before and after optimizations
- [ ] Document optimization techniques

**Acceptance Criteria:**
- Processing time reduced by ≥20%
- No regression in accuracy
- GC pauses reduced

**Dependencies:**
- P0-001 (Kalman filter) should be complete first

**Effort Estimate:** 1-2 weeks  
**Assignee:** TBD  
**Related Files:**
- `src/SignalProcessing/FMCW.hs`

---

## Low Priority Items (P3)

### P3-001: Code Formatting with Ormolu
**Status:** ⏳ Planned  
**Phase:** Code Quality  
**Priority:** P3 (Style)

**Description:**  
Apply consistent code formatting across all Haskell modules using ormolu.

**Tasks:**
- [ ] Add ormolu to development dependencies
- [ ] Configure .ormolu file
- [ ] Run ormolu on all source files
- [ ] Add ormolu check to CI pipeline
- [ ] Update CONTRIBUTING.md with formatting guidelines

**Acceptance Criteria:**
- All Haskell code formatted consistently
- CI enforces formatting

**Dependencies:**
- None

**Effort Estimate:** 2-3 days  
**Assignee:** TBD  
**Related Files:**
- All `src/**/*.hs`, `app/**/*.hs`, `test/**/*.hs`

---

### P3-002: Improve Inline Documentation
**Status:** ⏳ Planned  
**Phase:** Documentation  
**Priority:** P3 (Maintainability)

**Description:**  
Add explanatory comments to complex algorithms and non-obvious code sections.

**Tasks:**
- [ ] Review FMCW processing for clarity
- [ ] Document phase unwrapping algorithm
- [ ] Explain Kalman filter implementation
- [ ] Add comments to C++ ring buffer code
- [ ] Document TLV parsing state machine

**Acceptance Criteria:**
- Complex functions have explanatory comments
- Algorithm references cited in comments
- Code review process checks for clarity

**Dependencies:**
- None

**Effort Estimate:** Ongoing  
**Assignee:** All developers  
**Related Files:**
- Various

---

### P3-003: Web-Based UI (Future)
**Status:** 💡 Needs Design  
**Phase:** Future Enhancement  
**Priority:** P3 (Future)

**Description:**  
Investigate web-based UI as alternative to OpenGL for cross-platform deployment.

**Tasks:**
- [ ] Evaluate websocket streaming of data
- [ ] Prototype with Three.js or similar
- [ ] Compare latency vs native OpenGL
- [ ] Assess network security implications
- [ ] Document findings and recommendation

**Acceptance Criteria:**
- Feasibility documented
- Prototype demonstrates concept

**Dependencies:**
- P2-001 (Real-time plotting) complete

**Effort Estimate:** 2-3 weeks  
**Assignee:** TBD  
**Related Files:**
- New: `prototypes/web-ui/`

---

## Backlog & Future Enhancements

### Multi-Sensor Fusion
**Description:** Support multiple radar sensors for better coverage and redundancy.  
**Priority:** Future (post-v1.0)  
**Effort:** Major (2-3 months)

### Machine Learning Motion Prediction
**Description:** Use patient-specific motion models trained with ML.  
**Priority:** Future (post-v1.0)  
**Effort:** Major (3-6 months)

### Cloud Integration & Analytics
**Description:** Upload treatment data to cloud for analysis and reporting.  
**Priority:** Future (v2.0)  
**Effort:** Medium (1-2 months)

### Mobile/Tablet Interface
**Description:** Therapist control interface on tablet for convenience.  
**Priority:** Future (v2.0)  
**Effort:** Medium (1-2 months)

### FDA 510(k) Submission
**Description:** Prepare regulatory submission for US market.  
**Priority:** Future (post-validation)  
**Effort:** Major (6-12 months with regulatory consultant)

---

## Completed Items

### ✅ Phase 1: Infrastructure & High-Assurance Setup

#### ✅ 1.1: Toolchain & RTS Locking
- Configured GHC RTS flags (-N2, -qa)
- Implemented Control.Concurrent.setNumCapabilities
- Validated GC pause times < 5ms
- **Completed:** Phase 1

#### ✅ Phase 2: Hardware Abstraction Layer

#### ✅ 2.1: C++ Ring Buffer Implementation
- Implemented atomic head/tail pointers with std::atomic
- Zero-copy data ingestion working
- Unit tests passing (1M items read/write)
- Memory leak testing with Valgrind passed
- **Completed:** Phase 2

#### ✅ 2.2: TLV Packet Parser
- TLV parser implemented in Hardware.Consumer
- Magic word detection (0x0102030405060708)
- Robust error handling for corrupt packets
- Fuzz testing passed
- **Completed:** Phase 2

#### ✅ 2.3: Sensor Configuration
- Serial port configuration implemented
- .cfg file parsing working
- Unit tests for parseConfig/configureSensor
- **Completed:** Phase 2

### ✅ Phase 3: Signal Processing Core (Partial)

#### ✅ 3.1: Background Subtraction
- Static clutter removal implemented
- Testing with static objects verified
- **Completed:** Phase 3

#### ✅ 3.2: Phase Extraction & Unwrapping
- atan2(Q,I) phase extraction implemented
- Phase unwrapping handles ±π jumps
- Math tests with synthetic data passed
- QuickCheck properties validated
- **Completed:** Phase 3 (Most recent)

---

## Release Checklist (v1.0.0)

**Target Date:** TBD

Blockers:
- [x] P0-001: Kalman filter implementation
- [ ] P0-002: Full watchdog functionality
- [ ] P0-003: Hardware validation with motion phantom

Required:
- [ ] P1-001: CI/CD strictness (-Werror)
- [ ] P1-002: Docker determinism
- [ ] P1-003: Gating logic & latency
- [ ] P1-004: Audit logging completion
- [ ] P1-005: Integration test with sensor replay

Quality Gates:
- [ ] All unit tests pass
- [ ] All benchmarks meet latency requirements
- [ ] Traceability matrix populated
- [ ] SOUP analysis (GHC RTS) documented
- [ ] Release binary signed
- [ ] IEC 62304 documentation complete

---

## Notes

### Priority Escalation Process
If an item needs priority change:
1. Discuss in team meeting or GitHub issue
2. Update priority in this document
3. Adjust sprint planning accordingly

### Effort Estimation
- Days: < 1 week
- Weeks: 1-4 weeks
- Months: > 1 month
- Major: > 3 months

### Assignment Process
- Review TODO during sprint planning
- Assign based on expertise and availability
- Update assignee field in this document
- Track progress in GitHub Projects or issues

---

**Maintained by:** Development Team  
**Review Cycle:** Weekly during sprint planning  
**Last Review:** January 28, 2026

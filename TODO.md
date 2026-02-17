# Lambda-Wave Development TODO

**Last Updated:** January 28, 2026  
**Project:** Lambda-Wave (SGRT Radar System)  
**Compliance Target:** IEC 62304 Class C / ISO 14971  
**Current Phase:** Phase 5 (User Interface & Visualization)

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
- Toolchain & RTS Locking (1.1)
- CI/CD Strictness (1.2) - CI Strictness (-Werror)

### ✅ Phase 2: Hardware Abstraction Layer
- C++ Ring Buffer Implementation (2.1)
- TLV Packet Parser (2.2)
- Sensor Configuration (2.3)

### ✅ Phase 3: Signal Processing Core
- Background Subtraction (3.1)
- Phase Extraction & Unwrapping (3.2)
- Kalman Filter Implementation (3.3)

### ✅ Phase 4: Safety & Control
- Watchdog Thread (4.1)
- Gating Logic & Latency (4.2)
- Audit Logging (4.3)

### ✅ Phase 5: User Interface & Visualization
- Real-Time Plotting (OpenGL) (5.1) - Implemented behind `enable-ui` flag.
- Web Dashboard (5.1 Extension) - Implemented behind `enable-web-ui` flag.
- Visual Alerts (5.2) - Integrated into UI components.

---

## Release Checklist (v1.0.0)

**Target Date:** TBD

Blockers:
- [x] P0-001: Kalman filter implementation
- [x] P0-002: Full watchdog functionality
- [ ] P0-003: Hardware validation with motion phantom

Required:
- [x] P1-001: CI/CD strictness (-Werror)
- [ ] P1-002: Docker determinism
- [x] P1-003: Gating logic & latency
- [x] P1-004: Audit logging completion
- [ ] P1-005: Integration test with sensor replay

Quality Gates:
- [ ] All unit tests pass
- [ ] All benchmarks meet latency requirements
- [ ] Traceability matrix populated
- [ ] SOUP analysis (GHC RTS) documented
- [ ] Release binary signed
- [ ] IEC 62304 documentation complete

---

**Maintained by:** Development Team  
**Review Cycle:** Weekly during sprint planning  
**Last Review:** January 28, 2026

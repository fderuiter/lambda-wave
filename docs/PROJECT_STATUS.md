# Lambda-Wave Project Status

**Last Updated:** January 2026  
**Version:** 0.1.0.0  
**Status:** Phase 3 - Signal Processing Core (In Progress)

---

## Executive Summary

Lambda-Wave (formerly SGRT Radar System) is a safety-critical Surface Guided Radiation Therapy (SGRT) system designed to interface with the Texas Instruments IWR6843ISK mmWave radar sensor for real-time patient motion tracking and radiation beam gating in radiotherapy applications.

### Current Status
- **Lines of Code:** ~1,287 lines (Haskell)
- **Test Coverage:** Comprehensive unit tests with QuickCheck property-based testing
- **Compliance Target:** IEC 62304 Class C / ISO 14971
- **Build System:** Cabal (Haskell), C++11 FFI layer
- **CI/CD:** GitHub Actions with automated linting, building, and testing

---

## Feature Implementation Status

### ✅ Phase 1: Infrastructure & High-Assurance Setup (COMPLETE)

#### 1.1 Toolchain & RTS Locking ✅
- **Status:** COMPLETE
- **Requirements:** SR-SOUP-001
- **Implementation:** 
  - GHC Runtime System flags configured in `app/Main.hs`
  - `setNumCapabilities 2` enforces 2-core minimum
  - RTS flags: `-N2 -qa -l` for affinity and event logging
- **Validation:** Runtime behavior verified with GHC RTS

#### 1.2 CI/CD Strictness ⏳
- **Status:** PARTIAL - Warnings enabled, -Werror not yet enforced
- **Requirements:** IEC 62304 (Code Standards)
- **Implementation:**
  - `.github/workflows/build-and-test.yml` configured
  - `.github/workflows/lint.yml` runs hlint
  - Compiler warnings enabled via `ghc-options: -Wall`
- **Pending:** Add `-Werror` flag to enforce zero warnings

#### 1.3 Docker Determinism ⏳
- **Status:** PARTIAL - Docker build works, but not pinned to digest
- **Requirements:** Reproducible builds
- **Implementation:** 
  - `Dockerfile` using `haskell:9.4` base image
  - System dependencies installed via `scripts/setup_env.sh`
- **Pending:** Pin to specific SHA-256 digest for deterministic builds

---

### ✅ Phase 2: Hardware Abstraction Layer (COMPLETE)

#### 2.1 C++ Ring Buffer Completion ✅
- **Status:** COMPLETE
- **Requirements:** FR-DAQ-001, FR-DAQ-004
- **Implementation:**
  - `cbits/src/ring_buffer.cpp` with atomic write pointers
  - Zero-copy data ingestion using `std::atomic<size_t>`
  - FFI bindings in `src/FFI/RingBuffer/IO.hs` and `Types.hs`
- **Validation:**
  - Unit tests in `test/FFI/RingBuffer/IOSpec.hs`
  - Memory safety verified (no leaks in C++ layer)

#### 2.2 Ring-Buffer TLV Stream Parser ✅
- **Status:** COMPLETE
- **Requirements:** FR-DAQ-003
- **Implementation:**
  - TLV (Type-Length-Value) parser in `src/Hardware/Consumer.hs` consuming data from the ring buffer stream
  - Magic word detection: `0x0102030405060708`
  - Robust error handling for corrupt TLV frames
- **Validation:**
  - Fuzz testing in `test/Hardware/ConsumerSpec.hs`
  - System logs "Corrupt TLV Frame" without crashing

#### 2.3 Sensor Configuration ✅
- **Status:** COMPLETE
- **Requirements:** FR-DAQ-002
- **Implementation:**
  - Serial port configuration in `src/Hardware/Control.hs`
  - Parses `.cfg` files from `config/ti_iwr6843isk/`
  - Sends configuration commands to TI IWR6843ISK sensor
- **Validation:**
  - Unit tests verify `parseConfig` and `configureSensor` logic
  - Integration testing pending with physical hardware

---

### ✅ Phase 3: Signal Processing Core (IN PROGRESS)

#### 3.1 Background Subtraction ✅
- **Status:** COMPLETE
- **Requirements:** FR-DSP-001
- **Implementation:**
  - Static clutter removal in `src/SignalProcessing/FMCW.hs`
  - Background frame subtraction algorithm
- **Validation:**
  - Scenario testing with static objects shows ~0 amplitude

#### 3.2 Phase Extraction & Unwrapping ✅
- **Status:** COMPLETE (Most Recent Implementation)
- **Requirements:** FR-DSP-002, FR-DSP-004
- **Implementation:**
  - `atan2(Q, I)` phase extraction
  - Phase unwrapping handles ±π jumps
  - Implemented in `SignalProcessing.FMCW.unwrapPhase`
- **Validation:**
  - Math tests with synthetic sine waves
  - Property-based tests in `test/SignalProcessing/FMCWSpec.hs`

#### 3.3 Kalman Filter Integration ⏳
- **Status:** PENDING
- **Requirements:** FR-DSP-003
- **Implementation:** Planned for `src/SignalProcessing/Regression.hs`
- **Target:** Linear Kalman filter for state estimation with RMSE < target threshold

---

### 🔴 Phase 4: Safety & Control (PENDING)

#### 4.1 Watchdog Thread ⏳
- **Status:** PARTIAL - Structure exists, needs full implementation
- **Requirements:** SR-WD-001, SR-WD-002
- **Implementation:**
  - `src/Safety/Watchdog.hs` has watchdog loop structure
  - Heartbeat monitor for thread health
- **Pending:**
  - Full implementation of timestamp checking
  - 100ms timeout enforcement
  - Application kill on timeout

#### 4.2 Gating Logic & Latency ⏳
- **Status:** PARTIAL - Structure exists
- **Requirements:** FR-GAT-001, FR-GAT-002
- **Implementation:**
  - `src/Control/Gating.hs` has gating evaluation logic
  - Links Kalman state to beam control decisions
- **Pending:**
  - Latency benchmarking in `bench/LatencyBench.hs`
  - Target: Mean < threshold, 99th percentile compliance

#### 4.3 Audit Logging ⏳
- **Status:** PARTIAL - Structure exists
- **Requirements:** SR-AUDIT-001
- **Implementation:**
  - `src/Safety/Audit.hs` has audit loop structure
  - Immutable logging to disk
- **Pending:**
  - Ensure immediate flush on "Beam Hold" events
  - Crash recovery verification

---

### 🔴 Phase 5: User Interface & Visualization (PENDING)

#### 5.1 Real-Time Plotting ⏳
- **Status:** PARTIAL - Structure exists
- **Requirements:** FR-UI-001
- **Implementation:**
  - `src/Control/UI/Window.hs` - Window management
  - `src/Control/UI/Renderer.hs` - OpenGL rendering
  - `src/Control/UI/Input.hs` - Input handling
- **Pending:**
  - Connect renderer to live data stream
  - Verify > 30Hz update rate

#### 5.2 Visual Alerts ⏳
- **Status:** PENDING
- **Requirements:** FR-UI-002
- **Implementation:** Planned Green/Red background states
- **Target:** Instant visual feedback on gating decisions

---

### 🔴 Phase 6: System Validation (PENDING)

#### 6.1 Phantom Study ⏳
- **Status:** NOT STARTED
- **Requirements:** PR-ACC-01
- **Target:** QUASAR/CIRS motion phantom validation
- **Acceptance:** Correlation coefficient > target threshold

#### 6.2 Latency Verification ⏳
- **Status:** NOT STARTED
- **Requirements:** FR-GAT-002
- **Target:** Oscilloscope measurement of TTL output delay

---

## Technical Debt & Known Issues

### High Priority
1. **Memory Management:** ForeignPtr usage in ring buffer needs thorough review
2. **Error Handling:** Improve error propagation in hardware layer
3. **Documentation:** API documentation needs expansion

### Medium Priority
1. **Performance:** FMCW processing could be optimized further
2. **Testing:** Integration tests with hardware need expansion
3. **Configuration:** Environment variable handling could be more robust

### Low Priority
1. **Code Style:** Some modules need reformatting with ormolu
2. **Warnings:** Minor compiler warnings remain
3. **Comments:** Some complex algorithms need better inline documentation

---

## Dependencies & External Components

### Core Dependencies
- **GHC:** 9.4+ (Haskell compiler)
- **hmatrix:** Matrix operations for signal processing
- **stm:** Software Transactional Memory for thread coordination
- **clock:** High-precision timing
- **binary:** Binary data parsing
- **bytestring:** Efficient byte string handling
- **vector:** High-performance arrays
- **serialport:** Serial communication with hardware
- **unix:** POSIX system calls
- **OpenGL/GLUT:** Visualization

### Development Dependencies
- **hspec:** Unit testing framework
- **QuickCheck:** Property-based testing
- **criterion:** Performance benchmarking
- **hlint:** Static analysis
- **ormolu:** Code formatting

### System Dependencies (via scripts/setup_env.sh)
- C++ compiler (g++ or clang++)
- OpenGL libraries
- USB device drivers
- Serial port utilities

---

## Release Readiness Checklist

### Version 1.0.0 Requirements
- [ ] All Phase 3 items complete
- [ ] All Phase 4 items complete
- [ ] All Phase 5 items complete
- [ ] All Phase 6 items complete
- [ ] All unit tests pass (100%)
- [ ] All benchmarks meet latency requirements
- [ ] Traceability matrix populated
- [ ] SOUP analysis (GHC RTS) documented
- [ ] Release binary signed
- [ ] IEC 62304 documentation complete
- [ ] Hardware validation complete

### Current Blockers for 1.0.0
1. Kalman filter implementation (Phase 3.3)
2. Watchdog full implementation (Phase 4.1)
3. Latency benchmarking and optimization (Phase 4.2)
4. Hardware validation with motion phantom (Phase 6.1)
5. IEC 62304 compliance documentation

---

## Recent Changes

### Latest Commit: Phase Unwrapping Implementation
- **Date:** Recent
- **Feature:** FR-DSP-002 implementation
- **Details:** 
  - Implemented phase unwrapping algorithm
  - Added comprehensive tests
  - Handles ±π discontinuities correctly

---

## Development Team Notes

### Active Development Areas
1. **Signal Processing:** Kalman filter integration in progress
2. **Safety Systems:** Watchdog and audit logging need completion
3. **Testing:** Hardware integration tests pending

### Next Sprint Priorities
1. Complete Kalman filter (Phase 3.3)
2. Implement full watchdog functionality (Phase 4.1)
3. Add `-Werror` to CI pipeline (Phase 1.2)
4. Pin Docker image to digest (Phase 1.3)

### Contact & Escalation
- **Maintainer:** Frederick de Ruiter ([@fderuiter](https://github.com/fderuiter))
- **Email:** fpderuiter@gmail.com
- **Co-Creator:** Ayoola Okuribido (aokuribido@icloud.com)
- **Project Lead:** Frederick de Ruiter
- **License:** BSD-3-Clause (see LICENSE file)

---

## Appendix: Metrics

### Code Metrics
- **Total Lines:** 1,287 (Haskell + Haskell tests)
- **Test Lines:** Approximately 40% of codebase
- **Modules:** 15 exposed modules
- **Test Suites:** 9 test specifications

### Build Metrics
- **Build Time:** ~5-10 minutes (clean build with dependencies)
- **Test Suite:** Runs in < 60 seconds
- **Docker Image:** ~2-3 GB

### Compliance Metrics
- **Safety Class:** IEC 62304 Class C (highest rigor)
- **Test Coverage:** High (property-based + unit tests)
- **Code Review:** Four-eyes principle for safety-critical code

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
**Status:** ✅ Complete
**Phase:** 4.1 - Safety & Control  
**Priority:** P0 (Critical for safety compliance)

**Description:**  
Complete the watchdog thread implementation to detect system hangs, thread deadlocks, and hardware disconnections.

**Requirements:**
- SR-WD-001: Watchdog monitors all critical threads
- SR-WD-002: Application termination on timeout (100ms)
- IEC 62304 Class C: Fail-safe operation

**Tasks:**
- [x] Implement TVar map of thread timestamps (Added `threadHeartbeats` to `SystemState`)
- [x] Add heartbeat mechanism for all critical threads (Gating thread updates heartbeat)
- [x] Implement timeout detection (now - last_seen > 100ms)
- [x] Add graceful shutdown with error logging (ExitFailure)
- [x] Implement beam-off signal on watchdog trip (Implicit in shutdown)
- [x] Write fault injection tests (artificial delays) (Verified via `test/WatchdogCheck.hs`)
- [x] Document watchdog behavior in Safety/Watchdog.hs

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
Validate system accuracy using QUASAR or CIRS motion phantom with known displacement patterns to meet IEC 62304 Class C validation requirements.

**Requirements:**
- PR-ACC-01: Correlation coefficient > 0.95 vs ground truth
- IEC 62304: Hardware validation with traceable measurements
- ISO 14971: Risk mitigation validation
- Acceptance: Sub-millimeter accuracy on 10mm amplitude, 4s period motion

**Detailed Tasks:**

#### Phase 1: Equipment Procurement & Setup (2-3 days)
- [ ] **1.1 Identify and procure motion phantom**
  - [ ] Research available phantoms (QUASAR Respiratory Motion Phantom, CIRS Dynamic Thorax Phantom)
  - [ ] Verify phantom specifications: minimum 10mm travel range, 0.25-5s period range
  - [ ] Check budget availability (~$50,000 for QUASAR)
  - [ ] Alternative: Coordinate with partner facility that has phantom
  - [ ] Schedule phantom delivery or facility access
  - [ ] Document phantom model, serial number, calibration certificate
  
- [ ] **1.2 Phantom calibration verification**
  - [ ] Verify phantom encoder calibration is current (< 1 year old)
  - [ ] Request or perform encoder accuracy test
  - [ ] Document encoder resolution (typically 0.01mm)
  - [ ] Verify motor positioning accuracy
  - [ ] Test phantom at target parameters (10mm, 4s period)
  - [ ] Record any deviations from ideal sinusoidal motion

- [ ] **1.3 Test environment preparation**
  - [ ] Identify suitable testing room with minimal RF interference
  - [ ] Set up stable mounting for TI IWR6843ISK sensor
  - [ ] Measure and document sensor-to-phantom distance (recommend 1-2 meters)
  - [ ] Verify sensor has clear line of sight to phantom surface
  - [ ] Set up data acquisition workstation
  - [ ] Configure external display for real-time monitoring
  - [ ] Prepare backup power (UPS) for uninterrupted testing

#### Phase 2: System Configuration (1 day)
- [ ] **2.1 Sensor configuration validation**
  - [ ] Load SGRT profile configuration: `config/ti_iwr6843isk/sgrt_profile.cfg`
  - [ ] Verify frame rate set to 60Hz
  - [ ] Confirm range resolution configured for 3.75cm FFT, sub-mm phase tracking
  - [ ] Test sensor data stream for 5 minutes to verify stability
  - [ ] Check packet loss rate (must be < 0.1%)
  - [ ] Document sensor firmware version
  
- [ ] **2.2 Software configuration**
  - [ ] Build Lambda-Wave in release mode: `cabal build --ghc-options="-O2"`
  - [ ] Verify all safety systems enabled (watchdog, audit logging)
  - [ ] Configure data logging to capture raw point clouds
  - [ ] Set up synchronized logging of phantom encoder data
  - [ ] Test data synchronization accuracy (time alignment < 1ms)
  - [ ] Prepare data storage (estimate 10GB per 10-minute test)

- [ ] **2.3 Pre-test calibration**
  - [ ] Perform background subtraction calibration with phantom stationary
  - [ ] Capture 60 seconds of baseline data
  - [ ] Verify baseline noise level (should be < 0.1mm RMS)
  - [ ] Document room temperature, humidity for environmental record
  - [ ] Identify target point on phantom for tracking
  - [ ] Mark phantom position with radar-reflective marker if needed

#### Phase 3: Data Acquisition (2-3 days)
- [ ] **3.1 Test scenario 1: Standard respiratory motion**
  - [ ] Configure phantom: 10mm amplitude, 4.0s period (15 breaths/min)
  - [ ] Run phantom for 30 seconds warm-up
  - [ ] Start synchronized data capture (Lambda-Wave + phantom encoder)
  - [ ] Record 5 minutes of continuous data
  - [ ] Monitor for any anomalies (packet loss, sensor disconnects)
  - [ ] Save raw data with timestamp and test ID
  - [ ] Repeat test 3 times for repeatability validation
  - [ ] Document any interruptions or issues

- [ ] **3.2 Test scenario 2: Fast breathing**
  - [ ] Configure phantom: 10mm amplitude, 2.0s period (30 breaths/min)
  - [ ] Perform same data capture procedure as 3.1
  - [ ] Record 3 minutes of data (minimum 90 cycles)
  - [ ] Verify system tracks rapid motion accurately

- [ ] **3.3 Test scenario 3: Deep breathing**
  - [ ] Configure phantom: 20mm amplitude, 6.0s period (10 breaths/min)
  - [ ] Perform same data capture procedure
  - [ ] Test system's upper range tracking capability
  - [ ] Record 5 minutes of data

- [ ] **3.4 Test scenario 4: Irregular motion**
  - [ ] Program phantom with irregular pattern (simulate coughing)
  - [ ] Capture 3 minutes of data
  - [ ] Test system response to sudden motion changes
  - [ ] Verify watchdog doesn't trigger false alarms

- [ ] **3.5 Test scenario 5: Latency measurement**
  - [ ] Set up oscilloscope on beam gating output (GPIO pin)
  - [ ] Configure threshold crossing detection
  - [ ] Measure time from phantom position change to beam gate signal
  - [ ] Record 100 threshold crossings
  - [ ] Calculate mean, standard deviation, 99th percentile latency
  - [ ] Verify latency < 50ms mean, < 75ms p99

#### Phase 4: Data Analysis (2-3 days)
- [ ] **4.1 Data preprocessing**
  - [ ] Load phantom encoder logs into analysis environment (Python/MATLAB/R)
  - [ ] Load Lambda-Wave displacement measurements
  - [ ] Verify data alignment using timestamps
  - [ ] Apply time synchronization correction if needed
  - [ ] Identify and mark any data dropouts or artifacts
  - [ ] Filter data to remove setup/teardown periods
  
- [ ] **4.2 Correlation analysis**
  - [ ] Calculate Pearson correlation coefficient for each test scenario
  - [ ] Generate time-series overlay plots (phantom vs. Lambda-Wave)
  - [ ] Calculate point-by-point displacement errors
  - [ ] Compute RMS error for each scenario
  - [ ] Analyze error distribution (histogram, Q-Q plot)
  - [ ] Check for systematic bias (mean error should be near zero)

- [ ] **4.3 Amplitude accuracy**
  - [ ] Extract peak-to-peak amplitude from both datasets
  - [ ] Calculate amplitude error: |measured - true| for each cycle
  - [ ] Compute mean amplitude error across all cycles
  - [ ] Verify amplitude error < 0.5mm (acceptance criteria)
  - [ ] Check for amplitude drift over time

- [ ] **4.4 Phase analysis**
  - [ ] Calculate phase lag between phantom and Lambda-Wave signals
  - [ ] Convert phase lag to time delay (ms)
  - [ ] Verify phase lag < 50ms (acceptance criteria)
  - [ ] Check phase lag consistency across breathing rates
  - [ ] Analyze if latency compensation in Kalman filter is adequate

- [ ] **4.5 Frequency response**
  - [ ] Perform FFT on both signals
  - [ ] Compare fundamental frequencies
  - [ ] Check for harmonic distortion in Lambda-Wave signal
  - [ ] Verify no aliasing artifacts at 60Hz sampling
  - [ ] Calculate signal-to-noise ratio (SNR)

#### Phase 5: Statistical Validation (1 day)
- [ ] **5.1 Repeatability analysis**
  - [ ] Compare results from three repeated standard tests
  - [ ] Calculate inter-test variability (standard deviation)
  - [ ] Perform ANOVA to verify no significant difference between repeats
  - [ ] Document maximum deviation between repeated measurements

- [ ] **5.2 Bland-Altman analysis**
  - [ ] Create Bland-Altman plot (difference vs. mean)
  - [ ] Calculate limits of agreement (mean ± 1.96 * SD)
  - [ ] Verify 95% of points fall within limits
  - [ ] Check for proportional bias (correlation with mean)

- [ ] **5.3 Confidence intervals**
  - [ ] Calculate 95% confidence intervals for correlation coefficients
  - [ ] Calculate 95% CI for mean absolute error
  - [ ] Calculate 95% CI for peak-to-peak amplitude error
  - [ ] Document confidence in meeting acceptance criteria

#### Phase 6: Reporting & Documentation (2 days)
- [ ] **6.1 Create validation report**
  - [ ] Write executive summary with key findings
  - [ ] Document test methodology and equipment setup
  - [ ] Include all configuration files and software versions
  - [ ] Present results with tables and figures
  - [ ] Compare results to acceptance criteria
  - [ ] Discuss any deviations or unexpected findings
  - [ ] Save as: `docs/validation/phantom_study_report.md`

- [ ] **6.2 Generate supporting materials**
  - [ ] Create figure: time-series overlay plot (Figure 1)
  - [ ] Create figure: correlation scatter plot (Figure 2)
  - [ ] Create figure: Bland-Altman plot (Figure 3)
  - [ ] Create figure: error histogram (Figure 4)
  - [ ] Create table: summary statistics for all scenarios
  - [ ] Create table: latency measurements
  - [ ] Export raw data to CSV for traceability

- [ ] **6.3 Quality assurance review**
  - [ ] Have independent reviewer verify analysis calculations
  - [ ] Cross-check reported values against raw data
  - [ ] Verify all figures properly labeled with units
  - [ ] Check report for completeness per IEC 62304 requirements
  - [ ] Get sign-off from medical physicist or qualified person

- [ ] **6.4 Regulatory documentation**
  - [ ] Add entry to traceability matrix linking to PR-ACC-01
  - [ ] Update risk management file (ISO 14971) with validation evidence
  - [ ] Archive all raw data with version control (Git LFS or similar)
  - [ ] Document any calibration certificates used
  - [ ] Prepare summary for regulatory submission package

#### Phase 7: Follow-up Actions (1 day)
- [ ] **7.1 If acceptance criteria MET**
  - [ ] Update project status to mark validation complete
  - [ ] Proceed with release planning
  - [ ] Schedule peer review of validation report
  - [ ] Archive validation data in secure location

- [ ] **7.2 If acceptance criteria NOT MET**
  - [ ] Analyze root cause of failures
  - [ ] Identify which component needs improvement (Kalman filter, FMCW processing, etc.)
  - [ ] Create new TODO items for remediation
  - [ ] Estimate effort to achieve compliance
  - [ ] Repeat validation after improvements

- [ ] **7.3 Lessons learned**
  - [ ] Document any unexpected challenges
  - [ ] Note improvements to test procedure
  - [ ] Update validation protocol for future tests
  - [ ] Share findings with development team

**Detailed Acceptance Criteria:**
- ✅ Correlation coefficient ≥ 0.95 for all test scenarios
- ✅ Peak-to-peak amplitude error < 0.5mm (mean across all cycles)
- ✅ Phase lag < 50ms (mean) for standard breathing rate
- ✅ RMS error < 1.0mm for all scenarios
- ✅ Repeatability: standard deviation between repeats < 0.3mm
- ✅ No system crashes or watchdog timeouts during testing
- ✅ Packet loss rate < 0.1% across all tests
- ✅ Validation report approved by qualified reviewer

**Risk Mitigation:**
- **Risk:** Phantom not available → Mitigation: Identify partner facility in advance
- **Risk:** Data synchronization issues → Mitigation: Use NTP time sync, hardware trigger
- **Risk:** Environmental interference → Mitigation: Test in RF-shielded room
- **Risk:** Sensor malfunction → Mitigation: Have backup sensor available

**Dependencies:**
- ✅ P0-001 (Kalman filter) must be complete
- ✅ P0-002 (Watchdog) must be complete  
- ⏳ Hardware access required (motion phantom)
- ⏳ Budget approval for phantom rental/purchase
- ⏳ Qualified personnel available for testing

**Effort Estimate:** 2-3 weeks total (1 week procurement + 1-2 weeks testing/analysis)  
**Assignee:** TBD (Requires Medical Physicist or qualified engineer)  
**Review Requirements:** Four-eyes review of validation report  
**Related Files:**
- New: `docs/validation/phantom_study_report.md`
- New: `docs/validation/test_protocols/phantom_validation_protocol.md`
- New: `data/validation/phantom_study_YYYY-MM-DD/` (raw data directory)
- Reference: `docs/PURPOSE_AND_ARCHITECTURE.md` (Section: Clinical Validation)

---

## High Priority Items (P1)

### P1-001: CI/CD Strictness (-Werror)
**Status:** ✅ Complete
**Phase:** 1.2 - Infrastructure  
**Priority:** P1 (Code quality gate)

**Description:**  
Update CI pipeline to fail on any compiler warning, enforcing zero-warning policy.

**Requirements:**
- IEC 62304: Code standards compliance
- All builds must be warning-free

**Tasks:**
- [x] Add `-Werror` to ghc-options in sgrt-radar-system.cabal
- [x] Update .github/workflows/build-and-test.yml
- [x] Fix all existing warnings in codebase
- [x] Test CI with intentional warning to verify failure
- [x] Document warning policy in CONTRIBUTING.md

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
**Priority:** P1 (Reproducible builds for IEC 62304 compliance)

**Description:**  
Pin Docker base image to specific SHA-256 digest to ensure reproducible builds across environments and time. This is critical for IEC 62304 Class C compliance which requires deterministic build processes and software bill of materials (SBOM) traceability.

**Requirements:**
- IEC 62304: Reproducible build environment requirement
- Binary checksums must match across builds (or documented environment differences)
- SBOM traceability for all dependencies
- Compliance with supply chain security best practices

**Detailed Tasks:**

#### Phase 1: Image Digest Investigation (2-3 hours)
- [ ] **1.1 Identify current base image**
  - [ ] Check current Dockerfile base image line (currently `FROM haskell:9.4`)
  - [ ] Document current tag-based reference
  - [ ] Note any issues with floating tags (security, reproducibility)

- [ ] **1.2 Research available digests**
  - [ ] Pull latest haskell:9.4 image: `docker pull haskell:9.4`
  - [ ] Inspect image: `docker inspect haskell:9.4 --format '{{.RepoDigests}}'`
  - [ ] Record SHA-256 digest (format: sha256:abc123...)
  - [ ] Check Docker Hub for available GHC 9.4.x variants
  - [ ] Consider using specific GHC version tag (e.g., haskell:9.4.8) vs. floating 9.4

- [ ] **1.3 Verify image authenticity**
  - [ ] Check Docker Hub official image badge
  - [ ] Verify image signature if available
  - [ ] Review image build history on Docker Hub
  - [ ] Check for any known vulnerabilities: `docker scan haskell:9.4`
  - [ ] Document image provenance

#### Phase 2: Dockerfile Update (1-2 hours)
- [ ] **2.1 Create backup of current Dockerfile**
  - [ ] Copy to `Dockerfile.bak` for rollback capability
  - [ ] Commit current working Dockerfile to git

- [ ] **2.2 Update FROM statement with digest**
  - [ ] Replace `FROM haskell:9.4` with `FROM haskell:9.4@sha256:<digest>`
  - [ ] Add comment explaining digest pinning rationale
  - [ ] Example format:
    ```dockerfile
    # Pin to specific digest for reproducible builds (IEC 62304 compliance)
    # Image: haskell:9.4.8 as of 2026-02-09
    # Digest verified: sha256:abc123def456...
    # Update procedure: docs/BUILD_GUIDE.md#updating-base-image-digest
    FROM haskell:9.4@sha256:abc123def456...
    ```

- [ ] **2.3 Consider multi-stage build improvements**
  - [ ] Review if separate build and runtime images improve reproducibility
  - [ ] Evaluate using distroless or minimal base for runtime stage
  - [ ] Document trade-offs (image size vs. debugging capability)

- [ ] **2.4 Add image verification step**
  - [ ] Add HEALTHCHECK instruction if not present
  - [ ] Consider adding build-time verification:
    ```dockerfile
    RUN ghc --version | grep "9.4" || (echo "GHC version mismatch" && exit 1)
    ```

#### Phase 3: Local Verification (2-3 hours)
- [ ] **3.1 Clean build test**
  - [ ] Remove all local Docker images: `docker rmi lambda-wave:latest`
  - [ ] Clear Docker build cache: `docker builder prune -a`
  - [ ] Build from scratch: `docker build --no-cache -t lambda-wave:test .`
  - [ ] Time the build and document duration
  - [ ] Check for any build failures or warnings
  - [ ] Verify application runs: `docker run -it lambda-wave:test`

- [ ] **3.2 Extract and checksum binary**
  - [ ] Create temporary container: `docker create --name temp lambda-wave:test`
  - [ ] Extract binary: `docker cp temp:/usr/local/bin/sgrt-radar-system-exe ./binary1`
  - [ ] Calculate checksum: `sha256sum binary1 > checksums.txt`
  - [ ] Remove container: `docker rm temp`

- [ ] **3.3 Rebuild and compare**
  - [ ] Clear build cache again
  - [ ] Rebuild with same Dockerfile: `docker build --no-cache -t lambda-wave:test2 .`
  - [ ] Extract second binary to `binary2`
  - [ ] Calculate checksum: `sha256sum binary2 >> checksums.txt`
  - [ ] Compare checksums: `diff <(sha256sum binary1) <(sha256sum binary2)`

- [ ] **3.4 Analyze differences**
  - [ ] If checksums match: Document success, proceed to Phase 4
  - [ ] If checksums differ: Investigate causes
    - [ ] Check for embedded timestamps in binary
    - [ ] Check for non-deterministic GHC options
    - [ ] Consider using `-fbinary-objects` flag for determinism
    - [ ] Check if Cabal freeze file needed
    - [ ] Document which differences are acceptable (e.g., metadata only)

#### Phase 4: Multi-Machine Verification (4-6 hours)
- [ ] **4.1 Prepare test environments**
  - [ ] Identify 2-3 different build machines/environments:
    - Option A: Local developer machine (Linux/macOS)
    - Option B: GitHub Actions CI runner
    - Option C: Different team member's machine
  - [ ] Document specifications of each environment:
    - OS and version
    - Docker version
    - CPU architecture (x86_64, ARM64)
    - Available RAM
    - Timezone and locale settings

- [ ] **4.2 Build on Environment A**
  - [ ] Clone repository fresh: `git clone <repo-url> lambda-wave-test-a`
  - [ ] Checkout specific commit SHA for consistency
  - [ ] Build: `docker build -t lambda-wave:env-a .`
  - [ ] Extract binary: Save as `binary_env_a`
  - [ ] Calculate checksum: `sha256sum binary_env_a > checksums_multi.txt`
  - [ ] Document build date/time and system info
  - [ ] Save Docker image info: `docker inspect lambda-wave:env-a > image_a.json`

- [ ] **4.3 Build on Environment B**
  - [ ] Repeat 4.2 steps on second environment
  - [ ] Save binary as `binary_env_b`
  - [ ] Append checksum to `checksums_multi.txt`
  - [ ] Document build details

- [ ] **4.4 Build on Environment C**
  - [ ] Repeat 4.2 steps on third environment
  - [ ] Save binary as `binary_env_c`
  - [ ] Append checksum to `checksums_multi.txt`

- [ ] **4.5 Comparative analysis**
  - [ ] Compare all checksums side-by-side
  - [ ] If all match: Success! Document and proceed to Phase 5
  - [ ] If any differ: Deep investigation required
    - [ ] Use `diffoscope` tool for detailed binary comparison
    - [ ] Check for architecture-specific differences (x86 vs ARM)
    - [ ] Check for timestamp or metadata differences
    - [ ] Determine if differences affect functionality
    - [ ] Document findings in validation report

- [ ] **4.6 CI integration test**
  - [ ] Trigger GitHub Actions workflow to build
  - [ ] Download artifact from CI
  - [ ] Compare CI binary checksum to local builds
  - [ ] Document any CI-specific variations

#### Phase 5: Documentation Updates (2-3 hours)
- [ ] **5.1 Update BUILD_GUIDE.md**
  - [ ] Add section: "Reproducible Builds with Docker"
  - [ ] Document current base image digest
  - [ ] Explain why digest pinning is used
  - [ ] Provide instructions for verifying reproducibility
  - [ ] Add section: "Updating Base Image Digest" with step-by-step procedure

- [ ] **5.2 Create digest update procedure**
  - [ ] Write new doc: `docs/procedures/update_docker_base_image.md`
  - [ ] Include steps:
    1. Check for new haskell image releases
    2. Pull and scan new image for vulnerabilities
    3. Update Dockerfile digest
    4. Run full test suite
    5. Verify reproducibility
    6. Create PR with digest update
    7. Document change in CHANGELOG.md
  - [ ] Define update frequency (e.g., quarterly security review)

- [ ] **5.3 Update CI/CD documentation**
  - [ ] Document digest verification in CI workflow
  - [ ] Add automated checks for digest consistency
  - [ ] Configure Dependabot or Renovate for Docker base image updates

- [ ] **5.4 Create SBOM documentation**
  - [ ] Generate Software Bill of Materials (SBOM)
  - [ ] Use tool: `syft` or `docker sbom`
  - [ ] Document all base image dependencies
  - [ ] Save SBOM to `docs/sbom/docker_base_image_sbom.json`
  - [ ] Include SBOM in release artifacts

#### Phase 6: Release Process Integration (1-2 hours)
- [ ] **6.1 Update release checklist**
  - [ ] Add item: "Verify Docker base image digest is current"
  - [ ] Add item: "Verify build reproducibility across environments"
  - [ ] Add item: "Update SBOM for release"
  - [ ] Add item: "Document any digest changes in CHANGELOG"

- [ ] **6.2 Create automated verification**
  - [ ] Add pre-commit hook to check Dockerfile digest format
  - [ ] Add CI check to verify digest hasn't been changed to tag
  - [ ] Alert on Dockerfile changes that affect digest

- [ ] **6.3 Security monitoring**
  - [ ] Set up Docker Hub webhooks for base image updates
  - [ ] Configure vulnerability scanning for pinned image
  - [ ] Define response procedure for critical CVEs in base image

#### Phase 7: Validation & Testing (2-3 hours)
- [ ] **7.1 Functional testing**
  - [ ] Build Docker image with new digest-pinned Dockerfile
  - [ ] Run full test suite inside container: `docker run lambda-wave:test cabal test`
  - [ ] Verify all tests pass
  - [ ] Test with hardware simulation mode
  - [ ] Document any test failures

- [ ] **7.2 Performance testing**
  - [ ] Run benchmark suite in container
  - [ ] Compare performance to previous floating-tag build
  - [ ] Verify no performance regression (should be identical)
  - [ ] Document benchmark results

- [ ] **7.3 Rollback test**
  - [ ] Verify Dockerfile.bak can be restored if needed
  - [ ] Test rollback procedure: `mv Dockerfile Dockerfile.new && mv Dockerfile.bak Dockerfile`
  - [ ] Build with original Dockerfile to verify it still works
  - [ ] Restore new Dockerfile: `mv Dockerfile Dockerfile.bak && mv Dockerfile.new Dockerfile`

**Detailed Acceptance Criteria:**
- ✅ Dockerfile uses digest pin format: `FROM haskell:9.4@sha256:<hash>`
- ✅ Digest is documented with date and verification method
- ✅ Binary checksums match across 2+ different build environments (or documented environment differences explained)
- ✅ CI builds produce consistent binary checksums
- ✅ BUILD_GUIDE.md updated with digest pinning explanation
- ✅ Digest update procedure documented
- ✅ SBOM generated and archived
- ✅ All tests pass with digest-pinned image
- ✅ No performance regression detected

**Risk Analysis:**
- **Risk:** Digest points to image that gets deleted from Docker Hub
  - **Mitigation:** Regularly verify image availability, consider self-hosting
- **Risk:** Security vulnerability in pinned image
  - **Mitigation:** Scheduled security scans, defined update procedure
- **Risk:** Architecture differences cause build variations
  - **Mitigation:** Document architecture-specific requirements, test on all target platforms
- **Risk:** Timestamps or metadata cause checksum differences
  - **Mitigation:** Accept metadata differences if binary functionality identical, document acceptable variations

**Dependencies:**
- None (can start immediately)

**Effort Estimate:** 2-3 days total
- Day 1: Investigation, Dockerfile update, local verification
- Day 2: Multi-machine testing, documentation
- Day 3: CI integration, validation

**Assignee:** TBD (Requires DevOps/Build Engineer skills)  
**Review Requirements:** Two-eyes review of Dockerfile changes and reproducibility report  
**Safety Impact:** Medium (affects software configuration management per IEC 62304)  

**Related Files:**
- `Dockerfile` - Main file to update
- `docs/BUILD_GUIDE.md` - Document digest pinning
- New: `docs/procedures/update_docker_base_image.md` - Update procedure
- New: `docs/sbom/docker_base_image_sbom.json` - SBOM file
- `.github/workflows/build-and-test.yml` - May need CI updates

**Validation Evidence:**
- Checksums file comparing builds across environments
- Build logs from each test environment
- Docker inspect output for reproducibility analysis
- SBOM for compliance documentation

---

### P1-003: Gating Logic & Latency Optimization
**Status:** ✅ Complete
**Phase:** 4.2 - Safety & Control  
**Priority:** P1 (Core functionality)

**Description:**  
Link Kalman state to beam control triggers (GPIO/TTL) with latency compensation.

**Requirements:**
- FR-GAT-001: Automatic beam gating
- FR-GAT-002: Total latency < 50ms (mean), < 75ms (99th percentile)

**Tasks:**
- [x] Implement evaluateGating function
- [x] Add Hardware.Control.setBeam GPIO interface
- [x] Implement latency compensation using velocity prediction
- [x] Add hysteresis logic (Schmidt trigger)
- [x] Run bench/LatencyBench.hs and optimize (Verified < 1ms latency)
- [x] Profile with +RTS -s and reduce GC pauses
- [x] Document gating algorithm in Control/Gating.hs

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
**Status:** ✅ Complete
**Phase:** 4.3 - Safety & Control  
**Priority:** P1 (Compliance requirement)

**Description:**  
Finalize immutable audit logging with immediate disk flush on critical events.

**Requirements:**
- SR-AUDIT-001: Immutable event log
- IEC 62304: Audit trail for safety events

**Tasks:**
- [x] Implement immediate flush on "Beam Hold" events
- [x] Add buffered logging for non-critical events (TBQueue with 10MB rotation)
- [x] Implement log rotation and archival (Rename to .bak on limit)
- [x] Add crash recovery test (power plug simulation) (Simulated via Unit Test `test/Safety/AuditCheck.hs`)
- [x] Verify last event recorded after crash
- [x] Document log format and retention policy

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
**Priority:** P1 (Validation - Required for v1.0.0)

**Description:**  
Create comprehensive integration test using real sensor data captured from TI mmWave Studio. Replay captured binary files through the complete system pipeline to validate packet parsing, data processing, and system stability with authentic sensor data. This provides confidence that the system handles real-world sensor output correctly before hardware deployment.

**Requirements:**
- FR-DAQ-003: Packet parser validation with real sensor data
- Ensure parser handles all TLV packet types correctly
- Validate frame synchronization and timing
- Verify system stability over extended replay periods
- Establish regression test baseline for parser changes

**Detailed Tasks:**

#### Phase 1: Test Data Acquisition (1-2 days)
- [ ] **1.1 Hardware setup preparation**
  - [ ] Verify access to TI IWR6843ISK sensor
  - [ ] Install TI mmWave Studio (version 2.1+ recommended)
  - [ ] Connect sensor via USB (data port + config port)
  - [ ] Verify sensor firmware version: `config/ti_iwr6843isk/firmware_version.txt`
  - [ ] Document hardware setup (sensor orientation, mounting, etc.)

- [ ] **1.2 Configure sensor for data capture**
  - [ ] Load SGRT profile: `config/ti_iwr6843isk/sgrt_profile.cfg`
  - [ ] Verify configuration loaded successfully
  - [ ] Set frame rate to 60Hz
  - [ ] Configure data output to binary file
  - [ ] Test short 5-second capture to verify setup

- [ ] **1.3 Capture test scenario 1: Static scene**
  - [ ] Set up empty room or static background
  - [ ] Start mmWave Studio capture
  - [ ] Record 60 seconds (3,600 frames at 60Hz)
  - [ ] Save as: `test/fixtures/captures/static_background_60s.bin`
  - [ ] Document scene description, sensor parameters
  - [ ] Record file size and timestamp

- [ ] **1.4 Capture test scenario 2: Moving object**
  - [ ] Place human subject or moving phantom in sensor field of view
  - [ ] Perform controlled movement (e.g., arm raise, breathing simulation)
  - [ ] Record 120 seconds (7,200 frames)
  - [ ] Save as: `test/fixtures/captures/moving_object_120s.bin`
  - [ ] Document motion pattern and timing
  - [ ] Take photo/video of scene for reference

- [ ] **1.5 Capture test scenario 3: Edge cases**
  - [ ] Capture scene with multiple moving objects
  - [ ] Capture scene with fast motion (simulate cough)
  - [ ] Capture scene with minimal motion (sleeping patient simulation)
  - [ ] Save as separate .bin files with descriptive names
  - [ ] Document each scenario in `test/fixtures/captures/README.md`

- [ ] **1.6 Extract ground truth data**
  - [ ] Use mmWave Studio analyzer to inspect captures
  - [ ] Document expected frame count for each file
  - [ ] Note any anomalies or special events (frame drops, etc.)
  - [ ] Export point cloud statistics if available
  - [ ] Create reference JSON file with expected values:
    ```json
    {
      "filename": "static_background_60s.bin",
      "duration_s": 60,
      "expected_frames": 3600,
      "frame_rate_hz": 60,
      "capture_date": "2026-02-09",
      "sensor_firmware": "3.6.0",
      "config_profile": "sgrt_profile.cfg",
      "notes": "Empty room, sensor at 1.5m height"
    }
    ```

#### Phase 2: Test Infrastructure Development (2-3 days)
- [ ] **2.1 Create replay module**
  - [ ] Create new module: `test/Replay/BinaryFileReplay.hs`
  - [ ] Implement file reading functions:
    ```haskell
    -- Read binary file and split into frames
    loadCaptureFile :: FilePath -> IO [ByteString]
    
    -- Replay frames at specified rate
    replayFrames :: [ByteString] -> Double -> IO ()
    
    -- Parse frames and validate structure
    validateFrames :: [ByteString] -> Either ParseError [RadarFrame]
    ```
  - [ ] Handle frame synchronization (magic word detection)
  - [ ] Implement timing control (replay at 60Hz or faster)

- [ ] **2.2 Create test fixtures loading**
  - [ ] Add cabal configuration for test data files
  - [ ] Update `sgrt-radar-system.cabal`:
    ```cabal
    test-suite sgrt-radar-system-test
      ...
      data-files:
        test/fixtures/captures/*.bin
        test/fixtures/captures/*.json
    ```
  - [ ] Create fixture helper:
    ```haskell
    getFixturePath :: String -> IO FilePath
    ```

- [ ] **2.3 Create frame validator**
  - [ ] Implement frame structure validation:
    ```haskell
    validateFrameStructure :: ByteString -> ValidationResult
    data ValidationResult = Valid | InvalidMagicWord | InvalidLength | CorruptData
    ```
  - [ ] Check magic word: 0x0102030405060708
  - [ ] Verify frame length consistency
  - [ ] Validate TLV checksums if present
  - [ ] Check point cloud coordinate ranges (sanity checks)

- [ ] **2.4 Create statistics collector**
  - [ ] Implement frame statistics collection:
    ```haskell
    data FrameStats = FrameStats
      { totalFrames :: Int
      , validFrames :: Int
      , corruptFrames :: Int
      , averagePointCount :: Double
      , averageFrameSize :: Int
      , processingErrors :: [ParseError]
      }
    
    collectStats :: [RadarFrame] -> FrameStats
    ```
  - [ ] Track parse success rate
  - [ ] Collect timing statistics
  - [ ] Record error types and frequencies

#### Phase 3: Test Implementation (3-4 days)
- [ ] **3.1 Write test spec for static scene**
  - [ ] Create: `test/Integration/StaticSceneReplaySpec.hs`
  - [ ] Test structure:
    ```haskell
    spec :: Spec
    spec = describe "Static scene replay" $ do
      it "parses all frames successfully" $ do
        frames <- loadCaptureFile "test/fixtures/captures/static_background_60s.bin"
        let results = map parseFrame frames
        all isRight results `shouldBe` True
      
      it "extracts correct frame count" $ do
        frames <- loadCaptureFile "..."
        length frames `shouldBe` 3600
      
      it "detects no motion in static scene" $ do
        frames <- loadAndParse "..."
        let motionDetected = any hasSignificantMotion frames
        motionDetected `shouldBe` False
    ```

- [ ] **3.2 Write test spec for moving object**
  - [ ] Create: `test/Integration/MovingObjectReplaySpec.hs`
  - [ ] Tests to include:
    - [ ] Frame parsing success rate > 99%
    - [ ] Point cloud contains expected objects
    - [ ] Motion detection triggers correctly
    - [ ] Phase unwrapping handles motion correctly
    - [ ] Kalman filter tracks smoothly (no jumps)
    - [ ] Gating logic responds appropriately

- [ ] **3.3 Write parser regression tests**
  - [ ] Create: `test/Integration/ParserRegressionSpec.hs`
  - [ ] Test against known-good baseline
  - [ ] Verify backward compatibility with old captures
  - [ ] Test all TLV packet types encountered:
    - [ ] TLV Type 1: Point cloud
    - [ ] TLV Type 2: Range profile (if used)
    - [ ] TLV Type 6: Statistics
    - [ ] Any other types in sgrt_profile.cfg

- [ ] **3.4 Write performance tests**
  - [ ] Create: `test/Integration/ReplayPerformanceSpec.hs`
  - [ ] Measure parsing latency per frame
  - [ ] Verify can process at 60Hz+ (< 16.67ms per frame)
  - [ ] Check memory usage stability over 1000+ frames
  - [ ] Ensure no memory leaks during replay

- [ ] **3.5 Write stress tests**
  - [ ] Test with corrupted frames (inject errors)
  - [ ] Test with incomplete frames (truncated files)
  - [ ] Test with out-of-order frames
  - [ ] Test with extremely long replay (10+ minutes)
  - [ ] Verify watchdog doesn't trigger false alarms

#### Phase 4: Integration with Existing Test Suite (1 day)
- [ ] **4.1 Update test runner**
  - [ ] Add integration tests to `test/Spec.hs`:
    ```haskell
    main :: IO ()
    main = hspec $ do
      describe "Unit Tests" $ do
        -- existing unit tests
      describe "Integration Tests" $ do
        StaticSceneReplaySpec.spec
        MovingObjectReplaySpec.spec
        ParserRegressionSpec.spec
        ReplayPerformanceSpec.spec
    ```

- [ ] **4.2 Configure test execution**
  - [ ] Update cabal test configuration
  - [ ] Set appropriate timeout for long tests
  - [ ] Configure test parallelization if beneficial
  - [ ] Add test groups for selective execution

- [ ] **4.3 Handle large test data**
  - [ ] Evaluate if .bin files should be in Git (size concern)
  - [ ] Consider Git LFS for large binary files
  - [ ] Alternative: Generate synthetic data for CI
  - [ ] Document data management strategy

#### Phase 5: Continuous Integration Setup (1 day)
- [ ] **5.1 Update CI workflow**
  - [ ] Add integration test step to `.github/workflows/build-and-test.yml`
  - [ ] Upload test fixtures to CI environment
  - [ ] Configure artifact storage for test results
  - [ ] Set up test result reporting

- [ ] **5.2 CI optimization**
  - [ ] Cache test fixtures to speed up CI
  - [ ] Run integration tests on demand (not every commit)
  - [ ] Create separate CI job for integration tests
  - [ ] Configure timeout for long-running tests

- [ ] **5.3 Create test data generation script** (if Git LFS not used)
  - [ ] Write script to generate minimal test data: `scripts/generate_test_data.sh`
  - [ ] Synthetic data for CI (no real sensor needed)
  - [ ] Document limitations of synthetic vs. real data

#### Phase 6: Validation & Documentation (2 days)
- [ ] **6.1 Run full test suite locally**
  - [ ] Execute: `cabal test --test-show-details=direct`
  - [ ] Verify all integration tests pass
  - [ ] Document execution time for each test
  - [ ] Check test coverage of parser code
  - [ ] Review any flaky tests and stabilize

- [ ] **6.2 Validate against mmWave Studio**
  - [ ] Compare Lambda-Wave parser output to mmWave Studio analysis
  - [ ] Verify frame count matches
  - [ ] Compare point cloud coordinates (spot check)
  - [ ] Document any discrepancies and investigate

- [ ] **6.3 Create test report**
  - [ ] Write: `docs/testing/integration_test_report.md`
  - [ ] Include test methodology
  - [ ] Present test results with pass/fail counts
  - [ ] Document test coverage metrics
  - [ ] List known limitations or issues
  - [ ] Include recommendations for future tests

- [ ] **6.4 Update testing documentation**
  - [ ] Update `docs/DEVELOPER_GUIDE.md` with integration test section
  - [ ] Document how to capture new test data
  - [ ] Explain how to run integration tests locally
  - [ ] Document test data management strategy

- [ ] **6.5 Create fixture README**
  - [ ] Write: `test/fixtures/captures/README.md`
  - [ ] Document each test file
  - [ ] Explain capture conditions
  - [ ] Provide expected values and checksums
  - [ ] Include instructions for adding new fixtures

#### Phase 7: Regression Protection (1 day)
- [ ] **7.1 Establish baseline**
  - [ ] Run tests and save output as baseline
  - [ ] Commit baseline to repository
  - [ ] Document baseline date and conditions

- [ ] **7.2 Create comparison tools**
  - [ ] Script to compare new test results to baseline
  - [ ] Alert on unexpected changes
  - [ ] Provide diff visualization for failures

- [ ] **7.3 Version control for test data**
  - [ ] Tag test data version in Git
  - [ ] Document test data version in test files
  - [ ] Define procedure for updating test data

**Detailed Acceptance Criteria:**
- ✅ At least 3 different capture scenarios (.bin files) available
- ✅ Parser processes all real sensor frames without crashing
- ✅ Frame count matches expected count from mmWave Studio (±1% tolerance)
- ✅ Point cloud coordinates within expected ranges (e.g., -5m to +5m)
- ✅ Parser handles all TLV types in captured data
- ✅ Parse success rate > 99% (allowing for rare corrupted frames)
- ✅ Integration tests run in < 60 seconds total
- ✅ Tests are deterministic (same results on repeated runs)
- ✅ Documentation complete for adding new test captures
- ✅ CI integration working (tests run on pull requests)

**Success Metrics:**
- Parser accuracy: 100% on known-good frames
- Performance: Process frames at 60Hz+ (real-time capable)
- Reliability: Zero crashes over 10-minute replay
- Coverage: All TLV packet types tested

**Risk Mitigation:**
- **Risk:** Real sensor not available → Mitigation: Use previously captured data or synthetic data generator
- **Risk:** Test data too large for Git → Mitigation: Use Git LFS or generate synthetic data
- **Risk:** Test data becomes stale → Mitigation: Schedule periodic recapture with latest firmware
- **Risk:** Parser changes break tests → Mitigation: Version test data and maintain backward compatibility tests

**Rollback Plan:**
If integration tests reveal parser issues:
1. Document specific failure modes
2. Create minimal failing test case
3. Fix parser or adjust test expectations
4. Re-validate with full capture replay

**Dependencies:**
- Access to TI IWR6843ISK sensor (for initial capture - can reuse captures afterward)
- TI mmWave Studio installed (for capture and validation)
- Sufficient disk space for test fixtures (~100MB per minute of capture)

**Effort Estimate:** 1-2 weeks total
- Week 1: Data capture, test infrastructure, initial tests
- Week 2: Additional tests, CI integration, documentation

**Assignee:** TBD (Requires knowledge of packet parsing and Haskell testing)  
**Review Requirements:** Two-eyes review of test code, validation of test data authenticity  
**Safety Impact:** Medium (validates critical data ingestion path)

**Related Files:**
- `test/Hardware/ConsumerSpec.hs` - Extend existing parser tests
- New: `test/Integration/StaticSceneReplaySpec.hs`
- New: `test/Integration/MovingObjectReplaySpec.hs`
- New: `test/Integration/ParserRegressionSpec.hs`
- New: `test/Integration/ReplayPerformanceSpec.hs`
- New: `test/Replay/BinaryFileReplay.hs` - Replay infrastructure
- New: `test/fixtures/captures/*.bin` - Captured data files
- New: `test/fixtures/captures/*.json` - Ground truth metadata
- New: `test/fixtures/captures/README.md` - Test data documentation
- New: `docs/testing/integration_test_report.md` - Test results
- Update: `.github/workflows/build-and-test.yml` - CI integration

**Follow-up Tasks:**
After completion, consider:
- Automated capture refresh (quarterly with new firmware)
- Extended stress tests (24-hour replays)
- Multi-sensor captures (when multi-sensor support added)
- Performance profiling using replay data

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
- [x] P0-002: Full watchdog functionality
- [ ] P0-003: Hardware validation with motion phantom

Required:
- [ ] P1-001: CI/CD strictness (-Werror)
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

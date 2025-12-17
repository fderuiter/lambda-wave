# Phase 0 Project Setup Report: Surface Guided Radiation Therapy (SGRT) System

See `Haskell Radar SGRT System Development.md` for full architectural details.

## 1. Repository Strategy & Directory Structure

### Repository Type
**Recommendation:** Monorepo

**Justification:**
*   **Atomic Commits:** Enables synchronized changes across tightly coupled components (e.g., C++ FFI ring buffers and their Haskell consumers), ensuring the system is never in an inconsistent state.
*   **Simplified Dependency Management:** Streamlines the coordination of build tooling and dependencies between the Haskell stack (cabal) and C++ components (gcc/clang).
*   **Centralized Auditing:** Consolidates all project assets (code, hardware config, documentation, scripts) into a single view, which is essential for verifying IEC 62304 compliance during audits.

### File Tree
```
sgrt-radar-system/
├── .github/                  # GitHub-specific automation and governance
│   ├── ISSUE_TEMPLATE/
│   │   └── bug_report.md
│   └── workflows/
│       ├── build-and-test.yml
│       ├── lint.yml
│       └── release.yml
├── cbits/                    # C/C++ source for FFI layer
│   ├── include/
│   └── src/
├── config/                   # System and hardware configuration
│   └── ti_iwr6843isk/
│       └── sgrt_profile.cfg
├── docs/                     # Documentation and compliance artifacts
│   ├── architecture.md
│   └── iec_62304/
├── scripts/                  # Build and deployment scripts
├── src/                      # Haskell source code (Core Logic)
│   ├── Control/              # Control Plane (Gating, Kalman Filter)
│   ├── Data/                 # Data types
│   ├── Hardware/             # Radar interface via FFI
│   ├── Safety/               # Watchdog timers, failsafes
│   └── SignalProcessing/     # Data Plane (LLS, filtering)
├── test/                     # Test suites
├── .dockerignore
├── .env.example
├── .gitignore
├── .hlint.yaml
├── cabal.project
├── Dockerfile
├── LICENSE
├── PULL_REQUEST_TEMPLATE.md
├── README.md
└── sgrt-radar-system.cabal
```

### Naming Conventions
*   **Branches (Git-flow):**
    *   `main`: Production releases only (direct commits forbidden).
    *   `develop`: Primary integration branch.
    *   `feature/<issue-id>-short-description`: New development.
    *   `bugfix/<issue-id>-short-description`: Defect resolution.
    *   `release/vX.Y.Z`: Release stabilization.
*   **Directories/Files:**
    *   **Haskell:** PascalCase matching module hierarchy (e.g., `src/Hardware/Radar/Ingestion.hs` → `Hardware.Radar.Ingestion`).
    *   **C/C++:** `snake_case` for all source files.

---

## 2. The "Skeleton": Configuration & Tooling

### Core Configuration Files
*   **`sgrt-radar-system.cabal`**: Defines build instructions, package dependencies (`hmatrix`, `stm`, `clock`, `binary`), and links to C++ FFI code in `cbits/`.
*   **`cabal.project`**: Locks exact dependency versions to ensure reproducible builds across environments.
*   **`config/ti_iwr6843isk/sgrt_profile.cfg`**: Contains chirp parameters for the TI IWR6843ISK sensor.
*   **`Dockerfile`**: encapsulating the build environment including GHC, GCC/Clang, and the TI SDK.

**Hardware Constraints:**
The physical connection setup (documented in README) must adhere to specific limits:
*   Interface: XDS110 USB-to-UART.
*   Baud Rate Limit: 921,600 baud.

### Linter & Formatter Strategy
*   **Haskell:**
    *   Linting: `hlint` (static analysis).
    *   Formatting: `ormolu`.
*   **C/C++:**
    *   Formatting: `clang-format` (targeting `cbits/`).

### Environment Management
*   **Strategy:** Development uses a Dockerized Haskell toolchain to ensure consistency.
*   **Local Configuration:** Managed via a `.env` file (template: `.env.example`) to handle local paths and hardware ports.

---

## 3. GitHub Actions & CI/CD Workflows

### Workflows
*   **`lint.yml`**: Triggered on push to `feature/*` and `bugfix/*`. Runs `hlint` and `clang-format`.
*   **`build-and-test.yml`**: Triggered on PRs to `develop` or `main`. executes the build and runs the full test suite.
*   **`release.yml`**: Triggered by a semantic version tag (`vX.Y.Z`). Builds the static binary, generates a SHA256 checksum, and creates a GitHub Release.

### Pull Request Checks ("Gates")
To ensure quality and compliance, the following gates must pass before merging:
1.  **Code Linting:** `lint.yml` must succeed.
2.  **Successful Build:** Both Haskell and C++ components must compile successfully within the Docker environment.
3.  **Unit & Property Testing:** All test suites must pass, with strict enforcement of QuickCheck properties for `SignalProcessing`.

### Release Automation & Deployment
*   **Release Generation:** Automated via `release.yml` upon tagging `main`.
*   **Manual Deployment Rule:** In compliance with IEC 62304 Class C standards, there is **no auto-deployment**. Deployment is a manual, audited process using the officially generated release binary and its corresponding SHA256 checksum.

---

## 4. GitHub Governance & Administration

### Branch Protection Rules (`main` & `develop`)
1.  Require a Pull Request before merging.
2.  Require status checks to pass (`lint`, `build-and-test`).
3.  Require at least two approvals ("four-eyes" principle).
4.  Require linear history (no merge commits).
5.  Forbid force pushes.

### Templates

**`PULL_REQUEST_TEMPLATE.md`**
```markdown
## Related Issue
[Issue Link]

## Summary
[Brief description of changes]

## Verification Steps
[How to verify the fix/feature]

## Definition of Done Checklist
- [ ] Code Linting Passed
- [ ] Build Successful
- [ ] Unit/Property Tests Passed
- [ ] Safety/Compliance Review (if applicable)
```

**`ISSUE_TEMPLATE.md` (Bug Report)**
```markdown
## Steps to Reproduce
1. [Step 1]
2. [Step 2]

## Expected Behavior
[Description]

## Actual Behavior
[Description]

## System Environment
- SW Version:
- HW Version:

## Logs
[Paste logs here]
```

### Issue Labels
*   `bug`
*   `feature`
*   `safety-critical`
*   `iec-62304`
*   `dsp-firmware`
*   `haskell-core`
*   `ffi-interface`
*   `testing`
*   `documentation`

---

## 5. Documentation & Onboarding (DX)

### README Structure
To enable a developer to get running in <15 minutes:
1.  **Project Overview**: High-level summary.
2.  **System Architecture**: Brief explanation of components.
3.  **Prerequisites**: Required Hardware (TI Sensor) and Software (Docker).
4.  **Quick Start**: Steps to run the simulation mode.
5.  **Running Tests**: Command to execute the test suite.
6.  **Hardware Operation**: Instructions for physical sensor setup.

### CONTRIBUTING.md
*   **Development Workflow**: Git-flow steps.
*   **Coding Standards**: Style guides for Haskell and C++.
*   **PR Process**: Template usage and review requirements.
*   **Safety-Critical Code**: Specific procedures for reviewing and modifying safety-critical logic, emphasizing the "four-eyes" principle and rigorous testing.

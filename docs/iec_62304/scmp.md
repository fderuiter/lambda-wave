# Software Configuration Management Plan (SCMP)

## 1. Introduction
This document defines the Software Configuration Management Plan (SCMP) for the Lambda-Wave project in compliance with IEC 62304 Class C.

## 2. Configuration Management Strategy
All source code, documentation, and configuration items are versioned using Git. Changes are governed by Pull Requests, code reviews, and automated CI/CD gating.

## 3. Technical Constraints & Guardrails
To ensure safety and compliance, the following technical constraints are enforced by the CI pipeline:

- **Test Coverage:** A strict **90% test coverage requirement** is enforced for all safety-critical modules (e.g., `Safety.Watchdog`, `Safety.Audit`).
- **Compiler Safety Flags:** All components must compile without warnings. The flags `-Wall` and `-Werror` are mandatory.
- **Reproducible Builds:** The GHC base image is locked via SHA-256 digests in Dockerfile to guarantee reproducibility.
- **RTS Flags:** The GHC Runtime System must be locked using specific CPU affinity (`-qa`) and thread capability (`-N2`) configurations to ensure determinism.

## 4. Configuration Items (CIs)
The following items are under strict configuration control:
- Source code (`src/`, `cbits/`)
- Compliance documentation (`docs/iec_62304/`)
- Test scripts and validation protocols (`test/`, `bench/`)
- Build configurations (`sgrt-radar-system.cabal`, `cabal.project`, `Dockerfile`)

## 5. Anomaly and Problem Resolution
In compliance with IEC 62304 clause 5.1.9, 5.1.10, and 5.1.11, all identified software anomalies and problem reports must be tracked as formal issues in the project repository, assessed for safety impact by the QA/Safety Officer, and traced to specific pull requests that resolve them.

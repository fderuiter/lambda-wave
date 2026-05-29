# Software Development Plan (SDP)

## 1. Introduction
This document defines the Software Development Plan (SDP) for the Lambda-Wave (Haskell Radar SGRT) project, in compliance with IEC 62304 Class C requirements.

## 2. SDLC Model
The project follows a hybrid V-Model mapped to IEC 62304 phases:
- **Software Requirements Analysis (Clause 5.2):** Requirements are documented and traced.
- **Software Architectural Design (Clause 5.3):** Defined in the architecture documentation.
- **Software Detailed Design (Clause 5.4):** Implemented iteratively.
- **Software Unit Implementation and Verification (Clause 5.5):** Rigorous unit testing and static analysis.
- **Software Integration and Integration Testing (Clause 5.6):** Continuous Integration pipeline checks.
- **Software System Testing (Clause 5.7):** Phantom study and latency verification.
- **Software Release (Clause 5.8):** Signed binaries and documentation packages.

## 3. Milestones
The development roadmap is broken down into specific milestones, mapped to standard SDLC phases. 

- **Milestone Phase 1: Infrastructure & High-Assurance Setup**
  - **Target Date:** 2026-06-15
  - **SDLC Phase:** Architecture & Tooling Setup
- **Milestone Phase 2: Hardware Abstraction Layer (Ingestion)**
  - **Target Date:** 2026-07-01
  - **SDLC Phase:** Unit Implementation
- **Milestone Phase 3: Signal Processing Core (Physics Engine)**
  - **Target Date:** 2026-08-01
  - **SDLC Phase:** Unit Implementation & Verification
- **Milestone Phase 4: Safety & Control (The "Class C" Core)**
  - **Target Date:** 2026-09-01
  - **SDLC Phase:** Integration Testing
- **Milestone Phase 5: User Interface & Visualization**
  - **Target Date:** 2026-10-01
  - **SDLC Phase:** System Testing
- **Milestone Phase 6: System Validation (Verification)**
  - **Target Date:** 2026-11-01
  - **SDLC Phase:** Release Preparation

## 4. RACI Matrix
The following RACI matrix defines clear accountability for regulatory and development activities.

| Activity / Artifact | Lead Developer | QA/Safety Officer |
|---------------------|----------------|-------------------|
| Requirement Specs   | C              | A / R             |
| Architecture Design | A / R          | C                 |
| Implementation      | A / R          | I                 |
| Unit Testing        | A / R          | I                 |
| Integration Testing | R              | A                 |
| Safety & SOUP       | C              | A / R             |
| Release Approval    | I              | A / R             |

*(R: Responsible, A: Accountable, C: Consulted, I: Informed)*

## 5. Required IEC 62304 Planning Clauses
- **5.1.1 Software development plan:** This document.
- **5.1.2 Keep software development plan updated:** Version controlled in Git.
- **5.1.3 Software development plan reference to system design and development:** Traced via requirements.
- **5.1.4 Software development standards, methods and tools:** Defined in SCMP and architecture docs.
- **5.1.5 Software integration and integration testing planning:** Defined in SDLC model.
- **5.1.6 Software verification planning:** Enforced by CI/CD.
- **5.1.7 Software risk management planning:** Managed via SOUP and Hazard analysis.
- **5.1.8 Documentation planning:** Versioned documentation package.
- **5.1.9 Software configuration management planning:** Addressed in SCMP.
- **5.1.10 Software anomaly resolution planning:** Tracked via GitHub issues.
- **5.1.11 Software problem resolution planning:** Addressed in SCMP and Issue tracker.

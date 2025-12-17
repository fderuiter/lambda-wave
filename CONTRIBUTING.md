# Contributing to SGRT Radar System

## Development Workflow
We follow Git-flow:
*   `main`: Production releases only.
*   `develop`: Integration branch.
*   `feature/*`: New features.
*   `bugfix/*`: Bug fixes.

## Coding Standards
*   **Haskell:** Use `hlint` and `ormolu`.
*   **C/C++:** Use `clang-format`.

## PR Process
1.  Create a branch from `develop`.
2.  Implement changes.
3.  Ensure linting and tests pass.
4.  Open a PR using the template.
5.  Wait for 2 approvals.

## Safety-Critical Code
For safety-critical components (e.g., `Safety/`, `Control/Gating.hs`):
*   **Four-eyes principle:** At least two reviewers must approve.
*   **Rigorous Testing:** Property-based testing (QuickCheck) is mandatory.
*   **Audit:** Changes must be traceable to requirements.

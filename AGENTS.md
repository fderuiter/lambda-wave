# AGENTS.md - SGRT Radar System Developer Guide

This file provides the "operating instructions" for AI Agents and human developers working on the SGRT Radar System. It defines the project's persona, technology stack, and critical safety rules.

## 1. Project Persona
**Role:** Senior Safety-Critical Systems Engineer (Haskell/C++).
**Domain:** Medical Device Software (Class C - High Integrity).
**Focus:** Formal correctness, deterministic latency, memory safety (zero allocation in hot loops), and rigorous error handling.
**Tone:** Precise, technical, and safety-conscious. Avoid speculation; prefer verifiable facts.

## 2. Tech Stack Map
*   **Languages:**
    *   **Haskell:** GHC 9.4+, Haskell 2010 (Extensions: `OverloadedStrings`, `StrictData`, `CPP`, `NumericUnderscores`).
    *   **C++:** C++11 (GCC/Clang). used for low-level ring buffers (`cbits/`).
*   **Build System:** Cabal (`cabal-version: 2.2`).
*   **Core Libraries:**
    *   `stm`: Concurrency control (Software Transactional Memory).
    *   `binary` / `bytestring`: High-performance serialization.
    *   `unix`: POSIX system calls.
    *   `deepseq`: strict evaluation.
*   **UI Frameworks (Optional/Flag-Gated):**
    *   `GLUT` / `OpenGL`: Desktop visualization (`-f enable-ui`).

## 3. Rules of Engagement
These rules are mandatory for all `src/` and `cbits/` components.

### 3.1 The "Power of 10" (Adapted for Haskell/C++)
| Rule | Key Constraint | Goal |
|---|---|---|
| 1 | No Recursion/Goto | Predictable execution flow (use tail recursion/folds). |
| 2 | Fixed Loops | Prevent infinite loops; prove termination. |
| 3 | No Dynamic Allocation | No `malloc`/`new` in loops; strict Haskell data. |
| 4 | Short Functions | <60 lines for readability & verification. |
| 5 | High Assertion Density | `assert` impossible states; 2+ per function. |
| 6 | Small Data Scope | No global `IORef`s; use `ReaderT` / explicit passing. |
| 7 | Check Return Values | No partial functions (`head`, `tail`); check C returns. |
| 8 | Limit Preprocessor | Minimal `CPP`; no `#ifdef` logic in functions. |
| 9 | Restricted Pointers | `ForeignPtr` with `bracket`; max 1-level deref (C++). |
| 10 | Zero Warnings | Must compile with `-Wall -Werror`. |

### 3.2 Safety & Concurrency
*   **STM:** Use `atomically` for all shared state updates.
*   **FFI:** Always use `bracket` or `ForeignPtr` finalizers for C resources.
*   **Partial Functions:** Banned. Use `Maybe`, `Either`, or pattern matching.

## 4. Definition of Done
A Pull Request (PR) is ready for review only when:
1.  **Builds Cleanly:** `cabal build` passes with **zero warnings** (`-Werror`).
2.  **Tests Pass:** All relevant suites pass (e.g., `cabal test audit-check`).
3.  **Linted:** Code follows `hlint` (Haskell) and `clang-format` (C++) rules.
4.  **No Regressions:** Does not break existing safety properties (e.g., latency, memory).

## 5. Tooling & Paths
*   **Critical Directories:**
    *   `src/`: Safety-critical library code.
    *   `app/`: Main executable and UI logic.
    *   `cbits/`: C++ Ring Buffer implementation.
    *   `test/`: Verification suites (`AuditCheck`, `KalmanCheck`, etc.).
*   **Common Commands:**
    *   **Build:** `cabal build`
    *   **Run:** `cabal run sgrt-radar-system-exe`
    *   **Test (Specific):** `cabal test audit-check`
    *   **Test (All - Note: some suites disabled):** `cabal test all`
    *   **REPL:** `cabal repl`

## 6. Contextual Grounding
The SGRT Radar System processes realtime radar data for patient positioning in radiation therapy.
*   **Ingestion:** Reads binary frames from hardware via C++ Ring Buffer.
*   **Processing:** Kalman filtering (`SignalProcessing/Kalman`) and Regression.
*   **Safety:** Watchdog timers (`Safety/Watchdog`) and Audit Logging (`Safety/Audit`) ensure fail-safe operation.
*   **Gating:** `Control/Gating` makes the final beam-hold decision.

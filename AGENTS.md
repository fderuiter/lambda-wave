# AGENTS.md - NASA Power of 10 Rules for SGRT Radar System

This document outlines the safety-critical coding standards adapted from NASA's JPL Power of 10 Rules for the SGRT Radar System (Haskell/C++). These rules are mandatory for all `src/Safety/`, `src/Control/`, and `cbits/` components.

## 1. Simple Control Flow
**Restrict all code to very simple control flow constructs.**
*   **The Rule:**
    *   **Haskell:** Avoid complex, deeply nested recursion where possible; prefer standard higher-order functions (map, fold, traverse) or tail-recursive functions.
    *   **C/C++:** Do not use `goto`, `setjmp`, `longjmp`, or direct recursion.
*   **The Why:** Recursion and complex jumps make it nearly impossible to prove how much stack memory a program will use or to analyze the code automatically for errors.

## 2. Fixed Loop Bounds
**All loops must have a fixed upper bound.**
*   **The Rule:**
    *   **Haskell:** Ensure all recursive functions and iterative processes (e.g., `forever` loops in threads) have termination conditions or are explicitly designed as non-terminating system processes with watchdog supervision.
    *   **C/C++:** It must be possible for a verification tool to prove that a loop cannot exceed a set number of iterations.
*   **The Why:** This prevents infinite loops and ensures the software will always return a result within a predictable amount of time (crucial for real-time systems).

## 3. No Dynamic Memory Allocation
**Do not use dynamic memory allocation after initialization.**
*   **The Rule:**
    *   **Haskell:** Minimize allocation in hot loops. Use strict data structures (`Data.Vector.Unboxed`, strict fields) to avoid thunk buildup.
    *   **C/C++:** Avoid `malloc`, `free`, or `new`/`delete` during the main execution loop. All memory should be pre-allocated during initialization.
*   **The Why:** Dynamic memory management is a common source of bugs (memory leaks, dangling pointers) and unpredictable performance. Using a fixed block of pre-allocated memory is much safer.

## 4. Limit Function Length
**No function should be longer than what can be printed on a single sheet of paper.**
*   **The Rule:** Typically, this means about 60 lines of code per function.
*   **The Why:** Short functions are easier to read, understand, and verify. If a function is too long, it’s usually doing too many things and should be broken down.

## 5. Assertion Density
**The assertion density of the code should average to a minimum of two assertions per function.**
*   **The Rule:**
    *   **Haskell:** Use `assert` (from `Control.Exception`) or property-based tests to check for "impossible" conditions.
    *   **C/C++:** Use `assert()` macros aggressively.
*   **The Why:** Assertions act as active documentation and catch bugs early during testing. If an assertion fails, you know exactly where and why the program state is invalid.

## 6. Small Data Scope
**Data objects must be declared at the smallest possible level of scope.**
*   **The Rule:** Avoid global variables (`unsafePerformIO`, global `IORef`s). Pass state explicitly or use `ReaderT`/`StateT` monads in Haskell. In C/C++, declare variables as close to usage as possible.
*   **The Why:** This makes it easier to track where data is being modified. Global variables create "spooky action at a distance," where changing a value in one part of the code breaks something seemingly unrelated.

## 7. Check Return Values
**The return value of non-void functions must be checked by each calling function.**
*   **The Rule:**
    *   **Haskell:** Handle `Maybe`, `Either`, and `IO` results explicitly. Do not use partial functions (e.g., `head`, `fromJust`) that can crash on valid return types.
    *   **C/C++:** Check all return codes, especially from system calls and FFI boundaries.
*   **The Why:** Ignoring return values often leads to silent failures where the program continues running in an invalid state.

## 8. Limited Preprocessor Use
**The use of the preprocessor must be limited to the inclusion of header files and simple macro definitions.**
*   **The Rule:**
    *   **Haskell:** Minimize `CPP` usage (`-XCPP`). Use it only for cross-platform compatibility or testing mocks.
    *   **C/C++:** Avoid conditional compilation (like `#ifdef`) and complex macros in the middle of functions.
*   **The Why:** The preprocessor is powerful but can make code obscure and hard for analysis tools to parse correctly.

## 9. Restricted Pointer Use
**The use of pointers should be restricted.**
*   **The Rule:**
    *   **Haskell:** Use `Ptr` and `ForeignPtr` only when interfacing with C. Ensure proper resource management with `bracket`.
    *   **C/C++:** No more than one level of dereferencing is allowed. Function pointers should generally be avoided.
*   **The Why:** Pointers are often the hardest part of C to understand and verify. Restricting their complexity reduces the chance of accessing invalid memory.

## 10. Compile with All Warnings
**All code must be compiled, from the first day of development, with all compiler warnings enabled at the most pedantic setting.**
*   **The Rule:** The code must compile with zero warnings (`-Wall`, `-Werror` in GHC; `-Wall -Wextra -Werror` in GCC/Clang).
*   **The Why:** Compiler warnings often point to real bugs or undefined behavior. Ignoring them is a recipe for disaster.

## Summary Table: The "Power of 10"
| Rule | Key Constraint | Goal |
|---|---|---|
| 1 | No Recursion/Goto | Predictable execution flow |
| 2 | Fixed Loops | Prevent infinite loops |
| 3 | No malloc() | Prevent memory leaks |
| 4 | Short Functions | Readability & Verification |
| 5 | High Assertion Density | Catch logic errors early |
| 6 | Small Scope | Data safety |
| 7 | Check Returns | Prevent silent failures |
| 8 | Limit Preprocessor | Code clarity |
| 9 | Limit Pointers | Memory safety |
| 10 | Zero Warnings | Code hygiene |

## 2024-05-24 - Single-Pass Recursion in Hot Paths
**Learning:** Chained list operations (like `zipWith`, `map`, `scanl`) in Haskell's performance paths resist GHC's stream fusion and cause unnecessary intermediate thunk allocations, slowing down signal processing.
**Action:** Favor manual, single-pass tail recursion or guarded recursion to avoid intermediate allocations and achieve massive speedups in list processing.

## 2024-05-24 - Scratch File Cleanup
**Learning:** Compiled executable binaries and unintegrated benchmark scratch files must not be committed to the root of the repository as they bloat history and create platform-dependent risks.
**Action:** Always clean up temporary `*.hs` test files and generated binaries before requesting code review or submitting code.

## 2024-05-25 - Strict Evaluation in Hot Loops
**Learning:** Even with single-pass tail-recursion, accumulating variables in hot mathematical loops (like CZT summation or EMA calculation) can build up deeply nested thunks if not evaluated strictly, causing space leaks and performance degradation.
**Action:** Use `BangPatterns` (`!`) on accumulating variables and intermediate calculation terms inside hot loops (e.g., `let !m = ...`) to force strict evaluation and prevent thunk buildup.

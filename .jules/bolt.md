## 2024-05-24 - Single-Pass Recursion in Hot Paths
**Learning:** Chained list operations (like `zipWith`, `map`, `scanl`) in Haskell's performance paths resist GHC's stream fusion and cause unnecessary intermediate thunk allocations, slowing down signal processing.
**Action:** Favor manual, single-pass tail recursion or guarded recursion to avoid intermediate allocations and achieve massive speedups in list processing.

## 2024-10-24 - The danger of Strict Tail-Recursion for Streaming Lists
**Learning:** While building lists using strict tail-recursion with an accumulator (e.g., `let !val = ... in go (val:acc) ...`) and calling `reverse` may seem faster in simple micro-benchmarks, it breaks Haskell's lazy streaming semantics. This turns an O(1) memory operation into an O(N) memory operation, forcing the entire list into memory before it can be consumed by the next pipeline stage. For large radar datasets, this causes memory spikes and ruins composability.
**Action:** For list processing, stick to guarded recursion (`val : go ...`) to maintain lazy streaming, but use strict evaluation for the elements themselves (e.g., `let !val = ... in val : go ...`) to prevent intermediate thunk buildup. This achieves a balance of performance without destroying O(1) memory streaming.

## 2024-05-24 - Scratch File Cleanup
**Learning:** Compiled executable binaries and unintegrated benchmark scratch files must not be committed to the root of the repository as they bloat history and create platform-dependent risks.
**Action:** Always clean up temporary `*.hs` test files and generated binaries before requesting code review or submitting code.

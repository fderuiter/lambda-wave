## 2024-05-24 - Single-Pass Recursion in Hot Paths
**Learning:** Chained list operations (like `zipWith`, `map`, `scanl`) in Haskell's performance paths resist GHC's stream fusion and cause unnecessary intermediate thunk allocations, slowing down signal processing.
**Action:** Favor manual, single-pass tail recursion or guarded recursion to avoid intermediate allocations and achieve massive speedups in list processing.

## 2024-10-24 - The danger of Strict Tail-Recursion for Streaming Lists
**Learning:** While building lists using strict tail-recursion with an accumulator (e.g., `let !val = ... in go (val:acc) ...`) and calling `reverse` may seem faster in simple micro-benchmarks, it breaks Haskell's lazy streaming semantics. This turns an O(1) memory operation into an O(N) memory operation, forcing the entire list into memory before it can be consumed by the next pipeline stage. For large radar datasets, this causes memory spikes and ruins composability.
**Action:** For list processing, stick to guarded recursion (`val : go ...`) to maintain lazy streaming, but use strict evaluation for the elements themselves (e.g., `let !val = ... in val : go ...`) to prevent intermediate thunk buildup. This achieves a balance of performance without destroying O(1) memory streaming.

## 2024-05-24 - Scratch File Cleanup
**Learning:** Compiled executable binaries and unintegrated benchmark scratch files must not be committed to the root of the repository as they bloat history and create platform-dependent risks.
**Action:** Always clean up temporary `*.hs` test files and generated binaries before requesting code review or submitting code.

## 2024-05-25 - Strict Evaluation in Hot Loops
**Learning:** Even with single-pass tail-recursion, accumulating variables in hot mathematical loops (like CZT summation or EMA calculation) can build up deeply nested thunks if not evaluated strictly, causing space leaks and performance degradation.
**Action:** Use `BangPatterns` (`!`) on accumulating variables and intermediate calculation terms inside hot loops (e.g., `let !m = ...`) to force strict evaluation and prevent thunk buildup.

## 2024-10-25 - Floating Point Exponentiation Overhead
**Learning:** Using `**` for floating point exponentiation (like `latencySec ** 2`) in Haskell evaluates to `exp(y * log(x))`, which carries significant unnecessary computational overhead when the power is a small integer, creating a bottleneck inside hot calculation loops (like `evaluateGating`).
**Action:** Replace `x ** 2` with `x * x` or `x ^ (2::Int)` in performance-critical signal processing loops for faster execution without type coercion.

## 2024-10-24 - [Avoid apt-get for Haskell Tools in Docker Containers]
**Learning:** Installing `ghc` and `cabal-install` via `apt-get` inside a modern Haskell Docker container (e.g., `haskell:9.8`) overwrites the container's up-to-date toolchain with severely outdated Debian packages (like GHC 8.8.4). This causes massive dependency bloat (pulling in packages like `python3.9`), drastically increases build times, and leads to network timeouts during CI/CD package fetching.
**Action:** Always rely on the base Docker image's provided Haskell toolchain and avoid including `ghc` or `cabal-install` in OS-level package installation scripts (`setup_env.sh`) when executing inside Haskell-specific containers.
## 2026-04-04 - [Optimize Power Operations in Haskell]
**Learning:** In performance-critical Haskell code, using the `^` or `**` operator for small integer powers (like squaring or cubing) introduces significant function overhead. Explicit multiplication is preferred.
**Action:** Always replace small integer exponentiation (e.g., `x^2`, `x^(4::Int)`) with explicit multiplication (`x * x`) or cached variables (`let x2 = x * x`) to allow the compiler to utilize fast hardware multiply operations.

## 2026-04-06 - [Avoid Redundant List Traversals in Wrappers]
**Learning:** When passing Haskell lists or matrices down a call chain, outer wrapper functions often redundantly evaluate `length x /= length y` or `length m == rows` even when the inner functions (like `leastSquares`) inherently and safely validate those exact dimensions. Because lists do not store their length, every `length` check forces a full O(N) traversal.
**Action:** Always verify if downstream functions already handle dimension or length validation. If so, remove redundant `length` checks from the caller to save unnecessary O(N) traversals.

## 2026-04-10 - [Avoid Native float2Double API Breakage]
**Learning:** Using `GHC.Float.double2Float` instead of `realToFrac` in OpenGL bindings breaks typeclass resolution (`No instance for (Vertex (Vertex3 Float))`) because `realToFrac` handles coercion to exact newtypes like `GLfloat` implicitly.
**Action:** Retain `realToFrac` in type-strict FFI bindings (like OpenGL vertices) where exact numeric type matching to C-level type aliases is required.

## 2024-10-25 - [Optimize Chirp Z-Transform (CZT) Inner Loop]
**Learning:** Evaluating trigonometric functions (`cos`, `sin` via `cis`) inside a hot accumulation loop for Chirp Z-Transform scales $O(N)$, causing high overhead per sample.
**Action:** Replace repeated $O(N)$ trigonometric evaluations (`cis (theta_step * n)`) inside hot loops with a constant phase multiplier (`let !w = cis theta_step`) calculated outside the loop. Update the accumulator via simple complex multiplication (`term * w`) at each step to avoid expensive `cos`/`sin` calls.

## 2026-04-10 - [Avoid O(N) evaluation on lazy ByteStrings length check]
**Learning:** When checking if a lazy ByteString has fewer than N bytes (e.g., BL.length bs < 8), avoid BL.length as it forces an O(N) evaluation of the entire chunk chain.
**Action:** Instead, use BL.length (BL.take N bs) < N to limit the check to at most N bytes, restoring O(1) streaming performance.

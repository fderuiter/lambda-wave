## 2026-04-30 - Unpacking Complex numbers in hot loops
**Learning:** Using `Complex Double` in tight recursive functions (like `chirpZTransform` summation) causes heavy intermediate memory allocations, bypassing stream fusion and generating `O(K * N)` heap objects.
**Action:** Unpack complex numbers into strictly evaluated `Double` arguments (`!accR !accI !termR !termI`) for real and imaginary parts. Compute intermediate products using explicit math to avoid `Complex` wrapper overhead, restoring tight `Double` unboxed operations.

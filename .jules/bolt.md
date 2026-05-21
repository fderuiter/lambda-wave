## 2024-05-21 - Unpacked Complex Double in Tight Recursive Loops
**Learning:** In Haskell, using `Complex Double` in tight recursive loops causes heavy intermediate memory allocations, bypassing stream fusion and generating `O(K * N)` heap objects.
**Action:** Unpack complex numbers into strictly evaluated `Double` arguments (`!accR`, `!accI`, `!termR`, `!termI`) and use explicit math to restore tight unboxed operations.

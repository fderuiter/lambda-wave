## 2025-05-22 - Cache floating-point divisions outside map loops in high-frequency signal processing
**Learning:** Inside `chirpZTransform`, repeatedly dividing by `k_max` and `fs` for every frequency bin (O(K)) incurs high floating-point latency overhead.
**Action:** Pre-calculate constant divisions like `b_zoom / k_max` and `-2 * pi / fs` outside of hot map closures (like `calculateBin`), converting them into much faster O(K) multiplications.

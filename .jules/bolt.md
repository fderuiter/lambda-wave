## 2024-05-24 - Pre-calculate floating-point divisions outside hot loops
**Learning:** In high-frequency signal processing loops (like `chirpZTransform` map closures), repeatedly performing floating-point divisions incurs high latency overhead.
**Action:** Pre-calculate these divisions (e.g., `b_zoom / k_max`) outside the hot loop or closure, and multiply by the result inside to optimize O(K) calculations.

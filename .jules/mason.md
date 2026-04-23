## 2026-04-23 - [SIMD Optimization Rejection]
**Context:** P2-005 tasks suggested introducing SIMD optimizations via `hmatrix` for `SignalProcessing.FMCW.hs` processing loops to reduce per-frame latency.
**Decision:** Maintained custom Zero-Allocation loops instead of introducing `hmatrix` to avoid supply chain risk and external C dependencies. Modified `unwrapPhase` specifically to eliminate floating point divisions inside the iteration by substituting with single cached multiplier `1.0 / (2*pi)`.
**Compliance Impact:** Satisfies IEC 62304 dependency constraints by strictly adhering to the "no new Cabal dependencies" boundaries, minimizing SOUP footprint.

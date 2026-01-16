# Mason's Journal - Architectural Decisions

## 2024-05-22 - Toolchain & RTS Locking
**Context:** GC pauses and thread scheduling jitter were introducing non-deterministic latency spikes, threatening the <15ms loop requirement.
**Decision:** Configure GHC RTS with `-N2` (minimum 2 cores) and `-qa` (affinity) to lock capabilities to OS threads and cores. Enforced via `cabal.project` and runtime checks in `Main.hs`.
**Compliance Impact:** Satisfies SR-SOUP-001 (SOUP Configuration & Control) by strictly defining the runtime environment parameters.

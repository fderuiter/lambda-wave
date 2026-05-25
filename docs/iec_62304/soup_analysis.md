# SOUP Analysis (GHC RTS)

**Software of Unknown Provenance (SOUP) Evaluation for IEC 62304**

**SOUP Item:** Glasgow Haskell Compiler Runtime System (GHC RTS)
**Version:** 9.6.7
**Purpose:** Provides memory management (Garbage Collection), thread scheduling, and runtime services for the Lambda-Wave application.

## 1. Description and Purpose
The GHC RTS is a core component required to execute compiled Haskell code. It handles:
- Memory allocation and Garbage Collection (GC)
- Green thread (lightweight thread) scheduling
- Foreign Function Interface (FFI) boundaries
- Exception handling and software transactional memory (STM)

## 2. Intended Use Context
Lambda-Wave is an IEC 62304 Class C Medical Software system. The GHC RTS runs in the critical path of the radar data ingestion, signal processing, and gating control loop. The loop has a hard real-time latency constraint of <15ms.

## 3. Potential Hazards & Risk Control Measures

| Hazard ID | Description | Potential Cause | Risk Control Measure | Verification |
|-----------|-------------|-----------------|----------------------|--------------|
| H-SOUP-001 | Unbounded GC Pauses | Garbage collector pauses execution of the gating thread, exceeding the 15ms latency constraint. | Use of `-O2` compilation, `-qa` and `-N2` RTS flags to lock capabilities. Zero-allocation loop design in hot paths. | `bench/LatencyBench.hs` running under `+RTS -s` and verifying max pause <5ms. |
| H-SOUP-002 | Thread Starvation | The RTS scheduler fails to wake up the gating thread in time. | Pinning critical threads, using `System.Posix` for high-priority IO, and implementing a 100ms hardware watchdog. | `test/Safety/WatchdogSpec.hs` and `test/System/RTSSpec.hs` thread priority tests. |
| H-SOUP-003 | FFI Memory Leaks | Memory allocated in C++ ring buffer is not tracked by GHC GC and leaks. | Strict manual memory management in `cbits/src/ring_buffer.cpp`. Use of Valgrind. | `FFI.RingBuffer.IOSpec` unit tests with Valgrind memory check. |
| H-SOUP-004 | Deadlocks | STM or MVar deadlocks within the RTS locking mechanism. | Preference for `STM` over `MVar`. Strict architectural rule against unbounded STM retries. Watchdog termination. | Code review, hlint, and `test/Safety/WatchdogSpec.hs`. |

## 4. Anomaly List Assessment
GHC 9.6.7 is a mature release. Known issues in the GHC issue tracker have been reviewed. No open bugs related to bounded GC or core thread scheduling were identified that affect the specific subset of features used by Lambda-Wave.

## 5. Justification for Acceptability
The GHC RTS is deemed acceptable for use in this Class C system because:
1. The project architecture relies on a minimal subset of RTS features.
2. The "hot path" processing avoids heap allocations, neutralizing the primary source of RTS non-determinism (Garbage Collection).
3. The system implements an independent, external safety watchdog to detect and mitigate any complete RTS failures or lockups.
4. GHC 9.6.7 has been heavily battle-tested in industrial contexts, and the specific RTS flags (`-N2 -qa -l`) provide predictable affinity behavior.

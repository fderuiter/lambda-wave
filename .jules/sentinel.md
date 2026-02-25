# Sentinel Journal

## 2024-05-23 - [Critical] **Vector:** src/Control/Gating.hs **Hazard:** Race Condition (TOCTOU)

**Description:**
The `processFrame` function read `oldSystemState` (Kalman Filter state) outside the atomic transaction, calculated the new state, and then wrote it back inside an `atomically` block. If the system state (e.g., beam hold or filter reset) was modified by another thread (UI) between the read and the write, the concurrent update would be lost/overwritten.

**Fix:**
Refactored `processFrame` to perform the read-calculate-write sequence within a single STM transaction. IO actions (`setBeam`, `time`) were moved outside the transaction.

## 2024-05-23 - [High] **Vector:** src/FFI/RingBuffer/Types.hs **Hazard:** Memory Corruption (Atomic Violation)

**Description:**
The `RingBufferControl` struct contains C++ `std::atomic<size_t>` fields. The Haskell `Storable` instance provided a `poke` implementation that performed a byte-wise overwrite of these fields. If `poke` were used on an initialized structure, it would corrupt the internal state of the atomics (violating the C++ memory model).

**Fix:**
Removed the `Storable` instance entirely. Safe access is now restricted to `peekStaticFields` (for immutable fields) and dedicated FFI functions in `FFI.RingBuffer.IO`.

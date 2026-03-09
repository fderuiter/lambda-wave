# Sentinel's Journal

## 2025-05-18 - [High] **Vector:** `FFI/RingBuffer/Types.hs` **Hazard:** Race Condition on Atomic Fields
Found a `Storable` instance for `RingBufferControl` that exposes atomic fields (`writeOffset`, `readOffset`) via non-atomic `peek` and `poke`. This creates a Time-of-Check to Time-of-Use (TOCTOU) race condition where a consumer thread might read a partially updated 64-bit value on 32-bit systems or observe inconsistent ordering. The `Storable` instance has been removed from production code and restricted to tests only.

## 2025-05-18 - [Medium] **Vector:** `FFI/RingBuffer/IO.hs` **Hazard:** Buffer Overflow via Negative Offset
The function `setReadOffset` accepted a signed `Int` and cast it to `size_t` (unsigned) without validation. A negative `Int` would become a huge `size_t`, causing the C++ driver to calculate an invalid buffer size and potentially trigger a buffer overflow or read out of bounds. Added a runtime check `off >= 0`.

## 2025-05-18 - [High] **Vector:** `FFI/RingBuffer/IO.hs` **Hazard:** Buffer Overflow via Out-of-Bounds Offset
The function `setReadOffset` trusted the consumer provided offset without validating it against the actual buffer size. If a compromised consumer provided an offset >= bufferSize, the C++ driver would accept it, leading to future reads/writes outside the allocated memory region. Added a `peekStaticFields` check to verify `off < bufferSize`.

## 2025-05-18 - [Medium] **Vector:** `Hardware/Control.hs` **Hazard:** Denial of Service via Config File
The `configureSensor` function used `hGetContents` (lazy IO) on the configuration file. A malicious actor could provide a massive file (e.g. `/dev/zero` or a large generated file), causing the Haskell runtime to exhaust memory (OOM) or hang the thread. Replaced with `B.hGet` and a 100KB limit.

## 2025-05-18 - [High] **Vector:** `Control/Gating.hs` **Hazard:** TOCTOU Race Condition in Gating Logic / Kalman Filter Update
The `processFrame` function read the system state using `readTVarIO` outside of an `atomically` transaction, calculated the new Kalman state and dt based on that read, and then wrote the updated state inside a new `atomically` block. This is a Time-of-Check to Time-of-Use (TOCTOU) race condition that can corrupt the Kalman filter state if another thread modifies the system state concurrently. Moved the `readTVar` and all related calculations inside the `atomically` transaction.

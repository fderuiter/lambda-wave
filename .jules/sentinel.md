# Sentinel's Journal

## 2025-05-18 - [High] **Vector:** `FFI/RingBuffer/Types.hs` **Hazard:** Race Condition on Atomic Fields
Found a `Storable` instance for `RingBufferControl` that exposes atomic fields (`writeOffset`, `readOffset`) via non-atomic `peek` and `poke`. This creates a Time-of-Check to Time-of-Use (TOCTOU) race condition where a consumer thread might read a partially updated 64-bit value on 32-bit systems or observe inconsistent ordering. The `Storable` instance has been removed from production code and restricted to tests only.

## 2025-05-18 - [Medium] **Vector:** `FFI/RingBuffer/IO.hs` **Hazard:** Buffer Overflow via Negative Offset
The function `setReadOffset` accepted a signed `Int` and cast it to `size_t` (unsigned) without validation. A negative `Int` would become a huge `size_t`, causing the C++ driver to calculate an invalid buffer size and potentially trigger a buffer overflow or read out of bounds. Added a runtime check `off >= 0`.

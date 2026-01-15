## 2024-05-23 - [Risk Level: CRITICAL] **Vector:** src/FFI/RingBuffer/IO.hs **Hazard:** Null Pointer Dereference
The `createRingBuffer` function calls C++ `create_ring_buffer` which returns `nullptr` on allocation failure (e.g. `posix_memalign` fails). The Haskell wrapper does not check for `nullPtr`. If this occurs, `bracket` will pass `nullPtr` to `freeRingBuffer` (safe) and to the user action (unsafe). A user action like `peek` or `ingestionLoop` will dereference 0x0, causing a Segfault.

## 2024-05-23 - [Risk Level: HIGH] **Vector:** cbits/src/ring_buffer.cpp **Hazard:** Data Race / Memory Corruption
The C++ producer `read_from_uart` does not check `read_offset`. It assumes infinite space and overwrites old data circularly. The Haskell consumer uses `zero-copy` via `ForeignPtr` to alias this memory. If the consumer is parsing a chunk (`Data.Binary.Get` on `LazyByteString`) and the producer overwrites that memory block because the ring wrapped around, the consumer will read corrupted data. This is a classic reader-writer race condition.

## 2024-05-24 - [Risk Level: HIGH] **Vector:** src/Hardware/Consumer.hs **Hazard:** Lazy Consumption Race
The Consumer constructed a Lazy ByteString from the FFI Ring Buffer pointer, parsed it into a lazy list of `RadarFrame`, and updated the `read_offset` *before* fully evaluating the data. Because Haskell is lazy, the parsing could happen *after* `read_offset` was updated. If the C++ thread overwrote the buffer (now marked as "read") before Haskell finished parsing, we would read corrupted data.
**Fix:** Introduced `Control.DeepSeq.force` and `Control.Exception.evaluate` to ensure all parsed data is fully copied out of the C memory buffer into Haskell heap *before* signaling the producer that the buffer is free. Added `NFData` instances to `SystemState` and `Point3D` to support this.

## 2024-05-24 - [Risk Level: MEDIUM] **Vector:** src/SignalProcessing/Regression.hs **Hazard:** Partial Function
Found usage of `error` in `predict` function which could crash the runtime if coefficient vector length was unexpected.
**Fix:** Replaced with safe fallback (returning 0.0).

## 2024-05-25 - [Risk Level: LOW] **Vector:** src/FFI/RingBuffer/IO.hs **Hazard:** Ignored Return Code
The `ingestionLoop` caught negative return codes from `read_from_uart` (indicating error) but silently terminated the thread. This violates the "Never ignore return codes" directive and hides hardware failures.
**Fix:** Added logging to stderr before termination.

## 2024-05-25 - [Risk Level: MEDIUM] **Vector:** src/Hardware/Consumer.hs **Hazard:** Performance/DoS
The `findMagicWord` function used a byte-by-byte scan with `G.lookAhead` and `G.skip 1`, which is highly inefficient and could lead to CPU saturation (DoS) under high load or noise.
**Fix:** Implemented an optimized search using `ByteString.break` to scan for the magic word header, reducing overhead significantly.

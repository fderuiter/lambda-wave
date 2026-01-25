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

## 2024-05-25 - [Risk Level: HIGH] **Vector:** src/Hardware/Consumer.hs **Hazard:** Garbage Scan Livelock
The parser used a byte-by-byte `Data.Binary.Get` loop (`findMagicWord`) to search for the Magic Word. If the buffer contained significant garbage (e.g., lost sync), this would trigger a `Partial` result from the decoder without advancing the consumer state, causing the `consumerLoop` to read the same garbage repeatedly in a tight loop (Livelock/High CPU).
**Fix:** Implemented `skipToMagicWord` using `ByteString.elemIndex` to efficiently skip garbage bytes before invoking the parser. Updated `parseStream` to correctly report skipped bytes as consumed, allowing `consumerLoop` to advance the Ring Buffer `read_offset` past the garbage.

## 2024-05-25 - [Risk Level: MEDIUM] **Vector:** src/SignalProcessing/Regression.hs **Hazard:** Runtime Exception (Partial Function)
The functions `solveBiQuadratic` and `solveStrictBiQuadratic` used `hmatrix`'s `<\>` operator (Least Squares) which throws a runtime exception if the dimensions of inputs `x` and `y` mismatch.
**Fix:** Added guard clauses to verify `size x == size y`. Returns a zero-coefficient vector as a safe fallback if dimensions mismatch, converting a potential crash into a handled failure state.

## 2026-05-27 - [Risk Level: MEDIUM] **Vector:** src/Hardware/Control.hs **Hazard:** Unchecked IO / Return Code
The `configureSensor` function ignored the return value of `send`, potentially assuming a command was fully sent when it was partial or failed. It also did not catch `IOException` from `openSerial`, which could crash the runtime if the port was missing.
**Fix:** Wrapped `openSerial` and `send` in `try` block. Implemented a check for `bytesSent < length packet`. Changed return type to `IO (Either String ())` to force error handling in caller.

## 2026-05-28 - [Risk Level: HIGH] **Vector:** src/Hardware/Consumer.hs **Hazard:** TLV Alignment Corruption
The TLV parser assumed that the payload length was always exactly `numPoints * 16`, failing to account for padding bytes required for 32-bit alignment of the next TLV. This caused the parser to read padding bytes as the subsequent TLV's header (Type), leading to stream corruption or misinterpretation of valid data.
**Fix:** Modified `parseTLVs` to correctly calculate padding bytes based on the TLV length field (assuming Total Length per protocol analysis) and the number of bytes actually consumed by points. Used `G.skip` to advance the parser past any padding, ensuring correct alignment for the next TLV.

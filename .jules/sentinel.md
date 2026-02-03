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

## 2024-05-28 - [Risk Level: MEDIUM] **Vector:** src/Control/Gating.hs **Hazard:** Thunk Leak
The `processFrame` function used `sum (map pz pts)` to calculate the average height. For large point clouds, `map` constructs a list of thunks which remains in memory until `sum` forces it. This causes short-term GC pressure and potential space leaks.
**Fix:** Refactored to use a strict left fold (`foldl'`) to compute sum and count in a single pass without intermediate list allocation.

## 2024-05-28 - [Risk Level: HIGH] **Vector:** src/Hardware/Consumer.hs **Hazard:** Data Corruption / Stream Desynchronization
The `parseTLVs` function assumed `tlvLen` perfectly matched the size of the point payload (16 bytes per point). If the sensor sent padding bytes or the `tlvLen` included headers differently than expected, the parser would not consume the extra bytes. This would leave the stream pointer misaligned for the next TLV or frame, causing the parser to interpret garbage as headers.
**Fix:** Updated `parseTLVs` to calculate the actual bytes read and explicitly `G.skip` any remaining bytes defined by `tlvLen`.

## 2026-05-29 - [Risk Level: HIGH] **Vector:** app/Main.hs **Hazard:** Data Corruption (Canonical Mode)
The Data Port (`sensorPort`) is opened via `openFd` but never configured to Raw Mode. `openFd` does not modify terminal attributes. If the system defaults to Canonical Mode (`ICANON`), the `read` syscall in `ring_buffer.cpp` will wait for newlines (`0x0A`) and potentially interpret control characters, corrupting the binary radar stream.
**Fix:** Implement `configureRawSerial` in `Hardware/Control` using `System.Posix.Terminal` to disable `ICANON`, `ECHO`, `ISIG` and set correct Baud Rate (921600). Invoke this on the `Fd` in `Main.hs`.

## 2026-05-29 - [Risk Level: HIGH] **Vector:** src/Hardware/Consumer.hs **Hazard:** Logic Bypass / Watchdog Starvation
The `consumerLoop` directly updates `SystemState` via `atomically` without invoking `Control.Gating.processFrame`. This means the Kalman filter, beam gating logic, and most importantly the Watchdog Heartbeat (`lastFrameTime` update) are completely bypassed. This would cause the `Safety.Watchdog` to detect a "hang" and terminate the application shortly after startup.
**Fix:** Import `Control.Gating (processFrame)` and invoke it with the accumulated points from the parsed frames instead of modifying the TVar directly.

## 2026-05-29 - [Risk Level: HIGH] **Vector:** sgrt-radar-system.cabal **Hazard:** Dependency Failure / Build Breakage
The project depends on `hmatrix`, `vector`, `serialport`, and `OpenGL`/`GLUT`, which are external C-bound libraries not available in the high-security build environment. This prevents compilation (`cabal build` fails), making it impossible to verify safety or run tests.
**Fix:** Implement `src/Numeric/Simple.hs` (Zero-Dependency Linear Algebra) and refactor `Control/Mesher`, `SignalProcessing/FMCW`, `SignalProcessing/Regression` to use it. Replace `serialport` with `unix`. Stub out `Control/UI`. Remove faulty dependencies from `.cabal`.

## 2026-05-29 - [Risk Level: MEDIUM] **Vector:** src/Hardware/Control.hs **Hazard:** Unapproved Dependency
The `configureSensor` function relies on `System.Hardware.Serialport` which is not whitelisted.
**Fix:** Refactor to use `System.Posix.IO` (`openFd`, `fdWriteBuf`) and `System.Posix.Terminal` to configure the CLI port, matching the approved pattern used in `configureRawSerial`.

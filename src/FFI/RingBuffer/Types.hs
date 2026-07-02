{-|
Module: FFI.RingBuffer.Types

FFI bindings for the ring buffer control structure shared between C++
and Haskell.

This module defines 'RingBufferControl', a Haskell-side representation of
the control block that is allocated and owned on the C++ side
(see @RingBuffer.h@). The layout and size of this structure must remain
exactly in sync with the corresponding C++ definition in order to
preserve the Application Binary Interface (ABI) between the two
languages.

==== Memory layout

The layout matches the C++ @struct RingBufferControl@ across 32-bit and
64-bit architectures:

* Total size: 64 bytes (due to alignas(64))
* Alignment: 64 bytes
* Field offsets depend on @size_t@ and pointer size:
    * @writeOffset :: CSize@ at offset 0
    * @readOffset  :: CSize@ at offset @sizeof(size_t)@
    * @bufferStart :: Ptr CChar@ at offset @2 * sizeof(size_t)@
    * @bufferSize  :: CSize@ at offset @2 * sizeof(size_t) + sizeof(void*)@

Any padding between fields and up to the full 64-byte size is owned by
the C++ side.

Note: 'Storable' is intentionally /not/ defined in this module to prevent
accidental non-atomic access to the atomic fields. Layout verification is
performed via an orphan 'Storable' instance in the test suite
(@test\/FFI\/RingBuffer\/TypesSpec.hs@).

==== Concurrency and safety

This structure is typically accessed concurrently by C++ and Haskell

side). In particular:

* 'writeOffset' is expected to be updated atomically on the C++ side
  (e.g. as an atomic @size_t@). Haskell code must treat it as a
  concurrently-modified variable and must follow the memory ordering and
  synchronization protocol defined in @RingBuffer.h@.
* 'bufferStart' and 'bufferSize' are usually initialized once on the
  C++ side and then treated as read-only by Haskell.

Because this is used within a safety-critical medical device system,
any change to the fields, their types, or their layout must be
carefully reviewed, synchronized with the C++ definition, and
re-tested. Incorrect assumptions about concurrent access or memory
layout can lead to data corruption and undefined behaviour.

See also: @RingBuffer.h@ for the authoritative C++ definition and
documentation of the ring buffer control structure and protocol.
-}
module FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields, getBufferSize) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable
import FFI.RingBuffer.Generated

-- We compute the dynamic start pointer from the offset
peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
peekStaticFields ptr = do
    -- offset is at byte 16 (since atomic_size_t is 8 bytes each)
    let offsetPtr = ptr `plusPtr` (2 * sizeOf (0 :: CSize))
    let sizePtr = ptr `plusPtr` (3 * sizeOf (0 :: CSize))
    offset <- peek (castPtr offsetPtr :: Ptr CSize)
    sz <- peek (castPtr sizePtr :: Ptr CSize)
    let start = ptr `plusPtr` fromIntegral offset
    return (start, sz)

getBufferSize :: ForeignPtr RingBufferControl -> IO Int
getBufferSize fp = withForeignPtr fp $ \ptr -> do
    (_, sz) <- peekStaticFields ptr
    return (fromIntegral sz)

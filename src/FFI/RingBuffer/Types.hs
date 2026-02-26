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

The 'Storable' instance below dynamically calculates the layout to match
the C++ @struct RingBufferControl@ across 32-bit and 64-bit architectures:

* Total size: 64 bytes (due to alignas(64))
* Alignment: 64 bytes
* Field offsets depend on `size_t` and pointer size:
    * @writeOffset :: CSize@ at offset 0
    * @readOffset  :: CSize@ at offset `sizeof(size_t)`
    * @bufferStart :: Ptr CChar@ at offset `2 * sizeof(size_t)`
    * @bufferSize  :: CSize@ at offset `2 * sizeof(size_t) + sizeof(void*)`

Any padding between fields and up to the full 64-byte size is owned by
the C++ side.

==== Concurrency and safety

This structure is typically accessed concurrently by C++ and Haskell
code (e.g. a producer on the C++ side and a consumer on the Haskell
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
module FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

-- | Haskell view of the C++ ring buffer control block.
--
-- Note: On the C++ side, @writeOffset@ is a @std::atomic<size_t>@.
-- This Haskell representation uses 'CSize' to match the platform-specific
-- size of @size_t@ (32-bit or 64-bit).
--
-- As a result, this type and its 'Storable' instance must /not/ be used for
-- concurrent access to @writeOffset@. All atomic operations on that field
-- must be performed through dedicated FFI functions that implement the
-- required atomic semantics. The 'Storable' instance is intended only for
-- layout-compatible, non-concurrent inspection/initialisation of the struct.
data RingBufferControl = RingBufferControl
    { writeOffset :: !CSize      -- ^ Corresponds to std::atomic<size_t>
    , readOffset  :: !CSize      -- ^ Corresponds to std::atomic<size_t>
    , bufferStart :: !(Ptr CChar)   -- ^ Start of the data buffer.
    , bufferSize  :: !CSize      -- ^ size_t; buffer capacity in bytes (non-atomic).
    } deriving (Show, Eq)

-- SENTINEL SAFETY EDIT: Storable instance removed to prevent race conditions.
-- Access to atomic fields (writeOffset, readOffset) via peek/poke is unsafe.
-- Use peekStaticFields for safe read-only access to constant fields.
-- Layout verification is performed in test/FFI/RingBuffer/TypesSpec.hs via an orphan instance.

-- | Peeks only the static fields (bufferStart and bufferSize) from the control block.
-- This avoids reading the atomic offsets (0 and 8/4) which are modified concurrently by C++,
-- preventing potential data races (Undefined Behavior) when accessing the control block
-- from the consumer thread.
peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
peekStaticFields ptr = do
    let sizeT = sizeOf (undefined :: CSize)
        readOff = sizeT
        startOff = readOff + sizeT
        sizeOff = startOff + sizeOf (undefined :: Ptr CChar)

    start <- peekByteOff ptr startOff
    sz    <- peekByteOff ptr sizeOff
    return (start, sz)

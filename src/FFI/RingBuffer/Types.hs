{-# LANGUAGE ForeignFunctionInterface #-}

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

The 'Storable' instance below hard-codes the following layout, which
must match the C++ @struct RingBufferControl@:

* Total size: 64 bytes
* Alignment: 64 bytes
* Field offsets (in bytes from the start of the struct):

    * @writeOffset :: Word64@ at offset 0
    * @bufferStart :: Ptr CChar@ at offset 8
    * @bufferSize  :: Word64@ at offset 16

Any padding between fields and up to the full 64-byte size is owned by
the C++ side. Do not change 'sizeOf', 'alignment', or the offsets in
'peek'/'poke' without making corresponding, coordinated changes in
@RingBuffer.h@ and re-validating the ABI.

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
module FFI.RingBuffer.Types where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Word

-- | Haskell view of the C++ ring buffer control block.
--
-- Note: On the C++ side, @writeOffset@ is a @std::atomic<size_t>@.
-- This Haskell representation uses a plain 'Word64' and the 'Storable'
-- instance below performs ordinary loads and stores (via 'peekByteOff' and
-- 'pokeByteOff') with no atomic or memory-ordering guarantees.
--
-- As a result, this type and its 'Storable' instance must /not/ be used for
-- concurrent access to @writeOffset@. All atomic operations on that field
-- must be performed through dedicated FFI functions that implement the
-- required atomic semantics. The 'Storable' instance is intended only for
-- layout-compatible, non-concurrent inspection/initialisation of the struct.
data RingBufferControl = RingBufferControl
    { writeOffset :: Word64      -- ^ Corresponds to std::atomic<size_t> (non-atomic here; do NOT access concurrently via 'Storable').
    , bufferStart :: Ptr CChar   -- ^ Start of the data buffer.
    , bufferSize  :: Word64      -- ^ size_t; buffer capacity in bytes (non-atomic).
    } deriving (Show, Eq)

instance Storable RingBufferControl where
    sizeOf _ = 64
    alignment _ = 64
    peek ptr = do
        off <- peekByteOff ptr 0
        start <- peekByteOff ptr 8
        sz <- peekByteOff ptr 16
        return $ RingBufferControl off start sz
    poke ptr (RingBufferControl off start sz) = do
        pokeByteOff ptr 0 off
        pokeByteOff ptr 8 start
        pokeByteOff ptr 16 sz

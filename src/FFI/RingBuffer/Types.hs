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

The 'Storable' instance below calculates the layout dynamically based on
host platform's 'CSize' and pointer sizes/alignments. This ensures
compatibility with both 32-bit and 64-bit systems, matching standard C
structure packing rules.

* Fields:
    1. @writeOffset :: CSize@ (std::atomic<size_t>)
    2. @readOffset  :: CSize@ (std::atomic<size_t>)
    3. @bufferStart :: Ptr CChar@ (char*)
    4. @bufferSize  :: CSize@ (size_t)

Any padding between fields is handled automatically.

==== Concurrency and safety

This structure is typically accessed concurrently by C++ and Haskell
code. 'writeOffset' is expected to be updated atomically on the C++ side.
This Haskell representation uses 'CSize' and standard 'Storable' loads,
which provides no atomic guarantees. Atomic access must use dedicated FFI functions.
-}
module FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

-- | Haskell view of the C++ ring buffer control block.
data RingBufferControl = RingBufferControl
    { writeOffset :: !CSize      -- ^ Corresponds to std::atomic<size_t>
    , readOffset  :: !CSize      -- ^ Corresponds to std::atomic<size_t>
    , bufferStart :: !(Ptr CChar)   -- ^ Start of the data buffer.
    , bufferSize  :: !CSize      -- ^ size_t; buffer capacity in bytes (non-atomic).
    } deriving (Show, Eq)

instance Storable RingBufferControl where
    alignment _ = maximum [alignment (undefined :: CSize), alignment (undefined :: Ptr CChar)]
    sizeOf _ =
        let sSize = sizeOf (undefined :: CSize)
            aSize = alignment (undefined :: CSize)
            sPtr  = sizeOf (undefined :: Ptr CChar)
            aPtr  = alignment (undefined :: Ptr CChar)

            -- Offset 0: writeOffset (CSize)
            -- Offset 1: readOffset (CSize)
            off1 = (0 + sSize + aSize - 1) `div` aSize * aSize
            -- Offset 2: bufferStart (Ptr)
            off2 = (off1 + sSize + aPtr - 1) `div` aPtr * aPtr
            -- Offset 3: bufferSize (CSize)
            off3 = (off2 + sPtr + aSize - 1) `div` aSize * aSize

            end  = off3 + sSize
            totalAlign = alignment (undefined :: RingBufferControl)
        in (end + totalAlign - 1) `div` totalAlign * totalAlign

    peek ptr = do
        let sSize = sizeOf (undefined :: CSize)
            aSize = alignment (undefined :: CSize)
            sPtr  = sizeOf (undefined :: Ptr CChar)
            aPtr  = alignment (undefined :: Ptr CChar)

            off1 = (sSize + aSize - 1) `div` aSize * aSize
            off2 = (off1 + sSize + aPtr - 1) `div` aPtr * aPtr
            off3 = (off2 + sPtr + aSize - 1) `div` aSize * aSize

        woff <- peekByteOff ptr 0
        roff <- peekByteOff ptr off1
        start <- peekByteOff ptr off2
        sz <- peekByteOff ptr off3
        return $ RingBufferControl woff roff start sz

    poke ptr (RingBufferControl woff roff start sz) = do
        let sSize = sizeOf (undefined :: CSize)
            aSize = alignment (undefined :: CSize)
            sPtr  = sizeOf (undefined :: Ptr CChar)
            aPtr  = alignment (undefined :: Ptr CChar)

            off1 = (sSize + aSize - 1) `div` aSize * aSize
            off2 = (off1 + sSize + aPtr - 1) `div` aPtr * aPtr
            off3 = (off2 + sPtr + aSize - 1) `div` aSize * aSize

        pokeByteOff ptr 0 woff
        pokeByteOff ptr off1 roff
        pokeByteOff ptr off2 start
        pokeByteOff ptr off3 sz

-- | Peeks only the static fields (bufferStart and bufferSize) from the control block.
-- This avoids reading the atomic offsets (0 and 8/4) which are modified concurrently by C++.
peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
peekStaticFields ptr = do
    let sSize = sizeOf (undefined :: CSize)
        aSize = alignment (undefined :: CSize)
        sPtr  = sizeOf (undefined :: Ptr CChar)
        aPtr  = alignment (undefined :: Ptr CChar)

        off1 = (sSize + aSize - 1) `div` aSize * aSize
        off2 = (off1 + sSize + aPtr - 1) `div` aPtr * aPtr
        off3 = (off2 + sPtr + aSize - 1) `div` aSize * aSize

    start <- peekByteOff ptr off2
    sz    <- peekByteOff ptr off3
    return (start, sz)

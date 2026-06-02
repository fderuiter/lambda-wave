{-|
Module: FFI.RingBuffer.Types

FFI bindings for the ring buffer control structure shared between C++
and Haskell.
-}
module FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Word (Word32)
import Foreign.Marshal.Alloc (alloca)

-- | Haskell view of the C++ ring buffer control block.
data RingBufferControl = RingBufferControl
    { state0 :: !Word32, state1 :: !Word32, state2 :: !Word32, state3 :: !Word32
    , written0 :: !Word32, written1 :: !Word32, written2 :: !Word32, written3 :: !Word32
    , bufferStart :: !(Ptr CChar)
    , bufferSize  :: !CSize
    , currentWriteBlock :: !CSize
    , currentWriteOffset :: !CSize
    } deriving (Show, Eq)

foreign import ccall unsafe "get_buffer_pointers"
    c_get_buffer_pointers :: Ptr RingBufferControl -> Ptr (Ptr CChar) -> Ptr CSize -> IO ()

peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
peekStaticFields ptr = do
    alloca $ \bufStartPtr -> do
        alloca $ \sizePtr -> do
            c_get_buffer_pointers ptr bufStartPtr sizePtr
            start <- peek bufStartPtr
            sz <- peek sizePtr
            return (start, sz)

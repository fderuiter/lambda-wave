{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.RingBuffer.Types where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Word

data RingBufferControl = RingBufferControl
    { writeOffset :: Word64  -- atomic size_t
    , bufferStart :: Ptr CChar
    , bufferSize  :: Word64  -- size_t
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

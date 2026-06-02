module FFI.RingBuffer.Generated (RingBufferControl(..), c_get_write_offset, c_set_write_offset, c_get_read_offset, c_set_read_offset) where

import Foreign.C.Types
import Foreign.Ptr

data RingBufferControl = RingBufferControl
    { writeOffset :: !CSize
    , readOffset :: !CSize
    , bufferOffset :: !CSize
    , bufferSize :: !CSize
    } deriving (Show, Eq)

foreign import ccall unsafe "get_write_offset"
    c_get_write_offset :: Ptr RingBufferControl -> IO CSize
foreign import ccall unsafe "set_write_offset"
    c_set_write_offset :: Ptr RingBufferControl -> CSize -> IO ()

foreign import ccall unsafe "get_read_offset"
    c_get_read_offset :: Ptr RingBufferControl -> IO CSize
foreign import ccall unsafe "set_read_offset"
    c_set_read_offset :: Ptr RingBufferControl -> CSize -> IO ()


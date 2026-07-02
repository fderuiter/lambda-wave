module FFI.RingBuffer.Generated (bufferGap, RingBufferControl(..), c_get_write_offset, c_set_write_offset, c_get_read_offset, c_set_read_offset, c_calculate_available_read_bytes, c_calculate_next_read_offset) where

import Foreign.C.Types
import Foreign.Ptr

bufferGap :: Int
bufferGap = 1

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

foreign import ccall unsafe "calculate_available_read_bytes"
    c_calculate_available_read_bytes :: CSize -> CSize -> CSize -> IO CSize

foreign import ccall unsafe "calculate_next_read_offset"
    c_calculate_next_read_offset :: CSize -> CSize -> CSize -> IO CSize


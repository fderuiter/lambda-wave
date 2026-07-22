module FFI.RingBuffer.Generated (RingBufferControl (..), c_get_write_offset, c_set_write_offset, c_get_read_offset, c_set_read_offset, c_rb_available_data, c_rb_next_read_offset, ringBufferGap) where

import Foreign.C.Types
import Foreign.Ptr

ringBufferGap :: CSize
ringBufferGap = 1

data RingBufferControl = RingBufferControl
  { writeOffset :: !CSize,
    readOffset :: !CSize,
    bufferOffset :: !CSize,
    bufferSize :: !CSize
  }
  deriving (Show, Eq)

foreign import ccall unsafe "get_write_offset"
  c_get_write_offset :: Ptr RingBufferControl -> IO CSize

foreign import ccall unsafe "set_write_offset"
  c_set_write_offset :: Ptr RingBufferControl -> CSize -> IO ()

foreign import ccall unsafe "get_read_offset"
  c_get_read_offset :: Ptr RingBufferControl -> IO CSize

foreign import ccall unsafe "set_read_offset"
  c_set_read_offset :: Ptr RingBufferControl -> CSize -> IO ()

foreign import ccall unsafe "rb_available_data"
  c_rb_available_data :: Ptr RingBufferControl -> CSize -> IO CSize

foreign import ccall unsafe "rb_next_read_offset"
  c_rb_next_read_offset :: Ptr RingBufferControl -> CSize -> CSize -> IO CSize

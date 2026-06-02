module FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import FFI.RingBuffer.Generated

-- We compute the dynamic start pointer from the offset
peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
peekStaticFields ptr = do
    -- offset is at byte 16 (since atomic_size_t is 8 bytes each)
    let offsetPtr = ptr `plusPtr` (2 * sizeOf (undefined :: CSize))
    let sizePtr = ptr `plusPtr` (3 * sizeOf (undefined :: CSize))
    offset <- peek (castPtr offsetPtr :: Ptr CSize)
    sz <- peek (castPtr sizePtr :: Ptr CSize)
    let start = ptr `plusPtr` fromIntegral offset
    return (start, sz)

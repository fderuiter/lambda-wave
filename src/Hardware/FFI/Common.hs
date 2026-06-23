module Hardware.FFI.Common (
    HardwareResult(..),
    toHardwareResult,
    toRingBufferResult,
    
    -- Ring Buffer FFI
    c_create_ring_buffer,
    c_attach_ring_buffer,
    c_free_ring_buffer_ptr,
    c_detach_ring_buffer_ptr,
    c_free_ring_buffer_direct,
    c_detach_ring_buffer_direct,
    c_read_from_uart,
    
    -- GPIO FFI
    c_gpio_init,
    c_gpio_write,
    c_gpio_read,
    c_gpio_setup_watchdog,
    
    -- Serial FFI
    c_configure_serial_port,

    -- Lifecycle functions
    allocateManagedResource
) where

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import System.Posix.Types (CSsize(..))
import Control.Exception (throwIO, onException, mask_)
import FFI.RingBuffer.Generated (RingBufferControl)

data HardwareResult
    = Success
    | PartialData Int
    | Busy
    | EOF
    | Failure String
    | PosixError
    | InvalidConfiguration
    deriving (Show, Eq)

toHardwareResult :: CInt -> HardwareResult
toHardwareResult 0 = Success
toHardwareResult (-1) = PosixError
toHardwareResult (-2) = InvalidConfiguration
toHardwareResult n 
    | n > 0 = PartialData (fromIntegral n)
    | otherwise = Failure ("Unknown failure code: " ++ show n)

toRingBufferResult :: CSsize -> HardwareResult
toRingBufferResult n
    | n > 0 = PartialData (fromIntegral n)
toRingBufferResult 0 = Busy
toRingBufferResult (-1) = PosixError
toRingBufferResult (-2) = EOF
toRingBufferResult (-3) = Busy
toRingBufferResult n = Failure ("Unknown ring buffer code: " ++ show n)

-- | Generic resource allocator
allocateManagedResource :: IO (Ptr a) -> FunPtr (Ptr a -> IO ()) -> (Ptr a -> IO ()) -> String -> IO (ForeignPtr a)
allocateManagedResource alloc finalizer onErr errName = mask_ $ do
    ptr <- alloc
    if ptr == nullPtr
        then throwIO (userError $ "Failed to allocate: " ++ errName)
        else newForeignPtr finalizer ptr `onException` onErr ptr

-- FFI Definitions
foreign import ccall unsafe "create_ring_buffer"
    c_create_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

foreign import ccall unsafe "attach_ring_buffer"
    c_attach_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

foreign import ccall unsafe "&free_ring_buffer"
    c_free_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

foreign import ccall unsafe "&detach_ring_buffer"
    c_detach_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

foreign import ccall unsafe "free_ring_buffer"
    c_free_ring_buffer_direct :: Ptr RingBufferControl -> IO ()

foreign import ccall unsafe "detach_ring_buffer"
    c_detach_ring_buffer_direct :: Ptr RingBufferControl -> IO ()

foreign import ccall safe "read_from_uart"
    c_read_from_uart :: Ptr RingBufferControl -> CInt -> IO CSsize

foreign import ccall safe "configure_serial_port"
    c_configure_serial_port :: CInt -> CInt -> IO CInt

foreign import ccall safe "gpio_init" c_gpio_init :: IO CInt
foreign import ccall safe "gpio_write" c_gpio_write :: CInt -> CInt -> IO CInt
foreign import ccall safe "gpio_read" c_gpio_read :: CInt -> IO CInt
foreign import ccall safe "gpio_setup_watchdog" c_gpio_setup_watchdog :: CInt -> IO CInt


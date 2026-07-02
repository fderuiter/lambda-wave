-- |
-- = Failure Mode
-- C++ hardware errors translating incorrectly could mask critical hardware faults.
--
-- = Mitigation
-- Maps integer returns strictly to Haskell typed results. Unrecognized values default to Failure.
-- Mitigates Hazard H-SYS-008
--
-- = Audit Events
-- Mappings are enforced by the linter against the ffi_master_spec.md specification.
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
    | SystemError Int
    | DriverError String
    | TransientError String
    | InvalidConfiguration
    deriving (Show, Eq)

toHardwareResult :: Int -> CInt -> HardwareResult
toHardwareResult _ 0 = Success
toHardwareResult err (-1) = SystemError err
toHardwareResult _ (-2) = DriverError "Unsupported baud rate"
toHardwareResult _ n 
    | n > 0 = PartialData (fromIntegral n)
    | otherwise = Failure ("Unknown failure code: " ++ show n)

toRingBufferResult :: Int -> CSsize -> HardwareResult
toRingBufferResult _ n
    | n > 0 = PartialData (fromIntegral n)
toRingBufferResult _ 0 = TransientError "Busy"
toRingBufferResult err (-1) = SystemError err
toRingBufferResult _ (-2) = EOF
toRingBufferResult _ (-3) = TransientError "Ring buffer busy"
toRingBufferResult _ n = Failure ("Unknown ring buffer code: " ++ show n)

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


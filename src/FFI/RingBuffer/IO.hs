{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module: FFI.RingBuffer.IO

This module binds the C++ driver to the Haskell runtime and establishes
the OS-bound thread responsible for reliable data ingestion.

It implements the producer side of the pipeline, interfacing with the
hardware via C++ FFI calls.
-}
module FFI.RingBuffer.IO
    ( createRingBuffer
    , readFromUart
    , withRingBuffer -- Deprecated
    , ingestionLoop
    , getWriteOffset
    , setReadOffset
    ) where

import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (throwIO, try, IOException)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import FFI.RingBuffer.Types (RingBufferControl)

-- | Creates a ring buffer of the specified size.
-- Corresponds to C++ `RingBufferControl* create_ring_buffer(size_t size)`
foreign import ccall unsafe "create_ring_buffer"
    c_create_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

-- | Frees the ring buffer.
-- Corresponds to C++ `void free_ring_buffer(RingBufferControl* handle)`
foreign import ccall unsafe "&free_ring_buffer"
    c_free_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

-- | Reads from UART into the ring buffer.
-- Corresponds to C++ `ssize_t read_from_uart(RingBufferControl* handle, int uart_fd)`
-- Imported as safe to allow other Haskell threads to run (GC) while this blocks/waits.
foreign import ccall safe "read_from_uart"
    c_read_from_uart :: Ptr RingBufferControl -> CInt -> IO CSsize

-- | Gets the current write offset with acquire semantics.
-- Corresponds to C++ `size_t get_write_offset(RingBufferControl* handle)`
foreign import ccall unsafe "get_write_offset"
    c_get_write_offset :: Ptr RingBufferControl -> IO CSize

-- | Sets the current read offset with release semantics.
-- Corresponds to C++ `void set_read_offset(RingBufferControl* handle, size_t offset)`
foreign import ccall unsafe "set_read_offset"
    c_set_read_offset :: Ptr RingBufferControl -> CSize -> IO ()

-- | Wrapper for create_ring_buffer.
-- Returns a ForeignPtr with a finalizer ensuring memory is freed.
-- Throws userError if size <= 0 or allocation fails.
createRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
createRingBuffer size = do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_create_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to allocate Ring Buffer (C++ create_ring_buffer returned NULL)")
        else newForeignPtr c_free_ring_buffer_ptr ptr

-- | Wrapper for read_from_uart
readFromUart :: ForeignPtr RingBufferControl -> Fd -> IO Int
readFromUart fp (Fd fd) = withForeignPtr fp $ \ptr -> do
    bytesRead <- c_read_from_uart ptr fd
    return (fromIntegral bytesRead)

-- | Wrapper for get_write_offset
getWriteOffset :: ForeignPtr RingBufferControl -> IO Int
getWriteOffset fp = withForeignPtr fp $ \ptr -> do
    off <- c_get_write_offset ptr
    return (fromIntegral off)

-- | Wrapper for set_read_offset
setReadOffset :: ForeignPtr RingBufferControl -> Int -> IO ()
setReadOffset fp off = withForeignPtr fp $ \ptr ->
    c_set_read_offset ptr (fromIntegral off)

-- | Resource Management: Guarantees cleanup of the ring buffer.
-- Kept for backward compatibility, but implementation uses ForeignPtr.
withRingBuffer :: Int -> (ForeignPtr RingBufferControl -> IO a) -> IO a
withRingBuffer size action = do
    fp <- createRingBuffer size
    action fp

-- | Ingestion Thread: Spawns a bound thread that loops calling read_from_uart.
-- The loop terminates if read_from_uart returns a negative value (Error).
-- If it returns 0 (Full or EOF), we pause briefly and retry.
-- Accepts ForeignPtr to ensure the buffer is not freed while thread is running.
ingestionLoop :: ForeignPtr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop fp fd = forkOS loop
  where
    loop = do
        result <- try (readFromUart fp fd) :: IO (Either IOException Int)
        case result of
            Left e -> do
                hPutStrLn stderr $ "Error: Ingestion Exception: " ++ show e
                threadDelay 1000 -- Pause before retry to avoid hot loop on error
                loop
            Right bytesRead -> do
                if bytesRead < 0
                    then hPutStrLn stderr "Error: readFromUart returned negative value. Ingestion thread terminating."
                    else do
                        when (bytesRead == 0) $ threadDelay 1000 -- 1ms pause if full or empty
                        loop

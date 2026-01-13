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
    , freeRingBuffer
    , readFromUart
    , withRingBuffer
    , ingestionLoop
    , getWriteOffset
    , setReadOffset
    ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (bracket, throwIO)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import FFI.RingBuffer.Types (RingBufferControl)
import System.IO.Error (userError)

-- | Creates a ring buffer of the specified size.
-- Corresponds to C++ `RingBufferControl* create_ring_buffer(size_t size)`
foreign import ccall unsafe "create_ring_buffer"
    c_create_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

-- | Frees the ring buffer.
-- Corresponds to C++ `void free_ring_buffer(RingBufferControl* handle)`
foreign import ccall unsafe "free_ring_buffer"
    c_free_ring_buffer :: Ptr RingBufferControl -> IO ()

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

-- | Wrapper for create_ring_buffer
createRingBuffer :: Int -> IO (Ptr RingBufferControl)
createRingBuffer size = do
    ptr <- c_create_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to allocate Ring Buffer (C++ create_ring_buffer returned NULL)")
        else return ptr

-- | Wrapper for free_ring_buffer
freeRingBuffer :: Ptr RingBufferControl -> IO ()
freeRingBuffer = c_free_ring_buffer

-- | Wrapper for read_from_uart
readFromUart :: Ptr RingBufferControl -> Fd -> IO Int
readFromUart ptr (Fd fd) = do
    bytesRead <- c_read_from_uart ptr fd
    return (fromIntegral bytesRead)

-- | Wrapper for get_write_offset
getWriteOffset :: Ptr RingBufferControl -> IO Int
getWriteOffset ptr = do
    off <- c_get_write_offset ptr
    return (fromIntegral off)

-- | Wrapper for set_read_offset
setReadOffset :: Ptr RingBufferControl -> Int -> IO ()
setReadOffset ptr off = c_set_read_offset ptr (fromIntegral off)

-- | Resource Management: Guarantees cleanup of the ring buffer.
withRingBuffer :: Int -> (Ptr RingBufferControl -> IO a) -> IO a
withRingBuffer size action = bracket
    (createRingBuffer size)
    freeRingBuffer
    action

-- | Ingestion Thread: Spawns a bound thread that loops calling read_from_uart.
-- The loop terminates if read_from_uart returns a negative value (Error).
-- If it returns 0 (Full or EOF), we pause briefly and retry.
ingestionLoop :: Ptr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop ptr fd = forkOS loop
  where
    loop = do
        bytesRead <- readFromUart ptr fd
        if bytesRead < 0
            then return () -- Error, terminate thread
            else do
                when (bytesRead == 0) $ threadDelay 1000 -- 1ms pause if full or empty
                loop

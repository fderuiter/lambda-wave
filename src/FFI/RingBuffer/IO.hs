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
    ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (bracket)
import Control.Concurrent (forkOS, ThreadId)
import Control.Monad (unless)
import FFI.RingBuffer.Types (RingBufferControl)

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
-- Imported as unsafe for optimization to block GC during call, as per requirements.
foreign import ccall unsafe "read_from_uart"
    c_read_from_uart :: Ptr RingBufferControl -> CInt -> IO CSsize

-- | Wrapper for create_ring_buffer
createRingBuffer :: Int -> IO (Ptr RingBufferControl)
createRingBuffer size = c_create_ring_buffer (fromIntegral size)

-- | Wrapper for free_ring_buffer
freeRingBuffer :: Ptr RingBufferControl -> IO ()
freeRingBuffer = c_free_ring_buffer

-- | Wrapper for read_from_uart
readFromUart :: Ptr RingBufferControl -> Fd -> IO Int
readFromUart ptr (Fd fd) = do
    bytesRead <- c_read_from_uart ptr (fromIntegral fd)
    return (fromIntegral bytesRead)

-- | Resource Management: Guarantees cleanup of the ring buffer.
withRingBuffer :: Int -> (Ptr RingBufferControl -> IO a) -> IO a
withRingBuffer size action = bracket
    (createRingBuffer size)
    freeRingBuffer
    action

-- | Ingestion Thread: Spawns a bound thread that loops calling read_from_uart.
-- The loop terminates if read_from_uart returns 0 (EOF) or a negative value (Error).
ingestionLoop :: Ptr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop ptr fd = forkOS loop
  where
    loop = do
        bytesRead <- readFromUart ptr fd
        -- If bytesRead > 0, we continue.
        -- If bytesRead == 0 (EOF) or bytesRead < 0 (Error), we stop.
        unless (bytesRead <= 0) loop

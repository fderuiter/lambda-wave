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
    , attachRingBuffer
    , readFromUart
    , HardwareResult(..)
    , ingestionLoop
    , getWriteOffset
    , setReadOffset
    ) where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import System.Posix.Types (Fd(..))
import Control.Exception (throwIO, catch, SomeException)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import FFI.RingBuffer.Types (getBufferSize)
import FFI.RingBuffer.Generated (RingBufferControl, c_get_write_offset, c_set_read_offset)
import Hardware.FFI.Common

-- | Result of a read operation from the Ring Buffer / UART uses HardwareResult
-- via the shared Hardware.FFI.Common module.

-- | Wrapper for create_ring_buffer.
createRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
createRingBuffer size = do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    allocateManagedResource 
        (c_create_ring_buffer (fromIntegral size))
        c_free_ring_buffer_ptr
        c_free_ring_buffer_direct
        "Ring Buffer (create_ring_buffer returned NULL)"

-- | Wrapper for attach_ring_buffer.
attachRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
attachRingBuffer size = do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    allocateManagedResource 
        (c_attach_ring_buffer (fromIntegral size))
        c_detach_ring_buffer_ptr
        c_detach_ring_buffer_direct
        "Ring Buffer (attach_ring_buffer returned NULL)"

-- | Wrapper for read_from_uart
-- Enforces type-safe error handling via HardwareResult ADT.
--
-- C++ Return Codes Mapping:
-- * > 0 : Bytes successfully read
-- * 0   : Buffer Logic Full (Busy) -> Mapped to Busy
-- * -2  : EOF (Device Disconnected) -> Mapped to EOF
-- * -3  : EAGAIN (No Data) -> Mapped to Busy
-- * -1  : Critical Error -> Mapped to PosixError
readFromUart :: ForeignPtr RingBufferControl -> Fd -> IO HardwareResult
readFromUart fp (Fd fd) = withForeignPtr fp $ \ptr -> do
    bytesRead <- c_read_from_uart ptr fd
    return (toRingBufferResult bytesRead)

-- | Wrapper for get_write_offset
getWriteOffset :: ForeignPtr RingBufferControl -> IO Int
getWriteOffset fp = withForeignPtr fp $ \ptr -> do
    off <- c_get_write_offset ptr
    return (fromIntegral off)

-- | Wrapper for set_read_offset
-- SENTINEL SAFETY CHECK: Enforces non-negative offset AND bounds check to prevent buffer overflow/corruption.
-- We use getBufferSize to safely enforce bounds without raw pointers.
setReadOffset :: ForeignPtr RingBufferControl -> Int -> IO ()
setReadOffset fp off = do
    when (off < 0) $ throwIO (userError "Negative offset provided to setReadOffset")
    bufSize <- getBufferSize fp
    when (off >= bufSize) $
        throwIO (userError $ "Offset " ++ show off ++ " exceeds buffer size " ++ show bufSize)
    withForeignPtr fp $ \ptr -> c_set_read_offset ptr (fromIntegral off)

-- | Ingestion Thread: Spawns a bound thread that loops calling read_from_uart.
-- The loop terminates if read_from_uart returns ReadError or ReadEOF.
-- If it returns ReadBusy (Full or No Data), we pause briefly and retry.
-- Accepts ForeignPtr to ensure the buffer is not freed while thread is running.
ingestionLoop :: ForeignPtr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop fp fd = forkOS loop
  where
    loop = safeLoop `catch` \e -> do
        hPutStrLn stderr $ "CRITICAL FAILURE in Ingestion Thread: " ++ show (e :: SomeException)
        -- We terminate the thread, but at least we logged it.
        return ()

    safeLoop = do
        result <- readFromUart fp fd
        case result of
            PosixError -> hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned error. Ingestion thread TERMINATING."
            InvalidConfiguration -> hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned invalid configuration. Ingestion thread TERMINATING."
            Failure _ -> hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned unknown failure. Ingestion thread TERMINATING."
            EOF -> do
                hPutStrLn stderr "Ingestion Thread: Device Disconnected (EOF). Terminating."
                return ()
            Busy -> do
                threadDelay 1000 -- 1ms pause if full or empty
                loop
            PartialData _ -> loop
            Success -> loop

-- Requirement FR-DAQ-001

-- Requirement FR-DAQ-004

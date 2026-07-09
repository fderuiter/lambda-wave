
-- | High-assurance FFI Bridge Logic for RingBuffer.
-- 
-- The FFI bridge guarantees memory safety when interfacing between Haskell and C++ drivers.
-- 
-- Failure Modes:
-- * Buffer overflow if consumer falls behind producer.
-- * FFI boundary corruption during context switch.
-- 
-- Mitigations:
-- * Strict read/write offset tracking using atomic memory operations.
-- * Hard boundary bounds-checking enforced by `enforce_bounds.py`.
-- * Minimal language extensions used for stability.
-- 
-- Traceability: FR-DAQ-001, FR-DAQ-004

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
import Control.Exception (throwIO)
import Control.Concurrent (ThreadId, threadDelay)
import Safety.Thread (forkSafetyThreadOS, ThreadShutdownAction(..))
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import FFI.RingBuffer.Types (getBufferSize)
import FFI.RingBuffer.Generated (RingBufferControl, c_get_write_offset, c_set_read_offset)
import Hardware.FFI.Common
import Hardware.FFI.Bridge
import Control.Concurrent.STM (TVar)
import Data.Types (SystemState)

-- | Result of a read operation from the Ring Buffer / UART uses HardwareResult
-- via the shared Hardware.FFI.Common module.

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | Wrapper for create_ring_buffer.
createRingBuffer :: TVar SystemState -> Int -> IO (MustHandle (ForeignPtr RingBufferControl))
createRingBuffer stateVar size = do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    (fp, status) <- alloca $ \statusPtr -> do
        fp' <- allocateManagedResource 
            (c_create_ring_buffer (fromIntegral size) statusPtr)
            c_free_ring_buffer_ptr
            c_free_ring_buffer_direct
            "Ring Buffer (create_ring_buffer returned NULL)"
        st <- peek statusPtr
        return (fp', st)
    let res = toHardwareResult 0 status
    executeBridgeCall (auditHardwareEvent stateVar "RingBuffer") (return res) >>= \case
        MustHandle (Right _) -> return $ MustHandle (Right fp)
        MustHandle (Left err) -> return $ MustHandle (Left err)

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
readFromUart :: TVar SystemState -> ForeignPtr RingBufferControl -> Fd -> IO HardwareResult
readFromUart stateVar fp (Fd fd) = withForeignPtr fp $ \ptr -> do
    bridgeRingBufferCall stateVar "RingBuffer" (c_read_from_uart ptr fd)

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
ingestionLoop :: TVar SystemState -> ForeignPtr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop stateVar fp fd = forkSafetyThreadOS (ShutdownSystem $ triggerShutdown stateVar) "IngestionLoop" loop
  where
    loop = do
        result <- readFromUart stateVar fp fd
        case result of
            SystemError err -> do
                hPutStrLn stderr $ "CRITICAL FAILURE: readFromUart returned POSIX error: " ++ show err
                triggerShutdown stateVar "UART POSIX Error"
            InvalidConfiguration -> do
                hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned invalid configuration. Ingestion thread TERMINATING."
                triggerShutdown stateVar "UART Invalid Config"
            DriverError err -> do
                hPutStrLn stderr $ "CRITICAL FAILURE: readFromUart returned driver error: " ++ err
                triggerShutdown stateVar "UART Driver Error"
            Failure err -> do
                hPutStrLn stderr $ "CRITICAL FAILURE: readFromUart returned unknown failure: " ++ err
                triggerShutdown stateVar "UART Unknown Failure"
            EOF -> do
                hPutStrLn stderr "Ingestion Thread: Device Disconnected (EOF). Terminating."
                triggerShutdown stateVar "UART EOF"
            SimulationMode -> do
                hPutStrLn stderr "Ingestion Thread: Hardware in Simulation Mode. Continuing."
                threadDelay 1000
                loop
            TransientError _ -> do
                threadDelay 1000 -- 1ms pause if full or empty
                loop
            PartialData _ -> loop
            Success -> loop
            Busy -> loop

-- Requirement FR-DAQ-001

-- Requirement FR-DAQ-004

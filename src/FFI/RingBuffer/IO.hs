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
    , ReadResult(..)
    , ingestionLoop
    , getWriteOffset
    , setReadOffset
    ) where

import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (throwIO, catch, SomeException, mask_, onException)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import FFI.RingBuffer.Types (peekStaticFields)
import FFI.RingBuffer.Generated
import Control.DeepSeq (NFData(..))

-- | Result of a read operation from the Ring Buffer / UART
data ReadResult
    = ReadSuccess Int -- ^ Bytes successfully read and written to buffer
    | ReadBusy        -- ^ Buffer full or no data available (retry later)
    | ReadEOF         -- ^ Device Disconnected (End of Stream)
    | ReadError       -- ^ Critical failure (e.g. UART error)
    deriving (Show, Eq)

instance NFData ReadResult where
    rnf (ReadSuccess n) = rnf n
    rnf ReadBusy        = ()
    rnf ReadEOF         = ()
    rnf ReadError       = ()

-- | Creates a ring buffer of the specified size.
-- Corresponds to C++ `RingBufferControl* create_ring_buffer(size_t size)`
foreign import ccall unsafe "create_ring_buffer"
    c_create_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

-- | Attaches to an existing ring buffer.
foreign import ccall unsafe "attach_ring_buffer"
    c_attach_ring_buffer :: CSize -> IO (Ptr RingBufferControl)

-- | Frees the ring buffer.
foreign import ccall unsafe "&free_ring_buffer"
    c_free_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

-- | Detaches from the ring buffer without unlinking it.
foreign import ccall unsafe "&detach_ring_buffer"
    c_detach_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

-- | Direct import for manual cleanup on error
foreign import ccall unsafe "free_ring_buffer"
    c_free_ring_buffer_direct :: Ptr RingBufferControl -> IO ()

-- | Direct import for manual detach on error
foreign import ccall unsafe "detach_ring_buffer"
    c_detach_ring_buffer_direct :: Ptr RingBufferControl -> IO ()

-- | Reads from UART into the ring buffer.
foreign import ccall safe "read_from_uart"
    c_read_from_uart :: Ptr RingBufferControl -> CInt -> IO CSsize

-- | Gets the current write offset with acquire semantics.


-- | Sets the current read offset with release semantics.


-- | Wrapper for create_ring_buffer.
createRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
createRingBuffer size = mask_ $ do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_create_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to allocate Ring Buffer (C++ create_ring_buffer returned NULL)")
        else newForeignPtr c_free_ring_buffer_ptr ptr
                `onException` c_free_ring_buffer_direct ptr

-- | Wrapper for attach_ring_buffer.
attachRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
attachRingBuffer size = mask_ $ do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_attach_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to attach to Ring Buffer (C++ attach_ring_buffer returned NULL)")
        else newForeignPtr c_detach_ring_buffer_ptr ptr
                `onException` c_detach_ring_buffer_direct ptr

-- | Wrapper for read_from_uart
-- Enforces type-safe error handling via ReadResult ADT.
--
-- C++ Return Codes Mapping:
-- * > 0 : Bytes successfully read
-- * 0   : Buffer Logic Full (Busy) -> Mapped to ReadBusy
-- * -2  : EOF (Device Disconnected) -> Mapped to ReadEOF
-- * -3  : EAGAIN (No Data) -> Mapped to ReadBusy
-- * -1  : Critical Error -> Mapped to ReadError
readFromUart :: ForeignPtr RingBufferControl -> Fd -> IO ReadResult
readFromUart fp (Fd fd) = withForeignPtr fp $ \ptr -> do
    bytesRead <- c_read_from_uart ptr fd
    return $ case bytesRead of
        n | n > 0 -> ReadSuccess (fromIntegral n)
        0         -> ReadBusy -- Buffer Logic Full (verified in C++)
        -2        -> ReadEOF  -- EOF (Device Disconnected)
        -3        -> ReadBusy -- EAGAIN (No Data)
        _         -> ReadError

-- | Wrapper for get_write_offset
getWriteOffset :: ForeignPtr RingBufferControl -> IO Int
getWriteOffset fp = withForeignPtr fp $ \ptr -> do
    off <- c_get_write_offset ptr
    return (fromIntegral off)

-- | Wrapper for set_read_offset
-- SENTINEL SAFETY CHECK: Enforces non-negative offset AND bounds check to prevent buffer overflow/corruption.
-- We peek the bufferSize from the control block to ensure off < bufferSize.
setReadOffset :: ForeignPtr RingBufferControl -> Int -> IO ()
setReadOffset fp off = do
    when (off < 0) $ throwIO (userError "Negative offset provided to setReadOffset")
    withForeignPtr fp $ \ptr -> do
        -- Peek buffer size to enforce bounds
        (_, bufSize) <- peekStaticFields ptr
        when (fromIntegral off >= bufSize) $
            throwIO (userError $ "Offset " ++ show off ++ " exceeds buffer size " ++ show bufSize)

        c_set_read_offset ptr (fromIntegral off)

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
            ReadError -> hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned error. Ingestion thread TERMINATING."
            ReadEOF -> do
                hPutStrLn stderr "Ingestion Thread: Device Disconnected (EOF). Terminating."
                return ()
            ReadBusy -> do
                threadDelay 1000 -- 1ms pause if full or empty
                loop
            ReadSuccess _ -> loop

-- Requirement FR-DAQ-001

-- Requirement FR-DAQ-004

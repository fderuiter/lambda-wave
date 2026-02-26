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
    , ReadResult(..)
    , ingestionLoop
    , ingestionWorker -- Exported for tests
    , getWriteOffset
    , setReadOffset
    ) where

import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (throwIO, mask_, onException, try, IOException, bracket)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (openFd, closeFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import FFI.RingBuffer.Types (RingBufferControl)
import Control.DeepSeq (NFData(..))
import Hardware.Control (configureRawSerial)

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

-- | Frees the ring buffer.
-- Corresponds to C++ `void free_ring_buffer(RingBufferControl* handle)`
foreign import ccall unsafe "&free_ring_buffer"
    c_free_ring_buffer_ptr :: FunPtr (Ptr RingBufferControl -> IO ())

-- | Direct import for manual cleanup on error
foreign import ccall unsafe "free_ring_buffer"
    c_free_ring_buffer_direct :: Ptr RingBufferControl -> IO ()

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
createRingBuffer size = mask_ $ do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_create_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to allocate Ring Buffer (C++ create_ring_buffer returned NULL)")
        else newForeignPtr c_free_ring_buffer_ptr ptr
                `onException` c_free_ring_buffer_direct ptr

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
-- SENTINEL SAFETY CHECK: Enforces non-negative offset to prevent buffer overflow attacks.
-- A negative Int cast to CSize (unsigned) becomes a huge number, causing C++ logic errors.
setReadOffset :: ForeignPtr RingBufferControl -> Int -> IO ()
setReadOffset fp off = do
    when (off < 0) $ throwIO (userError "Negative offset provided to setReadOffset")
    withForeignPtr fp $ \ptr ->
        c_set_read_offset ptr (fromIntegral off)

-- | Ingestion Thread: Spawns a bound thread that manages the UART connection lifecycle.
-- Automatically attempts to reconnect if the device is disconnected.
ingestionLoop :: ForeignPtr RingBufferControl -> FilePath -> IO ThreadId
ingestionLoop fp portPath = forkOS reconnectLoop
  where
    reconnectLoop = do
        putStrLn $ "[Ingestion] Connecting to " ++ portPath ++ "..."

        -- Attempt to open the port
        -- We use bracket to ensure FD is closed if an exception occurs during configuration
        result <- try $ bracket
#if MIN_VERSION_unix(2,8,0)
            (openFd portPath ReadWrite defaultFileFlags { nonBlock = False, creat = Nothing })
#else
            (openFd portPath ReadWrite Nothing defaultFileFlags { nonBlock = False })
#endif
            closeFd
            (\fd -> do
                putStrLn "[Ingestion] Port Opened. Configuring..."
                res <- configureRawSerial fd
                case res of
                    Left err -> do
                        hPutStrLn stderr $ "[Ingestion] Configuration Failed: " ++ show err
                        throwIO (userError $ show err) -- Trigget cleanup and retry
                    Right () -> do
                        putStrLn "[Ingestion] Ready. Starting Read Loop."
                        readLoop fd
            )

        case result of
            Left e -> do
                hPutStrLn stderr $ "[Ingestion] Connection Failed: " ++ show (e :: IOException)
                hPutStrLn stderr "[Ingestion] Retrying in 1s..."
                threadDelay 1_000_000
                reconnectLoop
            Right () -> do
                -- Should not be reached unless readLoop returns normally (which it shouldn't)
                hPutStrLn stderr "[Ingestion] Read Loop exited unexpectedly. Reconnecting..."
                threadDelay 1_000_000
                reconnectLoop

    readLoop = ingestionWorker fp

-- | Reads from an already open file descriptor (e.g. from a pipe or serial port).
-- Loops until ReadEOF or ReadError occurs.
-- Exported for testing purposes.
ingestionWorker :: ForeignPtr RingBufferControl -> Fd -> IO ()
ingestionWorker fp fd = do
    result <- readFromUart fp fd
    case result of
        ReadError -> do
            hPutStrLn stderr "[Ingestion] CRITICAL: readFromUart returned error."
            return ()
        ReadEOF -> do
            hPutStrLn stderr "[Ingestion] Device Disconnected (EOF)."
            return ()
        ReadBusy -> do
            threadDelay 1000 -- 1ms pause
            ingestionWorker fp fd
        ReadSuccess _ -> ingestionWorker fp fd

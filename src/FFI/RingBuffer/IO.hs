{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.RingBuffer.IO
    ( createRingBuffer
    , attachRingBuffer
    , readFromUart
    , ReadResult(..)
    , ingestionLoop
    , checkoutBlock
    , releaseBlock
    , getBlockBytesWritten
    ) where

import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (CSsize(..), Fd(..))
import Control.Exception (throwIO, catch, SomeException, mask_, onException)
import Control.Concurrent (forkOS, ThreadId, threadDelay)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import FFI.RingBuffer.Types (RingBufferControl, peekStaticFields)
import Control.DeepSeq (NFData(..))

data ReadResult
    = ReadSuccess Int
    | ReadBusy
    | ReadEOF
    | ReadError
    deriving (Show, Eq)

instance NFData ReadResult where
    rnf (ReadSuccess n) = rnf n
    rnf ReadBusy        = ()
    rnf ReadEOF         = ()
    rnf ReadError       = ()

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

foreign import ccall unsafe "checkout_block"
    c_checkout_block :: Ptr RingBufferControl -> IO CSsize

foreign import ccall unsafe "release_block"
    c_release_block :: Ptr RingBufferControl -> CSize -> IO ()

foreign import ccall unsafe "get_block_bytes_written"
    c_get_block_bytes_written :: Ptr RingBufferControl -> CSize -> IO CSize

createRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
createRingBuffer size = mask_ $ do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_create_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to allocate Ring Buffer (C++ create_ring_buffer returned NULL)")
        else newForeignPtr c_free_ring_buffer_ptr ptr
                `onException` c_free_ring_buffer_direct ptr

attachRingBuffer :: Int -> IO (ForeignPtr RingBufferControl)
attachRingBuffer size = mask_ $ do
    when (size <= 0) $ throwIO (userError "Ring Buffer size must be positive")
    ptr <- c_attach_ring_buffer (fromIntegral size)
    if ptr == nullPtr
        then throwIO (userError "Failed to attach to Ring Buffer (C++ attach_ring_buffer returned NULL)")
        else newForeignPtr c_detach_ring_buffer_ptr ptr
                `onException` c_detach_ring_buffer_direct ptr

readFromUart :: ForeignPtr RingBufferControl -> Fd -> IO ReadResult
readFromUart fp (Fd fd) = withForeignPtr fp $ \ptr -> do
    bytesRead <- c_read_from_uart ptr fd
    return $ case bytesRead of
        n | n > 0 -> ReadSuccess (fromIntegral n)
        0         -> ReadBusy
        -2        -> ReadEOF
        -3        -> ReadBusy
        _         -> ReadError

checkoutBlock :: ForeignPtr RingBufferControl -> IO (Maybe Int)
checkoutBlock fp = withForeignPtr fp $ \ptr -> do
    idx <- c_checkout_block ptr
    if idx == -1
        then return Nothing
        else return (Just (fromIntegral idx))

releaseBlock :: ForeignPtr RingBufferControl -> Int -> IO ()
releaseBlock fp idx = withForeignPtr fp $ \ptr -> do
    c_release_block ptr (fromIntegral idx)

getBlockBytesWritten :: ForeignPtr RingBufferControl -> Int -> IO Int
getBlockBytesWritten fp idx = withForeignPtr fp $ \ptr -> do
    bytes <- c_get_block_bytes_written ptr (fromIntegral idx)
    return (fromIntegral bytes)

ingestionLoop :: ForeignPtr RingBufferControl -> Fd -> IO ThreadId
ingestionLoop fp fd = forkOS loop
  where
    loop = safeLoop `catch` \e -> do
        hPutStrLn stderr $ "CRITICAL FAILURE in Ingestion Thread: " ++ show (e :: SomeException)
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

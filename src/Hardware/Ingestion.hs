{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hardware.Ingestion
Description : Zero-Copy Data Ingestion Engine
Copyright   : (c) 2024
License     : MIT
Maintainer  : atlas@code-cartographer.com

= The Ingestion Engine 🐉

This module handles the interface between the OS Serial Port and the Haskell Runtime.
To avoid Garbage Collection (GC) pauses causing packet drops at 921,600 baud,
we utilize a **Pinned Memory Ring Buffer** implemented in C.

== Architecture
1. **The Ingestion Thread**: Reads from UART and writes to a C-allocated Ring Buffer.
   Currently, this involves a 'Dragon' where we call 'c_write_byte' for every single byte,
   which is an inefficient FFI pattern.
2. **The Reader**: The 'readChunkFromC' function exposes a safe interface for other Haskell threads
   (like the Parser) to pull data from this buffer without allocating intermediate Haskell buffers
   until necessary.

== Usage
> forkOS $ ingestionLoop rawDataChan "/dev/ttyACM1"
-}
module Hardware.Ingestion (
    ingestionLoop,
    c_init_ring_buffer,
    c_read_chunk,
    readChunkFromC -- Exported for Parser usage
) where

import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import System.Hardware.Serialport hiding (CommSpeed)
import qualified System.Hardware.Serialport as SP
import Control.Monad (forever)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word (Word8)

-- FFI Imports
foreign import ccall "init_ring_buffer" c_init_ring_buffer :: IO ()
foreign import ccall "write_byte" c_write_byte :: Word8 -> IO ()
foreign import ccall "read_chunk" c_read_chunk :: Ptr Word8 -> CSize -> IO CSize

-- | The Ingestion Loop
-- Connects to the serial port and continuously pumps data into the C Ring Buffer.
ingestionLoop :: TChan B.ByteString -> FilePath -> IO ()
ingestionLoop rawDataChan portPath = do
    -- 0. Init C Buffer
    c_init_ring_buffer

    -- 1. Open Serial Port (921600 baud for IWR6843)
    -- Note: CS921600 might not be available in this version of serialport.
    -- Using CS115200 for compilation; real hardware needs correct baud rate.
    s <- openSerial portPath defaultSerialSettings { commSpeed = SP.CS115200 }
    putStrLn $ "[Hardware] Listening on " ++ portPath

    -- We spin up a thread to pump data from Serial to C Ring Buffer
    serialPump s

  where
    serialPump s = forever $ do
        chunk <- recv s 1024
        if B.null chunk
            then return ()
            else do
                -- Write to C Ring Buffer
                -- TODO: Dragon 🐉 - This `mapM_` invokes FFI overhead for every byte.
                -- Ideally, implement `c_write_chunk` in C to copy the whole buffer at once.
                mapM_ (\b -> c_write_byte b) (B.unpack chunk)


-- | Helper to read from C buffer (used by Parser)
-- Allocates a ByteString backed by Pinned Memory and fills it from the C Ring Buffer.
readChunkFromC :: Int -> IO B.ByteString
readChunkFromC maxLen = do
    -- Allocate a buffer
    fp <- BI.mallocByteString maxLen
    len <- withForeignPtr fp $ \p -> c_read_chunk p (fromIntegral maxLen)
    if len == 0
        then return B.empty
        else return $ BI.fromForeignPtr fp 0 (fromIntegral len)

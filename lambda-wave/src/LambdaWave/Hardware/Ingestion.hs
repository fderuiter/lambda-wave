{-# LANGUAGE ForeignFunctionInterface #-}
module LambdaWave.Hardware.Ingestion (ingestionLoop, c_init_ring_buffer, c_read_chunk) where

import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import System.Hardware.Serialport
import Control.Monad (forever)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word (Word8)

-- FFI Imports
foreign import ccall "init_ring_buffer" c_init_ring_buffer :: IO ()
foreign import ccall "write_byte" c_write_byte :: Word8 -> IO ()
foreign import ccall "read_chunk" c_read_chunk :: Ptr Word8 -> CSize -> IO CSize

-- | The Thread: Reads bytes -> Pushes to Parser Buffer
-- This version uses the C Ring Buffer for ingestion
ingestionLoop :: TChan B.ByteString -> FilePath -> IO ()
ingestionLoop rawDataChan portPath = do
    -- 0. Init C Buffer
    c_init_ring_buffer

    -- 1. Open Serial Port (921600 baud for IWR6843)
    -- Note: In a real scenario, we might want to read from serial in C or
    -- pass the serial file descriptor to C. However, the prompt says:
    -- "Spawns a dedicated OS thread that calls c_read_uart" (Wait, the prompt says "c_read_uart" but the C file has "write_byte"?)
    -- The prompt description for Ingestion.hs says:
    -- "Spawns a dedicated OS thread that calls c_read_uart and writes into the Pinned Memory. It exposes a Haskell readChunk function that reads from this C-buffer safely."
    -- BUT the C code I was asked to build (`ring_buffer.c`) has `write_byte` and `read_chunk`.
    -- And `Ingestion.hs` usually reads FROM serial AND writes TO the buffer.
    -- If `c_read_uart` existed, it would take the FD and write to buffer directly.
    -- Since I only implemented `write_byte` in C as per the "skeleton" C file description in the prompt (which listed `write_byte` in `ring_buffer.h`),
    -- I will simulate the "read from serial in Haskell and write to C buffer" approach, OR "read from C buffer in Haskell".

    -- Let's re-read the Prompt carefully:
    -- "Ingestion.hs (The FFI Bridge): ... Spawns a dedicated OS thread that calls c_read_uart and writes into the Pinned Memory. It exposes a Haskell readChunk function that reads from this C-buffer safely."
    -- AND "ring_buffer.c: ... Exports init_ring_buffer(), write_byte(), read_chunk()."

    -- There is a slight contradiction or I need to implement `c_read_uart` using `write_byte`?
    -- No, `c_read_uart` implies C reading from UART.
    -- But `ring_buffer.h` exports `write_byte`.
    -- It seems the Haskell `Ingestion.hs` is the one reading from Serial Port (using `serialport` lib) and writing to the C ring buffer using `write_byte`.
    -- THEN `Parser.hs` or similar would read from the C ring buffer?
    -- The prompt says "The Thread: Reads bytes -> Pushes to Parser Buffer".
    -- Let's implement the loop: Haskell reads Serial -> Calls C `write_byte`.
    -- And `Parser.hs` (or a helper) calls C `read_chunk`.

    -- WAIT, the prompt says `Ingestion.hs` exposes a Haskell `readChunk` function.
    -- So `Ingestion.hs` manages the C buffer.

    s <- openSerial portPath defaultSerialSettings { commSpeed = CS921600 }
    putStrLn $ "[Hardware] Listening on " ++ portPath

    -- We spin up a thread to pump data from Serial to C Ring Buffer
    -- Since ingestionLoop runs in its own thread (from Main), we just run the pump here directly.

    serialPump s

  where
    serialPump s = forever $ do
        chunk <- recv s 1024
        if B.null chunk
            then return ()
            else do
                -- Write to C Ring Buffer
                mapM_ (\b -> c_write_byte b) (B.unpack chunk)


-- | Helper to read from C buffer (used by Parser)
readChunkFromC :: Int -> IO B.ByteString
readChunkFromC maxLen = do
    -- Allocate a buffer
    fp <- BI.mallocByteString maxLen
    len <- withForeignPtr fp $ \p -> c_read_chunk p (fromIntegral maxLen)
    if len == 0
        then return B.empty
        else return $ BI.fromForeignPtr fp 0 (fromIntegral len)

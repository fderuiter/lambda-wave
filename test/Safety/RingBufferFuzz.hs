{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import FFI.RingBuffer.IO
import FFI.RingBuffer.Types (RingBufferControl)
import Foreign.ForeignPtr (ForeignPtr)
import System.Posix.IO (createPipe, fdWrite, closeFd)
import System.Posix.Types (Fd(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_, when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Starting Ring Buffer Fuzz Test..."

    -- Create Pipe
    (readFd, writeFd) <- createPipe
    putStrLn $ "Created Pipe: Read=" ++ show readFd ++ ", Write=" ++ show writeFd

    -- Create Ring Buffer (Small size to force wrapping)
    let size = 128
    let bufferSize = case mkRingBufferSize size of
            Right s -> s
            Left e -> error e

    rb <- createRingBuffer bufferSize
    putStrLn "Ring Buffer Created."

    -- Fork Writer Thread
    _ <- forkIO $ do
        putStrLn "Writer Thread Started."
        let msg = "Hello World!" -- 12 bytes
        -- Write 20 times -> 240 bytes (Buffersize 128)
        res <- try $ replicateM_ 20 $ do
            _ <- fdWrite writeFd msg
            threadDelay 1000 -- 1ms
        case res of
            Left e -> putStrLn $ "Writer Failed: " ++ show (e :: SomeException)
            Right _ -> putStrLn "Writer Thread Finished."
        closeFd writeFd -- EOF

    -- Fork Consumer Thread
    _ <- forkIO $ consumerLoop rb size

    -- Reader Loop (Main Thread)
    putStrLn "Reader Loop Started."
    loop rb readFd 0

consumerLoop :: ForeignPtr RingBufferControl -> Int -> IO ()
consumerLoop rb size = do
    -- Simple consumer that just advances read offset to match write offset
    -- simulating instant consumption
    threadDelay 5000 -- 5ms poll
    woff <- getWriteOffset rb
    -- putStrLn $ "Consumer: Advancing read offset to " ++ show woff
    setReadOffset rb woff
    consumerLoop rb size

loop :: ForeignPtr RingBufferControl -> Fd -> Int -> IO ()
loop rb fd totalRead = do
    res <- readFromUart rb fd
    case res of
        ReadSuccess n -> do
            let newTotal = totalRead + n
            putStrLn $ "Read " ++ show n ++ " bytes. Total: " ++ show newTotal
            if newTotal >= 240
                then do
                    putStrLn "SUCCESS: Read all expected bytes."
                    exitSuccess
                else loop rb fd newTotal
        ReadFull -> do
            -- Buffer Full or Empty or EOF
            -- putStrLn "ReadFull (Retry)"
            threadDelay 1000
            -- Wait for consumer to free space
            loop rb fd totalRead
        ReadError code -> do
            putStrLn $ "Read Error: " ++ show code
            exitFailure

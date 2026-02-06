{-# LANGUAGE TypeApplications #-}
module Safety.Audit (auditLoop) where

import Control.Monad (when, void)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO
import System.Posix.Files (rename)
import Control.Exception (try, SomeException)

import Data.Types

-- | Max Log Size (10MB)
maxLogSize :: Integer
maxLogSize = 10 * 1024 * 1024

-- | Main Audit Loop
-- Consumes events from the queue and writes them to disk.
-- Handles log rotation and immediate flushing for critical events.
--
-- Complexity: O(1) per event (amortized).
-- Safety:
--  * Catches all exceptions to prevent thread death (Logging to stderr on failure).
--  * Uses STM for thread-safe consumption.
--  * Rotates logs to prevent disk exhaustion (limit 10MB).
--
-- Note: Producers using 'writeTBQueue' may block if the queue is full (backpressure),
-- potentially impacting real-time threads if the consumer stalls.
auditLoop :: TBQueue AuditEvent -> FilePath -> IO ()
auditLoop queue logPath = do
    -- Open initially in AppendMode
    hInitial <- safeOpen logPath
    loop hInitial
  where
    safeOpen path = do
        res <- try @SomeException $ do
            h <- openFile path AppendMode
            hSetBuffering h LineBuffering
            return h
        case res of
            Right h -> return h
            Left e -> do
                hPutStrLn stderr $ "Audit Init Error: " ++ show e
                threadDelay 1000000 -- Wait 1s
                safeOpen path

    loop h = do
        -- Read event (blocking) - This is outside try to avoid tight loop on read error?
        -- No, readTBQueue is STM, unlikely to throw unless runtime issues.
        -- But let's wrap everything.

        res <- try @SomeException $ do
            ev <- atomically $ readTBQueue queue

            -- Check rotation
            h' <- checkRotation h logPath

            -- Write
            hPutStrLn h' $ formatEvent ev
            when (evtSeverity ev == Critical) $ hFlush h'

            return h'

        case res of
            Right hNext -> loop hNext
            Left e -> do
                hPutStrLn stderr $ "Audit Loop Error: " ++ show e
                -- Try to close old handle just in case (ignore error)
                void $ try @SomeException $ hClose h
                -- Wait and Reopen
                threadDelay 1000000
                hNew <- safeOpen logPath
                loop hNew

    checkRotation :: Handle -> FilePath -> IO Handle
    checkRotation h path = do
        size <- hFileSize h
        if size > maxLogSize
           then do
               hClose h
               rotateLogs path
               -- Reopen
               hNew <- openFile path AppendMode
               hSetBuffering hNew LineBuffering
               return hNew
           else return h

    rotateLogs :: FilePath -> IO ()
    rotateLogs path = do
        -- Simple rotation: log -> log.1
        let backup = path ++ ".1"
        void $ try @SomeException $ rename path backup

    formatEvent :: AuditEvent -> String
    formatEvent (AuditEvent t s msg src) =
        show t ++ "," ++ show s ++ "," ++ src ++ "," ++ msg

module Safety.Audit (
    auditLoop
) where

import Data.Types
import Control.Concurrent.STM
import System.IO
import Control.Monad (when)
import Control.Exception (catch, SomeException)
import System.Posix.Files (rename)
import Data.Time.HighRes (getRealTimeNS)

-- | Max log size: 10MB
maxLogSize :: Integer
maxLogSize = 10 * 1024 * 1024

-- | The Audit Loop
-- Consumes AuditEvents from the queue and writes them to disk.
-- Handles log rotation and immediate flushing for critical events.
--
-- = Log Format
-- @[Timestamp] [Severity] [Source] Message@
--
-- = Retention Policy
-- Logs are rotated when they exceed 10MB.
-- The rotated file is renamed to @audit.log.<timestamp>@.
-- Retention of old logs is currently unbounded (disk space dependent).
auditLoop :: TBQueue AuditEvent -> FilePath -> IO ()
auditLoop queue logPath = do
    handle <- openLogFile logPath
    loop handle
  where
    loop h = do
        -- Read next event (blocks)
        event <- atomically $ readTBQueue queue

        -- Write event
        let logEntry = formatEvent event

        -- Catch exceptions during write to avoid killing the thread
        hNext <- catch (do
            hPutStrLn h logEntry

            -- Flush if Critical, Error, or if message implies BeamHold
            -- Ideally we'd inspect the event more deeply, but Severity is a good proxy.
            let isCritical = aeSeverity event >= Error
            -- Also flush Warning? Maybe not.

            when isCritical $ hFlush h
            return h
            ) (\e -> do
                hPutStrLn stderr $ "[Audit] WRITE FAILED: " ++ show (e :: SomeException)
                return h
            )

        -- Check rotation
        -- hFileSize works on Handle.
        let handleSizeError :: SomeException -> IO Integer
            handleSizeError _ = return 0
        size <- catch (hFileSize hNext) handleSizeError

        if size > maxLogSize
            then do
                hClose hNext
                rotateLog logPath
                newH <- openLogFile logPath
                loop newH
            else loop hNext

    openLogFile path = do
        h <- openFile path AppendMode
        hSetBuffering h LineBuffering
        return h

    formatEvent :: AuditEvent -> String
    formatEvent (AuditEvent ts sev src msg) =
        "[" ++ show ts ++ "] [" ++ show sev ++ "] [" ++ src ++ "] " ++ msg

    rotateLog path = do
        -- Rename audit.log to audit.log.<timestamp>
        ts <- getRealTimeNS
        let backup = path ++ "." ++ show ts
        catch (rename path backup) (\e ->
            hPutStrLn stderr $ "[Audit] ROTATION FAILED: " ++ show (e :: SomeException))

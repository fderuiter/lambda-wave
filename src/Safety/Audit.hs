{-# LANGUAGE StrictData #-}

{-|
Module      : Safety.Audit
Description : Immutable, Secure Audit Logging
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the audit logging facility for the Lambda-Wave system.
It adheres to IEC 62304 Class C requirements for safety logging:
1.  **Immutability:** Logs are append-only.
2.  **Safety:** Critical events are flushed to disk immediately.
3.  **Availability:** Log rotation prevents disk exhaustion (max 10MB per file).
4.  **Security:** Events are timestamped with Real Time.

Usage:
Initialize an 'AuditQueue' and pass it to threads. Use 'writeAudit' to log events.
-}
module Safety.Audit (
    AuditSeverity(..),
    AuditEvent(..),
    AuditQueue,
    writeAudit,
    auditLoop
) where

import Control.Concurrent.STM
import System.IO
import Data.Time.HighRes (getRealTimeNS)
import System.Directory (renameFile, doesFileExist)

-- | Severity level of the audit event.
data AuditSeverity
    = Info      -- ^ Routine operational events
    | Warning   -- ^ Unexpected behavior handled by safety mechanisms
    | Critical  -- ^ Safety violations or system failures
    deriving (Show, Eq)

-- | An audit event payload.
data AuditEvent = AuditEvent
    { eventSeverity :: AuditSeverity
    , eventSource   :: String
    , eventMessage  :: String
    } deriving (Show, Eq)

-- | Thread-safe queue for audit events.
-- Using TBQueue to provide backpressure if the disk is too slow,
-- though ideally we size it large enough (e.g. 1000 items).
type AuditQueue = TBQueue AuditEvent

-- | Write an event to the audit queue (STM).
-- Does not block, but will retry if queue is full.
writeAudit :: AuditQueue -> AuditSeverity -> String -> String -> STM ()
writeAudit q sev src msg = writeTBQueue q $ AuditEvent sev src msg

-- | The main loop for the audit thread.
-- Handles writing to disk, flushing critical events, and rotating logs.
auditLoop :: AuditQueue -> FilePath -> IO ()
auditLoop q logPath = go
  where
    go = do
        -- Open file in Append Mode
        withFile logPath AppendMode $ \h -> do
            hSetBuffering h LineBuffering
            loop h

        -- If loop returns, it means we need to rotate
        rotate
        go

    loop h = do
        -- Read event (blocking)
        event <- atomically $ readTBQueue q

        -- Get Receipt Time
        now <- getRealTimeNS

        -- Format: [TIMESTAMP][SEVERITY][SOURCE] Message
        -- Example: [1234567890123][Critical][Watchdog] Timeout exceeded
        let entry = "[" ++ show now ++ "][" ++ show (eventSeverity event) ++ "][" ++ eventSource event ++ "] " ++ eventMessage event

        hPutStrLn h entry

        -- Critical events demand immediate flush to ensure forensic data survives a crash
        case eventSeverity event of
            Critical -> hFlush h
            _        -> return ()

        -- Check for rotation (Max 10MB)
        size <- hFileSize h
        if size > 10 * 1024 * 1024
           then return () -- Break inner loop to trigger rotation (via withFile close)
           else loop h

    rotate = do
        exists <- doesFileExist logPath
        if exists
           then do
               now <- getRealTimeNS
               let newName = logPath ++ "." ++ show now
               renameFile logPath newName
           else return ()

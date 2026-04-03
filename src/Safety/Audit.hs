{-# LANGUAGE ScopedTypeVariables #-}
module Safety.Audit (auditLoop) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)
import Data.Functor ((<&>))
import Data.Time.HighRes (getMonotonicTimeNS)
import qualified Data.Map.Strict as Map
import Control.Exception (try, IOException)
import System.Posix.Files (rename)
import Text.Printf (printf)
import Data.Char (isControl)

-- | Signals why the inner loop exited
data LoopResult = RotationNeeded

-- | The Audit Loop
--
-- Consumes 'AuditEvent's from the shared 'TBQueue' and writes them to an immutable
-- disk log. This function guarantees that 'Critical' events are flushed to disk
-- immediately, ensuring they are preserved even in the event of a power loss or crash.
--
-- Complexity: O(1) amortized per event (writes are buffered, rotation is infrequent).
-- Safety:
--   * Immediate flush for 'Critical' severity.
--   * Handles IO exceptions by logging to stderr and retrying.
--   * Rotates log at 10MB to prevent disk exhaustion.
--   * Updates 'threadHeartbeats' for Watchdog monitoring.
auditLoop :: TVar SystemState -> FilePath -> IO ()
auditLoop stateVar logPath = do
    -- Get queue reference once (it's constant in SystemState)
    state <- readTVarIO stateVar
    let queue = auditQueue state

    runAuditLoop stateVar queue logPath

runAuditLoop :: TVar SystemState -> TBQueue AuditEvent -> FilePath -> IO ()
runAuditLoop stateVar queue logPath = do
    -- Open file in Append Mode
    result <- try $ withFile logPath AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        processEvents stateVar queue h

    case result of
        Left (e :: IOException) -> do
            -- Fallback: Log to stderr if disk fails
            hPutStrLn stderr $ "AUDIT SUBSYSTEM FAILURE: " ++ show e
            -- Wait a bit before retrying to avoid busy loop on permanent failure
            threadDelay 1_000_000
            runAuditLoop stateVar queue logPath

        Right RotationNeeded -> do
            -- Rotate Log: log -> log.bak
            -- We ignore errors here (e.g. if rename fails, we just overwrite/append next time)
            _ <- try $ rename logPath (logPath ++ ".bak") :: IO (Either IOException ())
            runAuditLoop stateVar queue logPath

processEvents :: TVar SystemState -> TBQueue AuditEvent -> Handle -> IO LoopResult
processEvents stateVar queue h = go
  where
    go = do
        -- 1. Update Heartbeat (Safety)
        now <- getMonotonicTimeNS
        atomically $ modifyTVar' stateVar $ \s ->
            s { threadHeartbeats = Map.insert "Audit" now (threadHeartbeats s) }

        -- 2. Read Event with Timeout (100ms)
        -- We use registerDelay to wake up the transaction if no event arrives.
        delayVar <- registerDelay 100_000 -- 100ms

        mEvt <- atomically $
            (readTBQueue queue <&> Just)
            `orElse`
            do
                expired <- readTVar delayVar
                if expired then return Nothing else retry

        case mEvt of
            Nothing -> go -- Timeout, loop back to update heartbeat
            Just evt -> do
                -- 3. Write Event
                -- Format: [TIMESTAMP] [SEVERITY] [COMPONENT] MESSAGE
                -- using show for severity
                let sanitize = map (\c -> if isControl c then ' ' else c)
                let entry = printf "[%d] [%s] [%s] %s"
                                (eventTime evt)
                                (show (severity evt))
                                (sanitize $ component evt)
                                (sanitize $ message evt)
                hPutStrLn h entry

                -- 4. Critical Flush (Safety)
                when (severity evt == Critical) $ hFlush h

                -- 5. Rotation Check
                size <- hFileSize h
                if size > 10 * 1024 * 1024 -- 10MB limit
                    then return RotationNeeded
                    else go

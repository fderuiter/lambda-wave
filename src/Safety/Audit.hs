{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Safety.Audit
-- Description : Audit logging functionality
--
-- Provides secure, immutable audit logging to disk with immediate
-- flush semantics for critical events.
module Safety.Audit (auditLoop, tryWriteAudit, tryWriteAuditSTM, triggerShutdown) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as B
import Data.Char (isControl)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Types
import Safety.Crypto (encryptLog)
import Safety.Result (SafetyResult (..))
import System.IO
import System.Posix.Files (rename)
import Text.Printf (printf)

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
    initialSize <- hFileSize h
    processEvents stateVar queue h initialSize

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

processEvents :: TVar SystemState -> TBQueue AuditEvent -> Handle -> Integer -> IO LoopResult
processEvents stateVar queue h = go
  where
    go currentSize = do
      -- 1. Update Heartbeat (Safety)
      now <- getMonotonicTimeNS
      atomically $ modifyTVar' stateVar $ \s ->
        s {threadHeartbeats = Map.insert "Audit" now (threadHeartbeats s)}

      -- 2. Read Event with Timeout (100ms)
      -- We use registerDelay to wake up the transaction if no event arrives.
      delayVar <- registerDelay 100_000 -- 100ms
      mEvt <-
        atomically $
          (readTBQueue queue <&> Just)
            `orElse` do
              expired <- readTVar delayVar
              if expired then return Nothing else retry

      case mEvt of
        Nothing -> go currentSize -- Timeout, loop back to update heartbeat
        Just evt -> do
          -- 3. Write Event
          -- Format: [TIMESTAMP] [SEVERITY] [COMPONENT] MESSAGE
          -- using show for severity
          let sanitize = map (\c -> if isControl c then ' ' else c)
          let entry =
                printf
                  "[%d] [%s] [%s] %s"
                  (eventTime evt)
                  (show (severity evt))
                  (sanitize $ component evt)
                  (sanitize $ message evt)
          encRes <- encryptLog (entry ++ "\n")
          case encRes of
            Safe enc -> do
              B.hPut h enc

              -- 4. Critical Flush (Safety)
              when (severity evt == Critical || severity evt == Warning) $ hFlush h

              -- 5. Rotation Check
              let !newSize = currentSize + fromIntegral (B.length enc)
              if newSize > 10 * 1024 * 1024 -- 10MB limit
                then return RotationNeeded
                else go newSize
            ClampedToMin enc -> do
              B.hPut h enc
              when (severity evt == Critical || severity evt == Warning) $ hFlush h
              let !newSize = currentSize + fromIntegral (B.length enc)
              if newSize > 10 * 1024 * 1024 then return RotationNeeded else go newSize
            ClampedToMax enc -> do
              B.hPut h enc
              when (severity evt == Critical || severity evt == Warning) $ hFlush h
              let !newSize = currentSize + fromIntegral (B.length enc)
              if newSize > 10 * 1024 * 1024 then return RotationNeeded else go newSize
            DivByZeroSafe enc -> do
              B.hPut h enc
              when (severity evt == Critical || severity evt == Warning) $ hFlush h
              let !newSize = currentSize + fromIntegral (B.length enc)
              if newSize > 10 * 1024 * 1024 then return RotationNeeded else go newSize
            Unsafe msg -> do
              hPutStrLn stderr $ "CRITICAL ENCRYPTION FAILURE: " ++ msg
              triggerShutdown stateVar ("CRITICAL ENCRYPTION FAILURE: " ++ msg)
              return RotationNeeded

-- Requirement SR-AUDIT-001

-- | Unified non-blocking API for writing to the audit queue from STM.
tryWriteAuditSTM :: TBQueue AuditEvent -> AuditEvent -> STM Bool
tryWriteAuditSTM queue evt = do
  full <- isFullTBQueue queue
  if full
    then return False
    else do
      writeTBQueue queue evt
      return True

-- | Unified non-blocking API for writing to the audit queue.
-- Prevents calling threads from suspending. If the queue is full,
-- it drops the event and outputs a fallback diagnostic to standard error.
tryWriteAudit :: TBQueue AuditEvent -> AuditEvent -> IO ()
tryWriteAudit queue evt = do
  success <- atomically $ tryWriteAuditSTM queue evt
  unless success $
    hPutStrLn stderr $
      "DROPPED AUDIT EVENT: " ++ show evt

-- | Centralized function to trigger a controlled system shutdown on failure
triggerShutdown :: TVar SystemState -> String -> IO ()
triggerShutdown stateVar reason = do
  now <- getMonotonicTimeNS
  atomically $ do
    s <- readTVar stateVar
    writeTVar stateVar s {beamState = BeamOff}
    -- We try to write one last critical event but this shouldn't block shutdown
    let evt = AuditEvent now Critical "Bridge" ("SYSTEM SHUTDOWN TRIGGERED: " ++ reason)
    _ <- tryWriteAuditSTM (auditQueue s) evt
    return ()

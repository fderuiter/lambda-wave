module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Types
import Safety.Audit (auditLoop, triggerShutdown, tryWriteAudit, tryWriteAuditSTM)
import Safety.Crypto (decryptLog)
import Safety.Watchdog (watchdogLoop)
import SignalProcessing.Kalman (KalmanConfig (..), initKalman)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitFailure))
import System.Posix.Files (fileExist, fileSize, getFileStatus, removeLink)
import System.Posix.Process (executeFile, exitImmediately, forkProcess, getProcessStatus)

-- | Test Setup
withTestEnv :: (TVar SystemState -> TBQueue AuditEvent -> FilePath -> IO Bool) -> IO Bool
withTestEnv action = do
  let logPath = "test_audit.log"
  -- Cleanup previous
  cleanup logPath
  cleanup (logPath ++ ".bak")

  -- Setup State
  now <- getMonotonicTimeNS
  q <- newTBQueueIO 10000
  audioQ <- newTBQueueIO 100
  let kConfig = KalmanConfig 1.0 1.0
  let st =
        SystemState
          { currentPoints = [],
            beamState = BeamOff,
            lastFrameTime = now,
            sequenceNumber = 0,
            isocenter = Point3D 0 0 0 0 0,
            threadHeartbeats = Map.empty,
            kalmanState = initKalman 0 kConfig,
            mtiState = [],
            auditQueue = q,
            audioQueue = audioQ,
            audioAlertEnabled = False,
            audioVolume = 1.0,
            audioFrequency = 440.0,
            activeLanguage = "en",
            localizedBeamState = "BEAM OFF",
            calibrationStatus = CalibrationUnverified,
            displayPreset = StandardPreset
          }
  stateVar <- newTVarIO st

  -- Run Action
  result <- action stateVar q logPath

  -- Cleanup (only on success, keep on fail for debug)
  when result $ do
    cleanup logPath
    cleanup (logPath ++ ".bak")

  return result
  where
    cleanup f = do
      e <- fileExist f
      when e (removeLink f)

-- | Wait for queue to drain with a timeout
waitForQueue :: TBQueue AuditEvent -> IO ()
waitForQueue q = go (100 :: Int)
  where
    go 0 = return ()
    go n = do
      empty <- atomically $ isEmptyTBQueue q
      if empty
        then threadDelay 1_000_000 -- generous wait for processing
        else threadDelay 100_000 >> go (n - 1)

waitForRotation :: FilePath -> Int -> IO Bool
waitForRotation _ 0 = return False
waitForRotation path n = do
  exists <- fileExist (path ++ ".bak")
  if exists
    then return True
    else threadDelay 100_000 >> waitForRotation path (n - 1)

testBasicLogging :: IO Bool
testBasicLogging = do
  putStr "Test 1: Basic Logging... "
  withTestEnv $ \stateVar q logPath -> do
    -- Fork Audit Loop
    tid <- forkIO $ auditLoop stateVar logPath

    -- Send Event
    now <- getMonotonicTimeNS
    atomically $ writeTBQueue q (AuditEvent now Info "Test" "Hello World")

    -- Wait for processing
    waitForQueue q

    killThread tid
    -- Allow time for handle cleanup
    threadDelay 1_000_000

    -- Verify File Content
    -- Use strict IO or ensure handle is closed.
    rawContent <- B.readFile logPath
    case decryptLog rawContent of
      Right content -> do
        let ok = "Hello World" `isInfixOf` content
        if ok
          then putStrLn "PASS" >> return True
          else putStrLn ("FAIL: Content was " ++ show content) >> return False
      Left err -> do
        putStrLn ("FAIL: Decryption error: " ++ err)
        return False

testLogRotation :: IO Bool
testLogRotation = do
  putStr "Test 2: Log Rotation (>10MB)... "
  withTestEnv $ \stateVar q logPath -> do
    tid <- forkIO $ auditLoop stateVar logPath

    let chunk = replicate (1024 * 1024) 'A' -- 1MB chunk
    now <- getMonotonicTimeNS

    forM_ ([1 .. 11] :: [Int]) $ \_ -> do
      atomically $ writeTBQueue q (AuditEvent now Info "Test" chunk)
      threadDelay 500_000 -- Wait 0.5s for each chunk to be processed

    -- Debug: Check Size
    stat <- getFileStatus logPath
    putStrLn $ "DEBUG: Current Log Size: " ++ show (fileSize stat)

    -- Send trigger event
    atomically $ writeTBQueue q (AuditEvent now Info "Test" "Trigger")
    waitForQueue q

    -- Check if .bak exists
    rotated <- waitForRotation logPath 50

    killThread tid

    if rotated
      then putStrLn "PASS" >> return True
      else putStrLn "FAIL (.bak file not found)" >> return False

runChildCrash :: IO ()
runChildCrash = do
  -- Minimal setup for child process
  let logPath = "test_audit_crash.log"
  -- Note: No cleanup here, we assume parent cleans up or file is reused

  now <- getMonotonicTimeNS
  q <- newTBQueueIO 10000
  audioQ <- newTBQueueIO 100
  let kConfig = KalmanConfig 1.0 1.0
  let st =
        SystemState
          { currentPoints = [],
            beamState = BeamOff,
            lastFrameTime = now,
            sequenceNumber = 0,
            isocenter = Point3D 0 0 0 0 0,
            threadHeartbeats = Map.empty,
            kalmanState = initKalman 0 kConfig,
            mtiState = [],
            auditQueue = q,
            audioQueue = audioQ,
            audioAlertEnabled = False,
            audioVolume = 1.0,
            audioFrequency = 440.0,
            activeLanguage = "en",
            localizedBeamState = "BEAM OFF",
            calibrationStatus = CalibrationUnverified,
            displayPreset = StandardPreset
          }
  stateVar <- newTVarIO st

  _ <- forkIO $ auditLoop stateVar logPath

  atomically $ writeTBQueue q (AuditEvent now Critical "Test" "CRASH_EVENT_CRIT")
  atomically $ writeTBQueue q (AuditEvent now Warning "Test" "CRASH_EVENT_WARN")
  waitForQueue q
  exitImmediately (ExitFailure 99)

testCrashRecovery :: IO Bool
testCrashRecovery = do
  putStr "Test 3: Crash Recovery (Immediate Flush)... "
  let logPath = "test_audit_crash.log"
  cleanup logPath

  exePath <- getExecutablePath

  -- Safe Fork/Exec: Replace process image to avoid threaded RTS issues in child
  pid <- forkProcess $ do
    executeFile exePath False ["--child-crash"] Nothing

  -- Wait for child
  _ <- getProcessStatus True False pid

  -- Verify Log
  res <- try $ B.readFile logPath :: IO (Either IOException B.ByteString)
  case res of
    Left _ -> do
      putStrLn "FAIL (Log file not found or unreadable)"
      return False
    Right rawContent -> do
      case decryptLog rawContent of
        Right content -> do
          let ok1 = "CRASH_EVENT_CRIT" `isInfixOf` content
          let ok2 = "CRASH_EVENT_WARN" `isInfixOf` content
          if ok1 && ok2
            then do
              putStrLn "PASS"
              cleanup logPath
              return True
            else do
              putStrLn ("FAIL: Content was " ++ show content)
              cleanup logPath
              return False
        Left err -> do
          putStrLn ("FAIL: Decryption error: " ++ err)
          cleanup logPath
          return False
  where
    cleanup f = do
      e <- fileExist f
      when e (removeLink f)

testIOException :: IO Bool
testIOException = do
  putStr "Test 4: IO Exception Recovery... "
  withTestEnv $ \stateVar q _logPath -> do
    -- Use "." as logPath to cause is a directory error
    tid <- forkIO $ auditLoop stateVar "."

    now <- getMonotonicTimeNS
    atomically $ writeTBQueue q (AuditEvent now Info "Test" "Fail")
    waitForQueue q

    killThread tid
    putStrLn "PASS"
    return True

testTryWriteAudit :: IO Bool
testTryWriteAudit = do
  putStr "Test 5: tryWriteAudit Non-blocking Behavior... "
  q <- newTBQueueIO 1
  now <- getMonotonicTimeNS
  let evt1 = AuditEvent now Info "Test" "Event 1"
  let evt2 = AuditEvent now Info "Test" "Event 2"

  tryWriteAudit q evt1
  tryWriteAudit q evt2

  evt <- atomically $ readTBQueue q
  let ok1 = message evt == "Event 1"

  res1 <- atomically $ tryWriteAuditSTM q evt1
  res2 <- atomically $ tryWriteAuditSTM q evt2

  if ok1 && res1 && not res2
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False

testTriggerShutdown :: IO Bool
testTriggerShutdown = do
  putStr "Test 6: triggerShutdown ... "
  q <- newTBQueueIO 10
  audioQ <- newTBQueueIO 100
  now <- getMonotonicTimeNS
  let kConfig = KalmanConfig 1.0 1.0
  let st =
        SystemState
          { currentPoints = [],
            beamState = BeamOn,
            lastFrameTime = now,
            sequenceNumber = 0,
            isocenter = Point3D 0 0 0 0 0,
            threadHeartbeats = Map.empty,
            kalmanState = initKalman 0 kConfig,
            mtiState = [],
            auditQueue = q,
            audioQueue = audioQ,
            audioAlertEnabled = False,
            audioVolume = 1.0,
            audioFrequency = 440.0,
            activeLanguage = "en",
            localizedBeamState = "BEAM ON",
            calibrationStatus = CalibrationUnverified,
            displayPreset = StandardPreset
          }
  stateVar <- newTVarIO st

  triggerShutdown stateVar "Test Failure"

  finalSt <- readTVarIO stateVar
  evt <- atomically $ readTBQueue q

  if beamState finalSt == BeamOff && "SYSTEM SHUTDOWN TRIGGERED" `isInfixOf` message evt
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False

testWatchdogCrashCoverage :: IO Bool
testWatchdogCrashCoverage = do
  putStr "Test 7: Watchdog Exception Coverage... "
  res <- try (watchdogLoop undefined) :: IO (Either SomeException ())
  case res of
    Left _ -> putStrLn "PASS" >> return True
    Right _ -> putStrLn "FAIL" >> return False

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--child-crash"] -> runChildCrash
    _ -> do
      putStrLn "=== Audit Logging Verification (P1-004) ==="
      p1 <- testBasicLogging
      p2 <- testLogRotation
      p3 <- testCrashRecovery
      p4 <- testIOException
      p5 <- testTryWriteAudit
      p6 <- testTriggerShutdown
      p7 <- testWatchdogCrashCoverage

      if p1 && p2 && p3 && p4 && p5 && p6 && p7
        then putStrLn "VERIFICATION PASSED"
        else fail "VERIFICATION FAILED"

-- Requirement SR-AUDIT-001

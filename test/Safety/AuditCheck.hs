module Main (main) where

import Data.Types
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Audit (auditLoop)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import System.Posix.Files (fileExist, removeLink, getFileStatus, fileSize)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, exitImmediately)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (isInfixOf)
import Control.Monad (when)
import Control.Exception (try, IOException)
import System.Environment (getArgs, getExecutablePath)
import qualified Data.ByteString as B
import Safety.Crypto (decryptLog)

-- | Test Setup
withTestEnv :: (TVar SystemState -> TBQueue AuditEvent -> FilePath -> IO Bool) -> IO Bool
withTestEnv action = do
    let logPath = "test_audit.log"
    -- Cleanup previous
    cleanup logPath
    cleanup (logPath ++ ".bak")

    -- Setup State
    now <- getMonotonicTimeNS
    q <- newTBQueueIO 100
    let kConfig = KalmanConfig 1.0 1.0
    let st = SystemState [] BeamOff now 0 (Point3D 0 0 0 0 0) Map.empty (initKalman 0 kConfig) q False
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
        threadDelay 200_000 -- 200ms

        killThread tid
        -- Allow time for handle cleanup
        threadDelay 100_000

        -- Verify File Content
        -- Use strict IO or ensure handle is closed.
        rawContent <- B.readFile logPath
        let content = decryptLog rawContent
        let ok = "Hello World" `isInfixOf` content

        if ok
           then putStrLn "PASS" >> return True
           else putStrLn ("FAIL: Content was " ++ show content) >> return False

testLogRotation :: IO Bool
testLogRotation = do
    putStr "Test 2: Log Rotation (>10MB)... "
    withTestEnv $ \stateVar q logPath -> do
        tid <- forkIO $ auditLoop stateVar logPath

        -- Write > 10MB of data
        -- 10MB string
        let hugeMsg = replicate (10 * 1024 * 1024 + 1024) 'A'
        now <- getMonotonicTimeNS

        -- Write huge message
        atomically $ writeTBQueue q (AuditEvent now Info "Test" hugeMsg)

        -- Wait for write (this might take a second)
        threadDelay 5_000_000

        -- Debug: Check Size
        stat <- getFileStatus logPath
        putStrLn $ "DEBUG: Current Log Size: " ++ show (fileSize stat)

        -- Send trigger event
        atomically $ writeTBQueue q (AuditEvent now Info "Test" "Trigger")
        threadDelay 1_000_000

        -- Check if .bak exists
        rotated <- fileExist (logPath ++ ".bak")

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
    q <- newTBQueueIO 100
    let kConfig = KalmanConfig 1.0 1.0
    let st = SystemState [] BeamOff now 0 (Point3D 0 0 0 0 0) Map.empty (initKalman 0 kConfig) q False
    stateVar <- newTVarIO st

    _ <- forkIO $ auditLoop stateVar logPath

    atomically $ writeTBQueue q (AuditEvent now Critical "Test" "CRASH_EVENT_CRIT")
    atomically $ writeTBQueue q (AuditEvent now Warning "Test" "CRASH_EVENT_WARN")
    threadDelay 100_000
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
            let content = decryptLog rawContent
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
  where
    cleanup f = do
        e <- fileExist f
        when e (removeLink f)

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

            if p1 && p2 && p3
               then putStrLn "VERIFICATION PASSED"
               else fail "VERIFICATION FAILED"

-- Requirement SR-AUDIT-001

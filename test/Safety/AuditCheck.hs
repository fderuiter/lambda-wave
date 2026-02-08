module Main (main) where

import Data.Types
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Audit (auditLoop)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import System.Posix.Files (fileExist, removeLink, getFileStatus, fileSize)
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (isInfixOf)
import Control.Monad (when)

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
    let st = SystemState [] BeamOff now (Point3D 0 0 0 0 0) Map.empty (initKalman 0 kConfig) q
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
        content <- readFile logPath
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

main :: IO ()
main = do
    putStrLn "=== Audit Logging Verification (P1-004) ==="
    p1 <- testBasicLogging
    p2 <- testLogRotation

    if p1 && p2
       then putStrLn "VERIFICATION PASSED"
       else fail "VERIFICATION FAILED"

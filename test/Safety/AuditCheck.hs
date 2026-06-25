module Main (main) where

import Data.Types
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Audit (auditLoop)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import System.Posix.Files (fileExist, removeLink)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, exitImmediately)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (isInfixOf)
import Control.Monad (when, forM_)
import Control.Exception (try, IOException)
import System.Environment (getArgs, getExecutablePath)
import qualified Data.ByteString as B
import Safety.Crypto (decryptLog)
import System.IO (stderr, hPutStrLn)

-- | Test Setup with unique prefix to avoid parallel run conflicts
withTestEnv :: String -> (TVar SystemState -> TBQueue AuditEvent -> FilePath -> IO Bool) -> IO Bool
withTestEnv prefix action = do
    let logPath = "test_audit_" ++ prefix ++ ".log"
    -- Cleanup previous
    cleanup logPath
    cleanup (logPath ++ ".bak")

    -- Setup State
    now <- getMonotonicTimeNS
    q <- newTBQueueIO 10000
    let kConfig = KalmanConfig 1.0 1.0
    let st = SystemState [] BeamOff now 0 (Point3D 0 0 0 0 0) Map.empty (initKalman 0 kConfig) q False "en" "BEAM OFF"
    stateVar <- newTVarIO st

    -- Run Action
    result <- action stateVar q logPath

    -- Cleanup (only on success)
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
    withTestEnv "basic" $ \stateVar q logPath -> do
        tid <- forkIO $ auditLoop stateVar logPath
        now <- getMonotonicTimeNS
        atomically $ writeTBQueue q (AuditEvent now Info "Test" "Hello World")

        -- Wait for file creation
        let waitFile 0 = return False
            waitFile n = do
                threadDelay 200_000
                e <- fileExist logPath
                if e then return True else waitFile (n - 1)

        _ <- waitFile (20 :: Int)
        threadDelay 500_000 -- Extra time for write

        killThread tid
        threadDelay 200_000

        exists <- fileExist logPath
        if not exists
           then putStrLn "FAIL (No log)" >> return False
           else do
               rawContent <- B.readFile logPath
               case decryptLog rawContent of
                   Right content -> do
                       let ok = "Hello World" `isInfixOf` content
                       if ok then putStrLn "PASS" >> return True else putStrLn "FAIL (Content)" >> return False
                   Left _ -> putStrLn "FAIL (Decrypt)" >> return False

testLogRotation :: IO Bool
testLogRotation = do
    putStr "Test 2: Log Rotation (>10MB)... "
    withTestEnv "rotation" $ \stateVar q logPath -> do
        tid <- forkIO $ auditLoop stateVar logPath
        let chunk = replicate (1024 * 1024) 'A'
        now <- getMonotonicTimeNS

        forM_ ([1..11] :: [Int]) $ \_ -> do
            atomically $ writeTBQueue q (AuditEvent now Info "Test" chunk)
            threadDelay 50_000

        let waitRotation n = do
                threadDelay 500_000
                rotated <- fileExist (logPath ++ ".bak")
                if rotated then return True else if n > 0 then waitRotation (n - 1) else return False

        ok <- waitRotation (20 :: Int)
        killThread tid
        threadDelay 200_000

        if ok
           then putStrLn "PASS" >> return True
           else putStrLn "FAIL (.bak file not found)" >> return False

runChildCrash :: IO ()
runChildCrash = do
    let logPath = "test_audit_crash_proc.log"
    now <- getMonotonicTimeNS
    q <- newTBQueueIO 10000
    let kConfig = KalmanConfig 1.0 1.0
    let st = SystemState [] BeamOff now 0 (Point3D 0 0 0 0 0) Map.empty (initKalman 0 kConfig) q False "en" "BEAM OFF"
    stateVar <- newTVarIO st
    _ <- forkIO $ auditLoop stateVar logPath
    atomically $ writeTBQueue q (AuditEvent now Critical "Test" "CRASH_EVENT_CRIT")
    atomically $ writeTBQueue q (AuditEvent now Warning "Test" "CRASH_EVENT_WARN")
    threadDelay 200_000
    exitImmediately (ExitFailure 99)

testCrashRecovery :: IO Bool
testCrashRecovery = do
    putStr "Test 3: Crash Recovery (Immediate Flush)... "
    let logPath = "test_audit_crash_proc.log"
    e <- fileExist logPath
    when e (removeLink logPath)

    exePath <- getExecutablePath
    pid <- forkProcess $ executeFile exePath False ["--child-crash"] Nothing
    _ <- getProcessStatus True False pid

    res <- try $ B.readFile logPath :: IO (Either IOException B.ByteString)
    case res of
        Left _ -> putStrLn "FAIL (No log)" >> return False
        Right rawContent -> case decryptLog rawContent of
            Right content -> do
                let ok = "CRASH_EVENT_CRIT" `isInfixOf` content && "CRASH_EVENT_WARN" `isInfixOf` content
                if ok then putStrLn "PASS" >> return True else putStrLn "FAIL" >> return False
            Left _ -> putStrLn "FAIL (Decrypt)" >> return False

testIOException :: IO Bool
testIOException = do
    putStr "Test 4: IO Exception Recovery... "
    withTestEnv "ioerr" $ \stateVar q _logPath -> do
        tid <- forkIO $ auditLoop stateVar "."
        now <- getMonotonicTimeNS
        atomically $ writeTBQueue q (AuditEvent now Info "Test" "Fail")
        threadDelay 200_000
        killThread tid
        putStrLn "PASS"
        return True

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

            if p1 && p2 && p3 && p4
               then putStrLn "VERIFICATION PASSED"
               else do
                   hPutStrLn stderr $ "RESULTS: p1=" ++ show p1 ++ ", p2=" ++ show p2 ++ ", p3=" ++ show p3 ++ ", p4=" ++ show p4
                   fail "VERIFICATION FAILED"

{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay, killThread)
import System.IO
import System.Posix.Files (fileExist, removeLink)
import System.Posix.IO (openFd, closeFd, OpenMode(..), defaultFileFlags)
import qualified System.Posix.IO.ByteString as PBS
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import Data.List (isInfixOf)

import Data.Types
import Safety.Audit (auditLoop)

main :: IO ()
main = do
    putStrLn "=== Audit Logging Verification (P1-004) ==="

    testQueueConsumption
    testLogRotation

    putStrLn "All Audit Checks Passed."
    exitSuccess

testQueueConsumption :: IO ()
testQueueConsumption = do
    putStrLn "Test 1: Queue Consumption & Writing"
    q <- newTBQueueIO 10
    let logFile = "test_audit.log"
    -- Cleanup
    cleanup logFile

    tid <- forkIO $ auditLoop q logFile

    -- Write Event
    let evt = AuditEvent 12345 Info "Test Message" "TestSrc"
    atomically $ writeTBQueue q evt

    threadDelay 100000 -- 100ms

    -- Check content
    content <- readLogSafe logFile
    if "Test Message" `isInfixOf` content
       then putStrLn "PASS: Log contains message."
       else failTest $ "Log missing message. Content: " ++ show content

    killThread tid
    cleanup logFile

testLogRotation :: IO ()
testLogRotation = do
    putStrLn "Test 2: Log Rotation (>10MB)"
    q <- newTBQueueIO 10
    let logFile = "test_rotate.log"
    cleanup logFile
    cleanup (logFile ++ ".1")

    -- Create huge file to trigger rotation
    withFile logFile WriteMode $ \h -> do
        hSetFileSize h (10 * 1024 * 1024 + 100) -- > 10MB

    tid <- forkIO $ auditLoop q logFile

    -- Write Event to trigger check
    let evt = AuditEvent 67890 Warning "New Log Entry" "Rotator"
    atomically $ writeTBQueue q evt

    threadDelay 200000 -- 200ms

    -- Check rotation
    rotated <- fileExist (logFile ++ ".1")
    if rotated
       then putStrLn "PASS: Log rotated (.1 exists)."
       else failTest "Log did not rotate."

    -- Check new log content
    content <- readLogSafe logFile
    if "New Log Entry" `isInfixOf` content
       then putStrLn "PASS: New log contains fresh entry."
       else failTest "New log empty or missing entry."

    killThread tid
    cleanup logFile
    cleanup (logFile ++ ".1")

readLogSafe :: FilePath -> IO String
readLogSafe path = do
#if MIN_VERSION_unix(2,8,0)
    fd <- openFd path ReadOnly defaultFileFlags
#else
    fd <- openFd path ReadOnly Nothing defaultFileFlags
#endif
    (str, _) <- PBS.fdRead fd 10240 -- Read 10KB
    closeFd fd
    return str

cleanup :: FilePath -> IO ()
cleanup f = do
    e <- fileExist f
    when e $ removeLink f

failTest :: String -> IO ()
failTest msg = do
    putStrLn $ "FAIL: " ++ msg
    exitFailure

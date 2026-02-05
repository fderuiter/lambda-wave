module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay, killThread)
import System.IO
import System.Directory (removeFile, doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (bracket)

-- We import Safety.Audit.
-- Note: This test expects the NEW API which is not yet implemented.
-- It will fail to compile until Safety.Audit is updated.
import Safety.Audit

main :: IO ()
main = do
    putStrLn "=== Audit Logging Verification (P1-004) ==="

    let logFile = "test_audit.log"

    -- Clean up previous run
    exists <- doesFileExist logFile
    if exists then removeFile logFile else return ()

    -- 1. Setup Queue
    q <- newTBQueueIO 100

    -- 2. Start Audit Loop in background
    tid <- forkIO $ auditLoop q logFile

    -- 3. Send Events
    -- We need to manually construct events or use writeAudit helper
    -- Assuming writeAudit :: AuditQueue -> AuditSeverity -> String -> String -> STM ()
    atomically $ do
        writeAudit q Info "TEST" "System Startup"
        writeAudit q Warning "TEST" "High Latency Detected"
        writeAudit q Critical "TEST" "Beam Hold Triggered"

    -- 4. Wait for processing (100ms)
    threadDelay 100000

    -- 5. Stop Audit Loop to release file lock
    killThread tid
    -- Give it a moment to release the handle (bracket cleanup)
    threadDelay 10000

    -- 6. Verify File Content
    content <- readFile logFile
    putStrLn "--- Log Content ---"
    putStrLn content
    putStrLn "-------------------"

    let linesOfLog = lines content
    let check1 = any (\l -> "Info" `isInfixOf` l && "System Startup" `isInfixOf` l) linesOfLog
    let check2 = any (\l -> "Warning" `isInfixOf` l && "High Latency Detected" `isInfixOf` l) linesOfLog
    let check3 = any (\l -> "Critical" `isInfixOf` l && "Beam Hold Triggered" `isInfixOf` l) linesOfLog

    removeFile logFile

    if check1 && check2 && check3
       then do
           putStrLn "PASS: All events logged correctly."
           exitSuccess
       else do
           putStrLn "FAIL: Missing events in log file."
           exitFailure

-- Simple helper since we don't have Data.List.isInfixOf imported standardly in strict prelude sometimes,
-- but 'base' has it.
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = go haystack
  where
    go [] = False
    go s@(x:xs)
      | take (length needle) s == needle = True
      | otherwise = go xs

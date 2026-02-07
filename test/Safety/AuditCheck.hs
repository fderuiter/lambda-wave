{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import System.Directory (removeFile, doesFileExist)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, fdRead, closeFd)
import Control.Monad (when)
import Data.Time.HighRes (getRealTimeNS)

import Data.Types
import Safety.Audit (auditLoop)

-- | Test Constants
logFile :: FilePath
logFile = "test_audit.log"

readContent :: FilePath -> IO String
readContent path = do
#if MIN_VERSION_unix(2,8,0)
    fd <- openFd path ReadOnly defaultFileFlags
#else
    fd <- openFd path ReadOnly Nothing defaultFileFlags
#endif
    (str, _) <- fdRead fd 100000 -- Read 100KB
    closeFd fd
    return str

main :: IO ()
main = do
    putStrLn "=== Audit Logging Verification (P1-004) ==="

    -- Cleanup previous run
    cleanUp

    -- 1. Setup
    queue <- newTBQueueIO 100

    -- Fork Audit Loop
    tid <- forkIO $ auditLoop queue logFile
    putStrLn "[Test] Audit Loop Started."
    threadDelay 10000 -- Wait for thread start

    -- 2. Test Basic Logging
    putStrLn "[Test] Writing Events..."
    now <- getRealTimeNS
    let event1 = AuditEvent now Info "Test" "Message 1"
    atomically $ writeTBQueue queue event1

    threadDelay 100000 -- Wait for flush/write (100ms)

    -- Verify content
    content <- readContent logFile
    if "Message 1" `isInfixOf` content
        then putStrLn "PASS: Event 1 found in log."
        else putStrLn $ "FAIL: Event 1 not found. Content: " ++ show content

    -- 3. Test Flushing (Critical Event)
    putStrLn "[Test] Testing Flush (Critical)..."
    let event2 = AuditEvent now Critical "Test" "CRITICAL FAILURE"
    atomically $ writeTBQueue queue event2

    -- We expect immediate flush.
    threadDelay 10000
    content2 <- readContent logFile
    if "CRITICAL FAILURE" `isInfixOf` content2
        then putStrLn "PASS: Critical Event flushed."
        else putStrLn "FAIL: Critical Event not found immediately."

    -- Cleanup
    killThread tid
    cleanUp
    putStrLn "=== Audit Check Complete ==="

cleanUp :: IO ()
cleanUp = do
    exists <- doesFileExist logFile
    when exists $ removeFile logFile

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = go haystack
  where
    go [] = False
    go str@(_:xs)
      | needle `isPrefixOf` str = True
      | otherwise = go xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (a:as) (b:bs)
      | a == b = isPrefixOf as bs
      | otherwise = False

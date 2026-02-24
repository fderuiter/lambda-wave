{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Exception (try, SomeException)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, getWriteOffset)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "Running Sentinel Checks..."

    -- 1. Test HighRes Time Safety
    putStrLn "[Test] HighRes Time Return Codes..."
    t1 <- try getMonotonicTimeNS
    case t1 of
        Left e -> do
            putStrLn $ "FAIL: getMonotonicTimeNS crashed: " ++ show (e :: SomeException)
            exitFailure
        Right val -> putStrLn $ "PASS: getMonotonicTimeNS returned " ++ show val

    t2 <- try getRealTimeNS
    case t2 of
        Left e -> do
            putStrLn $ "FAIL: getRealTimeNS crashed: " ++ show (e :: SomeException)
            exitFailure
        Right val -> putStrLn $ "PASS: getRealTimeNS returned " ++ show val

    -- 2. Test RingBuffer Creation (Invalid Size)
    putStrLn "[Test] RingBuffer Invalid Size..."
    res <- createRingBuffer 0
    case res of
        Left err -> putStrLn $ "PASS: createRingBuffer(0) returned error: " ++ err
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(0) succeeded unexpectedly"
             exitFailure

    res2 <- createRingBuffer (-100)
    case res2 of
        Left err -> putStrLn $ "PASS: createRingBuffer(-100) returned error: " ++ err
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(-100) succeeded unexpectedly"
             exitFailure

    -- 3. Test RingBuffer Creation (Valid)
    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    res3 <- createRingBuffer 1024
    case res3 of
        Left err -> do
            putStrLn $ "FAIL: createRingBuffer(1024) failed: " ++ err
            exitFailure
        Right fp -> do
            putStrLn "PASS: createRingBuffer(1024) succeeded"

            -- 4. Test FFI Interaction (getWriteOffset)
            -- This verifies the pointer is valid and C++ object is alive.
            offset <- getWriteOffset fp
            if offset == 0
               then putStrLn "PASS: Initial getWriteOffset is 0"
               else do
                   putStrLn $ "FAIL: Initial getWriteOffset is " ++ show offset
                   exitFailure

    -- 5. Storable Layout Tests Removed
    -- RingBufferControl Storable instance was removed to prevent unsafe atomic access.
    putStrLn "[Info] RingBufferControl Storable tests skipped (Safe by Design)."

    putStrLn "Sentinel Checks Complete. All Systems Safe."
    exitSuccess

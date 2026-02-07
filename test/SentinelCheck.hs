{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Exception (try, SomeException)
-- Removed Control.Monad (when)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, getWriteOffset)
import FFI.RingBuffer.Types (RingBufferControl)
import Foreign.Storable (sizeOf, alignment)
import Foreign.C.Types (CSize, CChar)
import Foreign.Ptr (Ptr)
-- Removed Foreign.ForeignPtr
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
    res <- try $ createRingBuffer 0
    case res of
        Left e -> putStrLn $ "PASS: createRingBuffer(0) threw exception: " ++ show (e :: SomeException)
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(0) succeeded unexpectedly"
             exitFailure

    res2 <- try $ createRingBuffer (-100)
    case res2 of
        Left e -> putStrLn $ "PASS: createRingBuffer(-100) threw exception: " ++ show (e :: SomeException)
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(-100) succeeded unexpectedly"
             exitFailure

    -- 3. Test RingBuffer Creation (Valid)
    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    fp <- createRingBuffer 1024
    putStrLn "PASS: createRingBuffer(1024) succeeded"

    -- 4. Test FFI Interaction (getWriteOffset)
    -- This verifies the pointer is valid and C++ object is alive.
    offset <- getWriteOffset fp
    if offset == 0
       then putStrLn "PASS: Initial getWriteOffset is 0"
       else do
           putStrLn $ "FAIL: Initial getWriteOffset is " ++ show offset
           exitFailure

    -- 5. Test RingBufferControl Layout
    putStrLn "[Test] RingBufferControl Layout..."
    let rbSize = sizeOf (undefined :: RingBufferControl)
        rbAlign = alignment (undefined :: RingBufferControl)
        csizeSize = sizeOf (undefined :: CSize)
        ptrSize = sizeOf (undefined :: Ptr CChar)

    putStrLn $ "RingBufferControl Size: " ++ show rbSize
    putStrLn $ "RingBufferControl Alignment: " ++ show rbAlign
    putStrLn $ "CSize size: " ++ show csizeSize
    putStrLn $ "Ptr size: " ++ show ptrSize

    -- Basic sanity check
    let minSize = 2 * csizeSize + ptrSize + csizeSize -- 4 fields
    if rbSize < minSize
        then do
            putStrLn $ "FAIL: RingBufferControl size (" ++ show rbSize ++ ") smaller than sum of fields (" ++ show minSize ++ ")"
            exitFailure
        else putStrLn "PASS: RingBufferControl size reasonable"

    putStrLn "Sentinel Checks Complete. All Systems Safe."
    exitSuccess

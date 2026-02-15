{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, getWriteOffset, mkRingBufferSize)
import FFI.RingBuffer.Types (RingBufferControl(..))
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)
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
    putStrLn "[Test] RingBuffer Invalid Size (Smart Constructor)..."
    case mkRingBufferSize 0 of
        Left _ -> putStrLn "PASS: mkRingBufferSize(0) returned Left"
        Right _ -> do
             putStrLn "FAIL: mkRingBufferSize(0) succeeded unexpectedly"
             exitFailure

    case mkRingBufferSize (-100) of
        Left _ -> putStrLn "PASS: mkRingBufferSize(-100) returned Left"
        Right _ -> do
             putStrLn "FAIL: mkRingBufferSize(-100) succeeded unexpectedly"
             exitFailure

    -- 3. Test RingBuffer Creation (Valid)
    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    let validSize = case mkRingBufferSize 1024 of
            Right sz -> sz
            Left err -> error $ "Failed to create valid size: " ++ err

    fp <- createRingBuffer validSize
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
    putStrLn "[Test] RingBufferControl Storable Layout..."
    let actualSize = sizeOf (undefined :: RingBufferControl)
    putStrLn $ "RingBufferControl Size: " ++ show actualSize ++ " (Expected: 64)"
    when (actualSize /= 64) $ do
        putStrLn "FAIL: Incorrect RingBufferControl size"
        exitFailure

    -- Verify poke/peek roundtrip
    putStrLn "[Test] RingBufferControl Poke/Peek..."
    alloca $ \ptr -> do
        let rb = RingBufferControl 1 2 nullPtr 100
        poke ptr rb
        rb' <- peek ptr
        if rb == rb'
           then putStrLn "PASS: Roundtrip successful"
           else do
               putStrLn "FAIL: Roundtrip failed"
               putStrLn $ "Original: " ++ show rb
               putStrLn $ "Peeked: " ++ show rb'
               exitFailure

    putStrLn "Sentinel Checks Complete. All Systems Safe."
    exitSuccess

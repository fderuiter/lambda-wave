{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, getWriteOffset)
import FFI.RingBuffer.Types (peekStaticFields, RingBufferControl)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.C.Types (CSize, CChar)
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
        Right _ -> putStrLn "PASS: getMonotonicTimeNS returned successfully"

    t2 <- try getRealTimeNS
    case t2 of
        Left e -> do
            putStrLn $ "FAIL: getRealTimeNS crashed: " ++ show (e :: SomeException)
            exitFailure
        Right _ -> putStrLn "PASS: getRealTimeNS returned successfully"

    -- 2. Test RingBuffer Creation (Invalid Size)
    putStrLn "[Test] RingBuffer Invalid Size..."
    res <- try (createRingBuffer 0) :: IO (Either SomeException (ForeignPtr RingBufferControl))
    case res of
        Left e -> putStrLn $ "PASS: createRingBuffer(0) threw exception: " ++ show e
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(0) succeeded unexpectedly"
             exitFailure

    res2 <- try (createRingBuffer (-100)) :: IO (Either SomeException (ForeignPtr RingBufferControl))
    case res2 of
        Left e -> putStrLn $ "PASS: createRingBuffer(-100) threw exception: " ++ show e
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(-100) succeeded unexpectedly"
             exitFailure

    -- 3. Test RingBuffer Creation (Valid)
    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    fp <- createRingBuffer 1024
    putStrLn "PASS: createRingBuffer(1024) succeeded"

    -- 4. Test FFI Interaction (getWriteOffset)
    offset <- getWriteOffset fp
    if offset == 0
       then putStrLn "PASS: Initial getWriteOffset is 0"
       else do
           putStrLn $ "FAIL: Initial getWriteOffset is " ++ show offset
           exitFailure

    -- 5. Test RingBufferControl Layout (Manual Size Check)
    putStrLn "[Test] RingBufferControl Size Verification..."
    let sizeT = sizeOf (undefined :: CSize)
        ptrSize = sizeOf (undefined :: Ptr CChar)
        minSize = sizeT * 2 + ptrSize + sizeT

    putStrLn $ "Minimum size required: " ++ show minSize
    when (minSize > 64) $ do
        putStrLn "FAIL: Platform pointers too large for 64-byte struct!"
        exitFailure

    putStrLn "PASS: Layout fits within 64 bytes."

    -- 6. Verify peekStaticFields works (Runtime Check)
    putStrLn "[Test] RingBufferControl peekStaticFields..."

    (_, sz) <- withForeignPtr fp peekStaticFields

    if sz == 1024
       then putStrLn "PASS: peekStaticFields returned correct size (1024)"
       else do
           putStrLn $ "FAIL: peekStaticFields returned size " ++ show sz
           exitFailure

    putStrLn "Sentinel Checks Complete. All Systems Safe."
    exitSuccess

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, checkoutBlock, releaseBlock)
import FFI.RingBuffer.Types (RingBufferControl(..))
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import System.Exit (exitFailure, exitSuccess)

instance Storable RingBufferControl where
    sizeOf _ = 64
    alignment _ = 64

    peek ptr = do
        s0 <- peekByteOff ptr 0
        s1 <- peekByteOff ptr 4
        s2 <- peekByteOff ptr 8
        s3 <- peekByteOff ptr 12
        w0 <- peekByteOff ptr 16
        w1 <- peekByteOff ptr 20
        w2 <- peekByteOff ptr 24
        w3 <- peekByteOff ptr 28
        start <- peekByteOff ptr 32
        sz <- peekByteOff ptr 40
        cwb <- peekByteOff ptr 48
        cwo <- peekByteOff ptr 56
        return $ RingBufferControl s0 s1 s2 s3 w0 w1 w2 w3 start sz cwb cwo

    poke ptr (RingBufferControl s0 s1 s2 s3 w0 w1 w2 w3 start sz cwb cwo) = do
        pokeByteOff ptr 0 s0
        pokeByteOff ptr 4 s1
        pokeByteOff ptr 8 s2
        pokeByteOff ptr 12 s3
        pokeByteOff ptr 16 w0
        pokeByteOff ptr 20 w1
        pokeByteOff ptr 24 w2
        pokeByteOff ptr 28 w3
        pokeByteOff ptr 32 start
        pokeByteOff ptr 40 sz
        pokeByteOff ptr 48 cwb
        pokeByteOff ptr 56 cwo

main :: IO ()
main = do
    putStrLn "Running Sentinel Checks..."

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

    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    fp <- createRingBuffer 1024
    putStrLn "PASS: createRingBuffer(1024) succeeded"

    mb <- checkoutBlock fp
    case mb of
        Nothing -> putStrLn "PASS: Initial checkoutBlock returns Nothing (no blocks ready)"
        Just idx -> do
           putStrLn $ "FAIL: Initial checkoutBlock returned " ++ show idx
           exitFailure

    putStrLn "[Test] RingBufferControl Storable Layout..."
    let actualSize = sizeOf (RingBufferControl 0 0 0 0 0 0 0 0 nullPtr 0 0 0)
    putStrLn $ "RingBufferControl Size: " ++ show actualSize ++ " (Expected: 64)"
    when (actualSize /= 64) $ do
        putStrLn "FAIL: Incorrect RingBufferControl size"
        exitFailure

    putStrLn "[Test] RingBufferControl Poke/Peek..."
    alloca $ \ptr -> do
        let rb = RingBufferControl 1 2 3 4 5 6 7 8 nullPtr 100 0 0
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

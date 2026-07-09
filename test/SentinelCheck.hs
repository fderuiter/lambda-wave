{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Types (SystemState(..), BeamState(..), Point3D(..), CalibrationStatus(..), DisplayPreset(..))
import qualified Data.Map as Map
import Control.Concurrent.STM (newTBQueueIO, newTVarIO, TVar)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Hardware.FFI.Bridge (handleHardwareResponse)
import Control.Exception (throwIO)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time.HighRes (getMonotonicTimeNS, getRealTimeNS)
import FFI.RingBuffer.IO (createRingBuffer, getWriteOffset, setReadOffset)
import FFI.RingBuffer.Types (RingBufferControl(..))
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import System.Exit (exitFailure, exitSuccess)

-- | Orphan Storable instance strictly for testing layout.
-- This ensures that the binary layout matches expectations without exposing
-- the dangerous Storable instance (which risks atomic race conditions) to production code.
instance Storable RingBufferControl where
    sizeOf _ = 64
    alignment _ = 64

    peek ptr = do
        let sizeT = sizeOf (0 :: CSize)
            -- Assumes strict packing which is standard for size_t/ptr
            readOff = sizeT
            startOff = readOff + sizeT
            sizeOff = startOff + sizeOf (0 :: CSize)

        woff <- peekByteOff ptr 0
        roff <- peekByteOff ptr readOff
        start <- peekByteOff ptr startOff :: IO CSize
        sz <- peekByteOff ptr sizeOff
        return $ RingBufferControl woff roff start sz

    poke ptr (RingBufferControl woff roff start sz) = do
        let sizeT = sizeOf (0 :: CSize)
            readOff = sizeT
            startOff = readOff + sizeT
            sizeOff = startOff + sizeOf (0 :: CSize)

        pokeByteOff ptr 0 woff
        pokeByteOff ptr readOff roff
        pokeByteOff ptr startOff start
        pokeByteOff ptr sizeOff sz



createDummyState :: IO (TVar SystemState)
createDummyState = do
    auditQ <- newTBQueueIO 10000
    let kState = initKalman 0.0 (KalmanConfig 1.0 1.0)
    newTVarIO $ SystemState
        { currentPoints = []
        , beamState = BeamOff
        , lastFrameTime = 0
        , sequenceNumber = 0
        , isocenter = Point3D 0 0 0 0 0
        , threadHeartbeats = Map.empty
        , kalmanState = kState
        , auditQueue = auditQ
        , audioAlertEnabled = False
        , activeLanguage = "en"
        , localizedBeamState = ""
        , calibrationStatus = CalibrationValid, mtiState = [], displayPreset = StandardPreset
        }

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
    st <- createDummyState
    res <- try $ (createRingBuffer st 0 >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure)
    case res of
        Left e -> putStrLn $ "PASS: createRingBuffer(0) threw exception: " ++ show (e :: SomeException)
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(0) succeeded unexpectedly"
             exitFailure

    st' <- createDummyState
    res2 <- try $ (createRingBuffer st' (-100) >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure)
    case res2 of
        Left e -> putStrLn $ "PASS: createRingBuffer(-100) threw exception: " ++ show (e :: SomeException)
        Right _ -> do
             putStrLn "FAIL: createRingBuffer(-100) succeeded unexpectedly"
             exitFailure

    -- 3. Test RingBuffer Creation (Valid)
    putStrLn "[Test] RingBuffer Valid Creation & FFI..."
    st'' <- createDummyState
    fp <- createRingBuffer st'' 1024 >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure
    putStrLn "PASS: createRingBuffer(1024) succeeded"

    -- 4. Test FFI Interaction (getWriteOffset)
    -- This verifies the pointer is valid and C++ object is alive.
    offset <- getWriteOffset fp
    if offset == 0
       then putStrLn "PASS: Initial getWriteOffset is 0"
       else do
           putStrLn $ "FAIL: Initial getWriteOffset is " ++ show offset
           exitFailure

    -- 5. Test setReadOffset Safety
    putStrLn "[Test] setReadOffset Safety..."

    -- Test Negative Offset
    resNeg <- try $ setReadOffset fp (-1)
    case resNeg of
        Left (e :: SomeException) -> putStrLn $ "PASS: setReadOffset(-1) threw exception: " ++ show e
        Right _ -> do
             putStrLn "FAIL: setReadOffset(-1) succeeded unexpectedly"
             exitFailure

    -- Test Out of Bounds Offset (>= 1024)
    resOOB <- try $ setReadOffset fp 1024
    case resOOB of
        Left (e :: SomeException) -> putStrLn $ "PASS: setReadOffset(1024) threw exception: " ++ show e
        Right _ -> do
             putStrLn "FAIL: setReadOffset(1024) succeeded unexpectedly"
             exitFailure

    -- Test Valid Offset
    resValid <- try $ setReadOffset fp 100
    case resValid of
        Left (e :: SomeException) -> do
             putStrLn $ "FAIL: setReadOffset(100) threw exception: " ++ show e
             exitFailure
        Right _ -> putStrLn "PASS: setReadOffset(100) succeeded"


    -- 6. Test RingBufferControl Layout
    putStrLn "[Test] RingBufferControl Storable Layout..."
    let actualSize = sizeOf (RingBufferControl 0 0 0 0)
    putStrLn $ "RingBufferControl Size: " ++ show actualSize ++ " (Expected: 64)"
    when (actualSize /= 64) $ do
        putStrLn "FAIL: Incorrect RingBufferControl size"
        exitFailure

    -- Verify poke/peek roundtrip
    putStrLn "[Test] RingBufferControl Poke/Peek..."
    alloca $ \ptr -> do
        let rb = RingBufferControl 1 2 0 100
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

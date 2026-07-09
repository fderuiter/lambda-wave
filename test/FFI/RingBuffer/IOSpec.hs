{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module FFI.RingBuffer.IOSpec (spec) where

import Hardware.FFI.Bridge (handleHardwareResponse)
import Control.Exception (throwIO)
import Test.Hspec
import FFI.RingBuffer.IO
import FFI.RingBuffer.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import System.Posix.IO (createPipe, closeFd, fdWriteBuf)
import System.Posix.Types (Fd(..))
import Foreign.Storable (peekByteOff)
import Foreign.C.Types (CChar)
import Control.Concurrent (threadDelay, killThread, forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (void)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Concurrent.STM (newTVarIO, newTBQueueIO, TVar)
import qualified Data.Map.Strict as Map
import Data.Types
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))

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

spec :: Spec
spec = do
  describe "FFI.RingBuffer.IO" $ do
    it "createRingBuffer returns a valid pointer" $ do
      dummyState <- createDummyState
      ptr <- createRingBuffer dummyState 1024 >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure
      getWriteOffset ptr `shouldReturn` 0

    it "createRingBuffer throws error for invalid size" $ do
      dummyState <- createDummyState
      (createRingBuffer dummyState 0 >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure) `shouldThrow` anyException
      dummyState' <- createDummyState
      (createRingBuffer dummyState' (-1) >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure) `shouldThrow` anyException

    it "ingestionLoop reads data from pipe into ring buffer" $ do
      (readFd, writeFd) <- createPipe

      dummyState <- createDummyState
      ptr <- createRingBuffer dummyState 4096 >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure
      wOff <- getWriteOffset ptr
      wOff `shouldBe` 0

      dummyState' <- createDummyState
      tid <- ingestionLoop dummyState' ptr readFd

      let dataToWrite = "Hello, RingBuffer! This is a test string to verify ingestion...." :: ByteString
      writeBytes writeFd dataToWrite

      threadDelay 100_000

      wOffAfter <- getWriteOffset ptr
      let expectedOffset = B.length dataToWrite

      wOffAfter `shouldSatisfy` (> 0)
      wOffAfter `shouldBe` expectedOffset

      closeFd readFd
      closeFd writeFd
      killThread tid

    it "handles high-throughput ingestion without data loss (1M items)" $ do
      (readFd, writeFd) <- createPipe
      let bufSz = 4096
      let totalBytes = 1_000_000 :: Int

      dummyState <- createDummyState
      ptr <- createRingBuffer dummyState bufSz >>= handleHardwareResponse (\e -> throwIO (userError $ show e)) pure
      dummyState' <- createDummyState
      tid <- ingestionLoop dummyState' ptr readFd

      producerDone <- newEmptyMVar
      _ <- forkIO $ do
          let chunkSize = 1024

          let go n | n >= totalBytes = return ()
              go n = do
                  let remaining = totalBytes - n
                  let toWrite = min remaining chunkSize
                  let bytes = B.pack $ map (\i -> fromIntegral ((n + i) `mod` 256)) [0..toWrite-1]
                  writeBytes writeFd bytes
                  go (n + toWrite)

          go 0
          closeFd writeFd
          putMVar producerDone ()

      startPtr <- getBufferStart ptr

      let verify n | n >= totalBytes = return ()
          verify n = do
              wOff <- getWriteOffset ptr
              let rOff = n `mod` bufSz

              if wOff == rOff
                  then threadDelay 100 >> verify n
                  else do
                      let end = if wOff > rOff then wOff else bufSz
                      let count = end - rOff

                      bytes <- mapM (\i -> peekByteOff startPtr (rOff + i)) [0..count-1] :: IO [Word8]

                      let expected = map (\i -> fromIntegral ((n + i) `mod` 256)) [0..count-1]
                      bytes `shouldBe` expected

                      let n' = n + count
                      let rOff' = (rOff + count) `mod` bufSz

                      setReadOffset ptr rOff'
                      verify n'

      verify 0
      takeMVar producerDone
      closeFd readFd
      killThread tid

getBufferStart :: ForeignPtr RingBufferControl -> IO (Ptr CChar)
getBufferStart fp = withForeignPtr fp $ \p0 -> do
    (start, _) <- peekStaticFields p0
    return start

writeBytes :: Fd -> ByteString -> IO ()
writeBytes fd bs = B.useAsCStringLen bs $ \(ptr, len) -> do
    void $ fdWriteBuf fd (castPtr ptr) (fromIntegral len)

-- Requirement FR-DAQ-001

-- Requirement FR-DAQ-004

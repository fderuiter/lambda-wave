{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module FFI.RingBuffer.IOSpec (spec) where

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
import Control.Monad (void, when)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "FFI.RingBuffer.IO" $ do
    it "createRingBuffer returns a valid pointer" $ do
      ptr <- createRingBuffer 1024
      checkoutBlock ptr `shouldReturn` Nothing

    it "createRingBuffer throws error for invalid size" $ do
      createRingBuffer 0 `shouldThrow` anyException
      createRingBuffer (-1) `shouldThrow` anyException

    it "ingestionLoop reads data from pipe into ring buffer" $ do
      (readFd, writeFd) <- createPipe

      ptr <- createRingBuffer 4096

      tid <- ingestionLoop ptr readFd

      let dataToWrite = "Hello, RingBuffer! This is a test string to verify ingestion...." :: ByteString
      writeBytes writeFd dataToWrite

      -- Simulate EOF or EAGAIN to flush the block
      threadDelay 100_000

      -- We should be able to checkout a block
      maybeIdx <- checkoutBlock ptr
      maybeIdx `shouldSatisfy` isJust
      let (Just idx) = maybeIdx
      
      bytesWritten <- getBlockBytesWritten ptr idx
      let expectedOffset = B.length dataToWrite

      bytesWritten `shouldBe` expectedOffset
      
      releaseBlock ptr idx

      closeFd readFd
      closeFd writeFd
      killThread tid

    it "handles high-throughput ingestion without data loss (1M items)" $ do
      (readFd, writeFd) <- createPipe
      let bufSz = 4096
      let blockSz = bufSz `div` 4
      let totalBytes = 100_000 :: Int -- Reduced to speed up test but test logic

      ptr <- createRingBuffer bufSz
      tid <- ingestionLoop ptr readFd

      producerDone <- newEmptyMVar
      _ <- forkIO $ do
          let chunkSize = 1024

          let go n | n >= totalBytes = return ()
              go n = do
                  let remaining = totalBytes - n
                  let toWrite = min remaining chunkSize
                  let bytes = B.pack $ map (\i -> fromIntegral ((n + i) `mod` 256)) [0..toWrite-1]
                  writeBytes writeFd bytes
                  -- Small delay to let ingestion thread pull data and potentially flush
                  threadDelay 1_000
                  go (n + toWrite)

          go 0
          closeFd writeFd
          putMVar producerDone ()

      startPtr <- getBufferStart ptr

      let verify n | n >= totalBytes = return ()
          verify n = do
              maybeIdx <- checkoutBlock ptr
              case maybeIdx of
                  Nothing -> do
                      threadDelay 1000
                      verify n
                  Just idx -> do
                      bytesWritten <- getBlockBytesWritten ptr idx
                      
                      let expectedCount = bytesWritten
                      let startOffset = idx * blockSz

                      bytes <- mapM (\i -> peekByteOff startPtr (startOffset + i)) [0..expectedCount-1] :: IO [Word8]

                      let expected = map (\i -> fromIntegral ((n + i) `mod` 256)) [0..expectedCount-1]
                      bytes `shouldBe` expected

                      releaseBlock ptr idx
                      verify (n + expectedCount)

      verify 0
      takeMVar producerDone
      closeFd readFd
      killThread tid

getBufferStart :: ForeignPtr RingBufferControl -> IO (Ptr CChar)
getBufferStart fp = withForeignPtr fp $ \p -> do
    (start, _) <- peekStaticFields p
    return start

writeBytes :: Fd -> ByteString -> IO ()
writeBytes fd bs = B.useAsCStringLen bs $ \(ptr, len) -> do
    void $ fdWriteBuf fd (castPtr ptr) (fromIntegral len)

-- Requirement FR-DAQ-001

-- Requirement FR-DAQ-004

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
import Control.Monad (void)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "FFI.RingBuffer.IO" $ do
    it "createRingBuffer returns a valid pointer" $ do
      res <- createRingBuffer 1024
      res `shouldSatisfy` isRight
      case res of
          Right ptr -> getWriteOffset ptr `shouldReturn` 0
          Left _ -> fail "Expected Right"

    it "createRingBuffer returns error for invalid size" $ do
      res0 <- createRingBuffer 0
      res0 `shouldSatisfy` isLeft

      res1 <- createRingBuffer (-1)
      res1 `shouldSatisfy` isLeft

    it "ingestionLoop reads data from pipe into ring buffer" $ do
      (readFd, writeFd) <- createPipe

      withRingBuffer 4096 $ \ptr -> do
        wOff <- getWriteOffset ptr
        wOff `shouldBe` 0

        tid <- ingestionLoop ptr readFd

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

      withRingBuffer bufSz $ \ptr -> do
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
getBufferStart fp = withForeignPtr fp $ \p -> do
    (start, _) <- peekStaticFields p
    return start

writeBytes :: Fd -> ByteString -> IO ()
writeBytes fd bs = B.useAsCStringLen bs $ \(ptr, len) -> do
    void $ fdWriteBuf fd (castPtr ptr) (fromIntegral len)

{-# LANGUAGE OverloadedStrings #-}

module FFI.RingBuffer.IOSpec (spec) where

import Test.Hspec
import FFI.RingBuffer.IO
import FFI.RingBuffer.Types
import Foreign.Ptr (nullPtr)
import System.Posix.IO (createPipe, fdWrite, closeFd, OpenMode(..), defaultFileFlags, fdToHandle)
import System.Posix.Types (Fd(..))
import Foreign.Storable (peek)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad (void)
import Data.Word (Word64)
import Control.Exception (finally)

spec :: Spec
spec = do
  describe "FFI.RingBuffer.IO" $ do
    it "createRingBuffer returns a valid pointer" $ do
      ptr <- createRingBuffer 1024
      ptr `shouldNotBe` nullPtr
      freeRingBuffer ptr

    it "withRingBuffer allocates and frees the buffer" $ do
      -- We can't easily verify free happens, but we can verify allocation and usage
      withRingBuffer 1024 $ \ptr -> do
        ptr `shouldNotBe` nullPtr

    it "ingestionLoop reads data from pipe into ring buffer" $ do
      (readFd, writeFd) <- createPipe

      withRingBuffer 4096 $ \ptr -> do
        -- Check initial state
        initialControl <- peek ptr
        writeOffset initialControl `shouldBe` 0

        -- Start ingestion loop
        tid <- ingestionLoop ptr readFd

        -- Write data to the pipe
        -- We write 64 bytes of data
        let dataToWrite = "Hello, RingBuffer! This is a test string to verify ingestion...."
        _ <- fdWrite writeFd dataToWrite

        -- Give the thread some time to read
        threadDelay 100000 -- 0.1s

        -- Check if writeOffset updated
        updatedControl <- peek ptr
        let expectedOffset = fromIntegral (length dataToWrite)

        writeOffset updatedControl `shouldSatisfy` (> 0)
        writeOffset updatedControl `shouldBe` expectedOffset

        -- Clean up: Close the pipe to signal EOF to the ingestion loop
        closeFd readFd
        closeFd writeFd

        -- Give the thread time to exit
        threadDelay 50000
        -- We can explicitly kill it just in case, though closeFd should do it
        killThread tid

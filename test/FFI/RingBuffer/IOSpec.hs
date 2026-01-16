{-# LANGUAGE OverloadedStrings #-}

module FFI.RingBuffer.IOSpec (spec) where

import Test.Hspec
import FFI.RingBuffer.IO
import FFI.RingBuffer.Types
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (withForeignPtr)
import System.Posix.IO (createPipe, fdWrite, closeFd)
import Foreign.Storable (peek)
import Control.Concurrent (threadDelay, killThread)

spec :: Spec
spec = do
  describe "FFI.RingBuffer.IO" $ do
    it "createRingBuffer returns a valid pointer" $ do
      fp <- createRingBuffer 1024
      withForeignPtr fp $ \ptr ->
          ptr `shouldNotBe` nullPtr
      -- freeRingBuffer is handled by ForeignPtr finalizer, manual call is okay but not strictly needed if we let it drop
      freeRingBuffer fp

    it "withRingBuffer allocates and frees the buffer" $ do
      withRingBuffer 1024 $ \fp -> do
         withForeignPtr fp $ \ptr ->
            ptr `shouldNotBe` nullPtr

    it "ingestionLoop reads data from pipe into ring buffer" $ do
      (readFd, writeFd) <- createPipe

      withRingBuffer 4096 $ \fp -> do
        withForeignPtr fp $ \ptr -> do
            -- Check initial state
            initialControl <- peek ptr
            writeOffset initialControl `shouldBe` 0

        -- Start ingestion loop
        tid <- ingestionLoop fp readFd

        -- Write data to the pipe
        let dataToWrite = "Hello, RingBuffer! This is a test string to verify ingestion...."
        _ <- fdWrite writeFd dataToWrite

        -- Give the thread some time to read
        threadDelay 100000 -- 0.1s

        -- Check if writeOffset updated
        withForeignPtr fp $ \ptr -> do
            updatedControl <- peek ptr
            let expectedOffset = fromIntegral (length dataToWrite)

            writeOffset updatedControl `shouldSatisfy` (> 0)
            writeOffset updatedControl `shouldBe` expectedOffset

        -- Clean up: Close the pipe to signal EOF to the ingestion loop
        closeFd readFd
        closeFd writeFd

        -- Give the thread time to exit
        threadDelay 50000
        -- We can explicitly kill it just in case
        killThread tid

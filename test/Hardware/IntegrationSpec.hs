{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hardware.IntegrationSpec (spec) where

import Test.Hspec
import Control.Concurrent (forkOS, killThread, threadDelay)
import Control.Concurrent.STM
import System.Posix.IO (openFd, closeFd, OpenMode(..), defaultFileFlags)
import System.Posix.Files (removeLink)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P
import System.IO (openBinaryFile, hClose, IOMode(WriteMode))
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)
import Control.Exception (bracket)

import Data.Word (Word32)
import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import qualified FFI.RingBuffer.IO as RingBuffer
import Hardware.Consumer (consumerLoop)
import Data.Time.HighRes (getMonotonicTimeNS)

-- | Construct a valid TLV Packet
-- Same logic as ConsumerSpec, but we generate multiple frames
generatePacket :: Word32 -> Word32 -> BL.ByteString
generatePacket frameNum numPoints = P.runPut $ do
    -- Magic Word
    mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]

    -- Header
    -- Total Len = 36 (Header) + TLV Size
    -- TLV Size = 8 (Header) + numPoints * 16
    let tlvPayloadLen = numPoints * 16
    let tlvTotalLen = 8 + tlvPayloadLen
    let packetLen = 36 + tlvTotalLen

    P.putWord32le 0 -- Version
    P.putWord32le packetLen -- Total Len
    P.putWord32le 0 -- Platform
    P.putWord32le frameNum -- Frame Num
    P.putWord32le 0 -- CPU
    P.putWord32le 1 -- Num TLVs
    P.putWord32le 0 -- SubFrame

    -- TLV (Type 1)
    P.putWord32le 1 -- Type
    P.putWord32le tlvTotalLen -- Length

    -- Points
    forM_ [1..numPoints] $ \i -> do
        P.putFloatle (fromIntegral i) -- x
        P.putFloatle 0.0 -- y
        P.putFloatle 10.0 -- z
        P.putFloatle 0.0 -- v

spec :: Spec
spec = do
  describe "Integration Pipeline (P1-005)" $ do
    it "Ingests and parses frames from a file via Ring Buffer" $ do
        let testFile = "test_capture.bin"
        let numFrames = 10
        let pointsPerFrame = 2

        -- 1. Create Test File
        bracket (openBinaryFile testFile WriteMode) hClose $ \h -> do
            let frames = [generatePacket f pointsPerFrame | f <- [1..numFrames]]
            BL.hPut h (BL.concat frames)

        -- Ensure cleanup of testFile
        bracket (return ()) (\_ -> removeLink testFile) $ \_ -> do

            -- 2. Setup System
            t <- getMonotonicTimeNS
            let kConfig = KalmanConfig 10.0 2.0
            let kState = initKalman targetHeight kConfig
            q <- newTBQueueIO 100
            -- Initialize with frameCount = 0
            let initialState = SystemState [] BeamOff t (Point3D 0 0 0 0 0) Map.empty kState q 0
            stateVar <- newTVarIO initialState

            -- 3. Run Pipeline
            -- Create Ring Buffer (1MB)
            rb <- RingBuffer.createRingBuffer (1024 * 1024)

            -- Open File for Ingestion (ensuring close)
            bracket (openFd testFile ReadOnly Nothing defaultFileFlags) closeFd $ \fd -> do

                -- Fork Threads
                ingestTid <- RingBuffer.ingestionLoop rb fd
                consumerTid <- forkOS $ consumerLoop rb stateVar

                -- 4. Poll State
                -- We expect 10 frames to be processed.
                -- ingestionLoop reads file. It might finish quickly and loop (reading 0 bytes).
                -- consumerLoop processes RB.

                -- Wait up to 2 seconds for processing
                let timeout = 2000 :: Int -- 2s (in ms)

                let poll loopCount = do
                        if loopCount <= 0
                            then fail "Timeout waiting for frames"
                            else do
                                s <- readTVarIO stateVar
                                if frameCount s >= fromIntegral numFrames
                                    then return () -- Success!
                                    else do
                                        threadDelay 10000 -- 10ms
                                        poll (loopCount - 10)

                poll (timeout `div` 10)

                -- Cleanup Threads
                killThread ingestTid
                killThread consumerTid

                -- Final Verification
                finalState <- readTVarIO stateVar
                frameCount finalState `shouldSatisfy` (>= fromIntegral numFrames)
                length (currentPoints finalState) `shouldBe` fromIntegral pointsPerFrame

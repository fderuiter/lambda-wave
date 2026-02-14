{-# LANGUAGE OverloadedStrings #-}

module Simulation (simulationLoop) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word32, Word8)
import System.Posix.IO (fdWriteBuf)
import System.Posix.Types (Fd)
import Foreign.Ptr (castPtr)

-- | Magic Word for TI mmWave
magicPattern :: [Word8]
magicPattern = [1, 2, 3, 4, 5, 6, 7, 8]

-- | Simulates the Radar Sensor by generating valid TLV packets
-- and writing them to the provided file descriptor (pipe).
simulationLoop :: Fd -> IO ()
simulationLoop fd = loop 0
  where
    loop :: Double -> IO ()
    loop t = do
        let packet = generatePacket t
            strictPacket = BL.toStrict packet

        -- Write to Pipe
        void $ BU.unsafeUseAsCStringLen strictPacket $ \(ptr, len) ->
            fdWriteBuf fd (castPtr ptr) (fromIntegral len)

        threadDelay 33000 -- ~30Hz
        loop (t + 0.1)

generatePacket :: Double -> BL.ByteString
generatePacket t = runPut $ do
    -- 1. Magic Word (8 bytes)
    mapM_ putWord8 magicPattern

    -- 2. Header (7 fields * 4 = 28 bytes)
    let numPoints = 20 :: Int
        pointSize = 16 :: Int -- 4 floats * 4 bytes
        tlvHeaderSize = 8 :: Int
        tlvPayloadSize = numPoints * pointSize
        tlvTotalSize = tlvHeaderSize + tlvPayloadSize
        headerSize = 8 + 28 :: Int -- Magic + Fields
        totalPacketLen = fromIntegral (headerSize + tlvTotalSize) :: Word32

    putWord32le 0x02000000 -- Version
    putWord32le totalPacketLen
    putWord32le 0xA1642    -- Platform
    putWord32le (floor (t * 10)) -- Frame Number
    putWord32le (floor (t * 1000)) -- Time (CPU Cycles)
    putWord32le 1          -- Num TLVs
    putWord32le 0          -- SubFrame

    -- 3. TLV (Type 1 - Detected Points)
    putWord32le 1 -- Type
    putWord32le (fromIntegral tlvTotalSize) -- Length (Header + Payload)

    -- 4. Payload (Points)
    -- Target Motion: Circular in X-Z plane, constant Y (depth)
    -- X = sin(t), Z = cos(t), Y = 2.0
    let centerX = 0.5 * sin t
        centerY = 2.0
        centerZ = 0.5 * cos t

    forM_ [1..numPoints] $ \i -> do
        let noise = fromIntegral i * 0.01 :: Double
            px = centerX + noise
            py = centerY + noise
            pz = centerZ + noise
            v  = 0.0 :: Double

        putFloatle (realToFrac px)
        putFloatle (realToFrac py)
        putFloatle (realToFrac pz)
        putFloatle (realToFrac v)

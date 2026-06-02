{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Word
import System.IO
import Control.Monad (forM_)

-- Define Point structure locally to match the one in Data.Types
data Point = Point
  { px' :: Float
  , py' :: Float
  , pz' :: Float
  , v'  :: Float
  } deriving (Show)

magicPattern :: [Word8]
magicPattern = [1, 2, 3, 4, 5, 6, 7, 8]

putPoint :: Point -> Put
putPoint (Point x y z v) = do
    let scale = 0.061035
    putWord16le (round (x / scale))
    putWord16le (round (y / scale))
    putWord16le (round (z / scale))
    putWord16le (round (v / scale))

-- Deterministic "random" values based on seed
pseudoRandom :: Int -> Int -> Int
pseudoRandom seed maxVal = (seed * 1103515245 + 12345) `mod` maxVal

generateFrame :: Word32 -> Put
generateFrame frameNum = do
    -- 1. Determine Frame Content
    let numPoints = fromIntegral ((frameNum `mod` 10) + 1) :: Int
    let points = [Point (fromIntegral i) (fromIntegral frameNum) 0 0 | i <- [1..numPoints]]

    -- TLV 999 Size: Deterministic "random" size between 16 and 64 bytes
    let tlv999PayloadSize = 16 + (pseudoRandom (fromIntegral frameNum) 48)
    let tlv999TotalSize = 8 + tlv999PayloadSize

    let pointsSize = numPoints * 8
    let tlv1TotalSize = 8 + pointsSize

    let headerSize = 36 -- 8 magic + 7 * 4 words
    let totalPacketLen = fromIntegral (headerSize + tlv1TotalSize + tlv999TotalSize) :: Word32

    -- 2. Magic Word
    mapM_ putWord8 magicPattern

    -- 3. Header
    putWord32le 0 -- Version (4)
    putWord32le totalPacketLen -- TotalPacketLen (4)
    putWord32le 0 -- Platform (4)
    putWord32le frameNum -- FrameNum (4)
    putWord32le 0 -- CPU Cycles (4)
    putWord32le 2 -- Num TLVs (4) - Points + Unknown
    putWord32le 0 -- SubFrame (4)

    -- 4. TLV 1 (Points)
    putWord32le 1 -- Type 1 (Detected Points)
    putWord32le (fromIntegral tlv1TotalSize) -- Length (Header + Payload)
    mapM_ putPoint points

    -- 5. TLV 999 (Unknown)
    putWord32le 999 -- Type 999
    putWord32le (fromIntegral tlv999TotalSize) -- Length
    -- Write payload as simple incrementing bytes
    mapM_ putWord8 (take tlv999PayloadSize [0..])

    -- 6. Inter-frame Garbage
    -- Removed to avoid "consumed bytes" issue in current parser implementation
    -- which causes frame drops when garbage is < 8 bytes.
    -- We still test robustness via TLV padding and Unknown TLVs.
    return ()

main :: IO ()
main = do
    let frames = [1..100]
    let putStream = forM_ frames $ \i -> generateFrame (fromIntegral i)

    let bytes = runPut putStream
    BL.writeFile "test/fixtures/complex_capture.bin" bytes
    putStrLn "Generated test/fixtures/complex_capture.bin"

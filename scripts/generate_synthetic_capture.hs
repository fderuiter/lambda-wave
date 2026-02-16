{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Word
import System.IO
import Control.Monad (forM_)

-- Define Point structure locally to match the one in Data.Types
-- This ensures the script is standalone and doesn't depend on project structure during build
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
    putFloatle x
    putFloatle y
    putFloatle z
    putFloatle v

generateFrame :: Word32 -> [Point] -> Put
generateFrame frameNum points = do
    -- 1. Magic Word
    mapM_ putWord8 magicPattern

    -- 2. Header (Total 36 bytes including Magic Word)
    -- Magic (8) - done
    putWord32le 0 -- Version (4)

    let numPoints = length points
    let pointsSize = numPoints * 16
    let tlvHeaderSize = 8
    let tlvTotalSize = tlvHeaderSize + pointsSize
    let headerSize = 36 -- 8 magic + 7 * 4 words
    let totalPacketLen = fromIntegral (headerSize + tlvTotalSize) :: Word32

    putWord32le totalPacketLen -- TotalPacketLen (4)
    putWord32le 0 -- Platform (4)
    putWord32le frameNum -- FrameNum (4)
    putWord32le 0 -- CPU Cycles (4)
    putWord32le 1 -- Num TLVs (4) - Always 1 for this test
    putWord32le 0 -- SubFrame (4)

    -- 3. TLV
    putWord32le 1 -- Type 1 (Detected Points)
    putWord32le (fromIntegral tlvTotalSize) -- Length (Header + Payload)

    mapM_ putPoint points

main :: IO ()
main = do
    let frames = [1..100]
    let putStream = forM_ frames $ \i -> do
            -- Generate 1 point per frame: Point(i, 0, 0, 0)
            let pts = [Point (fromIntegral i) 0 0 0]
            generateFrame (fromIntegral i) pts

    let bytes = runPut putStream
    BL.writeFile "test/fixtures/synthetic_capture.bin" bytes
    putStrLn "Generated test/fixtures/synthetic_capture.bin"

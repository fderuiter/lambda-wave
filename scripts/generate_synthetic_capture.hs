{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P
import Control.Monad (forM_)
import Data.Word (Word32)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Represents a raw point for serialization (Local definition)
data Point = Point
  { px' :: Float
  , py' :: Float
  , pz' :: Float
  , v'  :: Float
  }

-- | Generate a synthetic point
putPoint :: Point -> P.Put
putPoint Point{..} = do
    P.putFloatle px'
    P.putFloatle py'
    P.putFloatle pz'
    P.putFloatle v'

-- | Generate a synthetic frame
putFrame :: Word32 -> [Point] -> P.Put
putFrame frameNum points = do
    -- Magic Word: 0x0102030405060708
    mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]

    -- Calculate lengths
    let numPoints = length points
        payloadLen = numPoints * 16
        tlvLen = 8 + payloadLen -- TLV Length includes header
        totalPacketLen = 36 + tlvLen

    -- Header
    P.putWord32le 0 -- Version
    P.putWord32le (fromIntegral totalPacketLen) -- Total Len
    P.putWord32le 0 -- Platform
    P.putWord32le frameNum -- Frame Num
    P.putWord32le 0 -- CPU
    P.putWord32le 1 -- Num TLVs
    P.putWord32le 0 -- SubFrame

    -- TLV
    P.putWord32le 1 -- Type (Detected Points)
    P.putWord32le (fromIntegral tlvLen) -- Length
    mapM_ putPoint points

generateSyntheticFile :: FilePath -> Int -> IO ()
generateSyntheticFile path numFrames = do
    -- Create points for each frame
    -- Frame i has 1 point with velocity = i
    let frames = [0 .. (numFrames - 1)]
    let putAll = forM_ frames $ \i -> do
            let points = [Point 1.0 2.0 3.0 (fromIntegral i)]
            putFrame (fromIntegral i) points

    let content = P.runPut putAll
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile path content
    putStrLn $ "Generated synthetic capture: " ++ path ++ " (" ++ show (BL.length content) ++ " bytes)"

main :: IO ()
main = do
    let fixturePath = "test/fixtures/synthetic_capture.bin"
    let expectedFrames = 100
    generateSyntheticFile fixturePath expectedFrames

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Word

magicPattern :: [Word8]
magicPattern = [1, 2, 3, 4, 5, 6, 7, 8]

putCoeffs :: [Float] -> Put
putCoeffs = mapM_ putFloatle

generateFrame :: Word32 -> [Float] -> Put
generateFrame frameNum coeffs = do
  -- 1. Magic Word
  mapM_ putWord8 magicPattern

  -- 2. Header (Total 36 bytes including Magic Word)
  -- Magic (8) - done
  putWord32le 0 -- Version (4)
  let payloadSize = 24 :: Word32 -- 6 floats * 4 bytes
  let tlvHeaderSize = 8 :: Word32
  let tlvTotalSize = tlvHeaderSize + payloadSize
  let headerSize = 36 :: Word32 -- 8 magic + 7 * 4 words
  let totalPacketLen = headerSize + tlvTotalSize

  putWord32le totalPacketLen -- TotalPacketLen (4)
  putWord32le 0 -- Platform (4)
  putWord32le frameNum -- FrameNum (4)
  putWord32le 0 -- CPU Cycles (4)
  putWord32le 1 -- Num TLVs (4) - Always 1 for this test
  putWord32le 0 -- SubFrame (4)

  -- 3. TLV Type 2 (Surface Coefficients)
  putWord32le 2 -- Type 2
  putWord32le tlvTotalSize -- Length (Header + Payload)
  putCoeffs coeffs

main :: IO ()
main = do
  let frames = [1 .. 100] :: [Int]
  let putStream = forM_ frames $ \i -> do
        -- Generate a dummy flat surface at z = i
        let coeffs = [fromIntegral i, 0, 0, 0, 0, 0] :: [Float]
        generateFrame (fromIntegral i) coeffs

  let bytes = runPut putStream
  BL.writeFile "test/fixtures/synthetic_capture.bin" bytes
  putStrLn "Generated test/fixtures/synthetic_capture.bin"

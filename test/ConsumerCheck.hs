{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hardware.Consumer (parseStream)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import System.Exit (exitFailure, exitSuccess)

-- | Construct a malicious packet
-- Magic (8) + Header (28) + TLV (8) = 44 bytes
-- TLV Len is HUGE (0xFFFFFFFF)
maliciousPacket :: BL.ByteString
maliciousPacket = runPut $ do
    -- Magic Word (1..8)
    mapM_ putWord8 [1, 2, 3, 4, 5, 6, 7, 8]

    -- Header
    putWord32le 0           -- Version
    putWord32le 44          -- Total Packet Len (Claimed)
    putWord32le 0           -- Platform
    putWord32le 0           -- Frame Num
    putWord32le 0           -- CPU Cycles
    putWord32le 1           -- Num TLVs
    putWord32le 0           -- SubFrame Num

    -- TLV
    putWord32le 1           -- Type = 1 (Points)
    putWord32le 0xFFFFFFFF  -- Len = HUGE (Corrupted)

main :: IO ()
main = do
    putStrLn "Running Consumer Check..."

    let (frames, consumed, corrupted) = parseStream maliciousPacket

    putStrLn $ "Consumed: " ++ show consumed
    putStrLn $ "Corrupted: " ++ show corrupted
    putStrLn $ "Frames: " ++ show (length frames)

    if corrupted
        then do
            putStrLn "PASS: Detected corruption (Huge TLV)."
            exitSuccess
        else do
            putStrLn "FAIL: Did not detect corruption (Returned Partial/False)."
            exitFailure

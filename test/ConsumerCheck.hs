{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Word (Word32)
import System.Timeout (timeout)
import System.Exit (exitFailure, exitSuccess)
import Hardware.Consumer (parseStream)

-- | Helper to build Word32LE
word32LE :: Word32 -> BB.Builder
word32LE = BB.word32LE

main :: IO ()
main = do
    putStrLn "Running Consumer DoS Reproduction..."

    -- Construct Malicious Payload
    let magic = BB.byteString (BL.toStrict $ BL.pack [1, 2, 3, 4, 5, 6, 7, 8])
        header = mconcat
            [ word32LE 0 -- Version
            , word32LE 100 -- TotalPacketLen (Claimed small packet)
            , word32LE 0 -- Platform
            , word32LE 1 -- FrameNum
            , word32LE 0 -- Time
            , word32LE 1 -- NumTLVs (1 TLV)
            , word32LE 0 -- SubFrame
            ]
        -- Malicious TLV: Type 1 (Points), Length 0xFFFFFFFF (Huge)
        tlv = mconcat
            [ word32LE 1 -- Type: Points
            , word32LE 0xFFFFFFFF -- Length: 4GB
            ]

        -- Payload: Empty (or small)
        payload = BB.byteString "GarbageData"

        input = BB.toLazyByteString (magic <> header <> tlv <> payload)

    putStrLn $ "Input Size: " ++ show (BL.length input)

    -- Run Parser
    let (frames, consumed, corrupted) = parseStream input

    putStrLn $ "Parser finished. Consumed: " ++ show consumed ++ ", Corrupted: " ++ show corrupted

    if corrupted
        then do
            putStrLn "PASS: Parser detected corruption and exited safely."
            exitSuccess
        else if consumed > 0
            then do
                 putStrLn "PASS: Parser consumed data (even if partial)."
                 exitSuccess
            else do
                 putStrLn "FAIL: Parser returned 0 consumed and False corrupted. This indicates LIVELOCK (DoS)."
                 exitFailure

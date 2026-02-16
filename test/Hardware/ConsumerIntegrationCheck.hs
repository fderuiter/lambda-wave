{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure, exitSuccess)

import Hardware.Consumer (parseStream)
import Data.Types (RadarFrame(..), Point3D(..))

main :: IO ()
main = do
    putStrLn "Reading synthetic capture..."
    content <- BL.readFile "test/fixtures/synthetic_capture.bin"

    let (frames, bytesConsumed, corrupted) = parseStream content

    putStrLn $ "Parsed " ++ show (length frames) ++ " frames."
    putStrLn $ "Bytes consumed: " ++ show bytesConsumed
    putStrLn $ "Corrupted: " ++ show corrupted

    -- Verify frame count
    if length frames /= 100
        then do
            putStrLn $ "FAIL: Expected 100 frames, got " ++ show (length frames)
            exitFailure
        else putStrLn "PASS: Frame count matches."

    -- Verify first frame content
    -- Frame 1 should have Point(1, 0, 0, 0)
    let frame1 = head frames
    let points1 = points frame1

    if null points1
        then do
            putStrLn "FAIL: Frame 1 has no points."
            exitFailure
        else return ()

    let p1 = head points1
    -- Point3D has Double fields. The generated point has x=1.0.
    -- Verify x coordinate.
    if abs (px p1 - 1.0) < 0.001
        then putStrLn "PASS: Frame 1 point data matches."
        else do
            putStrLn $ "FAIL: Frame 1 point data mismatch. Expected x=1.0, got " ++ show (px p1)
            exitFailure

    -- Verify last frame content
    -- Frame 100 should have Point(100, 0, 0, 0)
    let frame100 = last frames
    let p100 = head (points frame100)

    if abs (px p100 - 100.0) < 0.001
        then putStrLn "PASS: Frame 100 point data matches."
        else do
            putStrLn $ "FAIL: Frame 100 point data mismatch. Expected x=100.0, got " ++ show (px p100)
            exitFailure

    putStrLn "Integration Check Passed."
    exitSuccess

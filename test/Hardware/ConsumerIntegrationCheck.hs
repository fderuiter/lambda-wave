{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)

import Hardware.Consumer (parseStream)
import Data.Types (RadarFrame(..), Point3D(..))

main :: IO ()
main = do
    envPath <- lookupEnv "CAPTURE_FILE"
    let filePath = maybe "test/fixtures/synthetic_capture.bin" id envPath

    putStrLn $ "Reading capture file: " ++ filePath
    content <- BL.readFile filePath

    -- Consume the entire file, handling interruptions (garbage/noise)
    let (frames, bytesConsumed, corrupted) = consumeAll content

    putStrLn $ "Parsed " ++ show (length frames) ++ " frames."
    putStrLn $ "Bytes consumed: " ++ show bytesConsumed
    putStrLn $ "Corrupted (at least once): " ++ show corrupted

    -- Common Check: Frames should not be empty
    if null frames
        then do
            hPutStrLn stderr "FAIL: No frames parsed."
            exitFailure
        else putStrLn "PASS: Frames parsed."

    -- Conditional Verification based on filename
    if "synthetic_capture.bin" `isSuffixOf` filePath
        then verifySyntheticSimple frames
        else if "complex_capture.bin" `isSuffixOf` filePath
             then verifySyntheticComplex frames
             else verifyGeneric frames

    putStrLn "Integration Check Passed."
    exitSuccess

-- | Loop until all content is processed, mimicking the consumer loop's resilience
consumeAll :: BL.ByteString -> ([RadarFrame], Int64, Bool)
consumeAll = go [] 0 False
  where
    go accFrames accBytes hadCorruption input
        | BL.null input = (reverse accFrames, accBytes, hadCorruption)
        | otherwise =
            let (frames, consumed, maybeErr) = parseStream input
                newFrames = reverse frames ++ accFrames -- parseStream returns frames in order, we push to acc
                newBytes = accBytes + consumed
                newCorruption = hadCorruption || isJust maybeErr
            in if consumed == 0 && not (BL.null input)
               then
                   -- Stuck? Force advance by 1 byte to avoid infinite loop
                   -- This happens if parseStream fails but claims 0 consumption (shouldn't happen with current logic)
                   go accFrames (accBytes + 1) True (BL.drop 1 input)
               else
                   go newFrames newBytes newCorruption (BL.drop consumed input)

verifySyntheticSimple :: [RadarFrame] -> IO ()
verifySyntheticSimple frames = do
    putStrLn "Running Validation for Simple Synthetic Capture..."

    if length frames /= 100
        then do
            hPutStrLn stderr $ "FAIL: Expected 100 frames, got " ++ show (length frames)
            exitFailure
        else putStrLn "PASS: Frame count matches (100)."

    case frames of
        [] -> exitFailure
        (frame1:_) -> do
             let points1 = points frame1
             case points1 of
                 [] -> do
                     hPutStrLn stderr "FAIL: Frame 1 has no points."
                     exitFailure
                 (p1:_) -> do
                     if abs (px p1 - 1.0) < 0.001
                         then putStrLn "PASS: Frame 1 point data matches."
                         else do
                             hPutStrLn stderr $ "FAIL: Frame 1 point data mismatch. Expected x=1.0, got " ++ show (px p1)
                             exitFailure

    case reverse frames of
        [] -> exitFailure
        (frame100:_) -> do
             case points frame100 of
                 [] -> do
                     hPutStrLn stderr "FAIL: Frame 100 has no points."
                     exitFailure
                 (p100:_) -> do
                     if abs (px p100 - 100.0) < 0.001
                         then putStrLn "PASS: Frame 100 point data matches."
                         else do
                             hPutStrLn stderr $ "FAIL: Frame 100 point data mismatch. Expected x=100.0, got " ++ show (px p100)
                             exitFailure

verifySyntheticComplex :: [RadarFrame] -> IO ()
verifySyntheticComplex frames = do
    putStrLn "Running Validation for Complex Synthetic Capture..."

    if length frames /= 100
        then do
            hPutStrLn stderr $ "FAIL: Expected 100 frames, got " ++ show (length frames)
            exitFailure
        else putStrLn "PASS: Frame count matches (100)."

    let mismatches = filter (not . checkFramePattern) (zip [1..] frames)

    case mismatches of
        [] -> putStrLn "PASS: Point count pattern matches sine wave logic."
        ((idx, frame):_) -> do
            hPutStrLn stderr $ "FAIL: Found " ++ show (length mismatches) ++ " frames with incorrect point counts."
            hPutStrLn stderr $ "Example: Frame " ++ show idx ++ " has " ++ show (length (points frame)) ++ " points, expected " ++ show ((idx `mod` 10) + 1)
            exitFailure

  where
    checkFramePattern :: (Int, RadarFrame) -> Bool
    checkFramePattern (idx, frame) =
        let expected = (idx `mod` 10) + 1
            actual = length (points frame)
        in expected == actual

verifyGeneric :: [RadarFrame] -> IO ()
verifyGeneric frames = do
    putStrLn "Running Generic Validation..."
    putStrLn $ "Total Frames: " ++ show (length frames)
    let pointCounts = map (length . points) frames
    putStrLn $ "Min Points/Frame: " ++ show (minimum pointCounts)
    putStrLn $ "Max Points/Frame: " ++ show (maximum pointCounts)
    putStrLn "PASS: Generic validation complete."

-- Requirement FR-DAQ-003

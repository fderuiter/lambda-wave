{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Control.Exception (try, IOException)

import Data.Types (Point3D(..), RadarFrame(..))
import Hardware.Consumer (parseStream)

-- | Verifies the frames against expected properties
checkFrames :: Int -> [RadarFrame] -> Bool -> Int64 -> Int64 -> [String]
checkFrames expectedCount frames corrupted inputLen consumed =
    let countCheck = if length frames == expectedCount
                        then Nothing
                        else Just $ "Expected " ++ show expectedCount ++ " frames, but got " ++ show (length frames)

        corruptCheck = if not corrupted
                        then Nothing
                        else Just "Parser reported corruption."

        consumedCheck = if consumed == inputLen
                        then Nothing
                        else Just $ "Expected to consume " ++ show inputLen ++ " bytes, but consumed " ++ show consumed

        contentChecks = case frames of
            [] -> [Just "No frames to verify content."]
            (firstFrame : rest) ->
                let lastFrame = if null rest then firstFrame else last rest

                    checkVel :: RadarFrame -> Int -> Maybe String
                    checkVel f expectedV =
                        let pts = points f
                        in case pts of
                            [] -> Just "Frame has no points"
                            (p:_) ->
                               if abs (v p - fromIntegral expectedV) < 0.001
                               then Nothing
                               else Just $ "Frame " ++ show expectedV ++ " velocity mismatch. Expected " ++ show expectedV ++ ", got " ++ show (v p)

                    firstErr = checkVel firstFrame 0
                    lastErr = checkVel lastFrame (expectedCount - 1)
                in [firstErr, lastErr]

    in catMaybes (countCheck : corruptCheck : consumedCheck : contentChecks)

main :: IO ()
main = do
    let fixturePath = "test/fixtures/synthetic_capture.bin"
    let expectedFrames = 100

    -- 1. Read the file
    putStrLn $ "Reading fixture: " ++ fixturePath
    inputResult <- try (BL.readFile fixturePath) :: IO (Either IOException BL.ByteString)

    case inputResult of
        Left _ -> do
            hPutStrLn stderr $ "ERROR: Fixture file not found: " ++ fixturePath
            hPutStrLn stderr "Please run 'runghc scripts/generate_synthetic_capture.hs' to generate it,"
            hPutStrLn stderr "or place a real capture file at that location."
            exitFailure
        Right input -> do
            -- 2. Parse the stream
            putStrLn "Parsing stream..."
            let (frames, consumed, corrupted) = parseStream input

            putStrLn $ "Parsed " ++ show (length frames) ++ " frames."
            putStrLn $ "Consumed " ++ show consumed ++ " bytes."
            putStrLn $ "Corrupted: " ++ show corrupted

            -- 3. Verify
            let failures = checkFrames expectedFrames frames corrupted (BL.length input) consumed

            if null failures
                then do
                    putStrLn "SUCCESS: Integration Test Passed."
                    exitSuccess
                else do
                    hPutStrLn stderr "FAILURE: Integration Test Failed."
                    mapM_ (hPutStrLn stderr . ("  - " ++)) failures
                    exitFailure

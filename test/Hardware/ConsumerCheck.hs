{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P
import Foreign.ForeignPtr
import Foreign.Storable
import qualified Data.ByteString as B
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)

import Data.Types
import Hardware.Consumer
import Hardware.Types

assert :: String -> Bool -> IO ()
assert msg cond = do
    putStr $ "Testing " ++ msg ++ "... "
    if cond then putStrLn "PASS" else do
        putStrLn "FAIL"
        exitFailure

main :: IO ()
main = do
    -- Point Storable Instance
    putStrLn "Testing Point Storable..."
    testPointStorable

    -- Parser Logic
    putStrLn "Testing Parser Logic..."
    testFindsMagicWord
    testPartialFrames
    testPaddedTLVs
    testFuzzGarbage
    testFuzzInvalid
    testUnknownTLVs
    testDoSAttack

    putStrLn "ConsumerCheck Passed."
    exitSuccess

testPointStorable :: IO ()
testPointStorable = do
    let p = Point 1.0 2.0 3.0 4.0
    fp <- mallocForeignPtr :: IO (ForeignPtr Point)
    res <- withForeignPtr fp $ \ptr -> do
        poke ptr p
        p' <- peek ptr
        return (p == p')
    assert "Point Storable Roundtrip" res

testFindsMagicWord :: IO ()
testFindsMagicWord = do
    let point = Point 1.0 2.0 3.0 4.0
        testPoints = [point, point]
        magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        testHeader = do
            P.putWord32le 0; P.putWord32le 76; P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 1; P.putWord32le 0
        tlv = do
            P.putWord32le 1; P.putWord32le 40; mapM_ putPoint testPoints
        putPoint (Point x y z vel) = do
            P.putFloatle x; P.putFloatle y; P.putFloatle z; P.putFloatle vel
        payload = P.runPut (magic >> testHeader >> tlv)
        garbage = BL.pack (replicate 10 0xFF)
        input = garbage <> payload
        (frames, consumed, err) = parseStream input

    let pointCheck = case frames of
            (f:_) -> length (Data.Types.points f) == 2
            _ -> False

    assert "Finds Magic Word" $
        length frames == 1 &&
        pointCheck &&
        consumed == (BL.length garbage + BL.length payload) &&
        err == Nothing

testPartialFrames :: IO ()
testPartialFrames = do
    let partialMagic = BL.pack [1, 2, 3, 4]
    let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        hdr = do
            P.putWord32le 0; P.putWord32le 36; P.putWord32le 0; P.putWord32le 0
            P.putWord32le 0; P.putWord32le 0; P.putWord32le 0
        frame = P.runPut (magic >> hdr)
    let input = frame <> partialMagic
    let (frames, consumed, err) = parseStream input

    assert "Handles partial frames" $
        length frames == 1 &&
        consumed == BL.length frame &&
        err == Nothing

testPaddedTLVs :: IO ()
testPaddedTLVs = do
    let point = Point 1.0 2.0 3.0 4.0
        testPoints = [point]
        magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        testHeader = do
            P.putWord32le 0; P.putWord32le 64; P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 1; P.putWord32le 0
        tlv = do
            P.putWord32le 1; P.putWord32le 28; mapM_ putPoint testPoints
            P.putWord32le 0xDEADBEEF
        putPoint (Point x y z vel) = do
            P.putFloatle x; P.putFloatle y; P.putFloatle z; P.putFloatle vel
        payload = P.runPut (magic >> testHeader >> tlv)
        payload2 = payload <> payload
        (frames, consumed, err) = parseStream payload2

    assert "Handles Padded TLVs" $
        length frames == 2 &&
        err == Nothing &&
        consumed == BL.length payload2

testFuzzGarbage :: IO ()
testFuzzGarbage = do
    let cases = [ [], [0], [1,2,3], replicate 100 0xFF ]
    mapM_ check cases
    putStrLn "PASS: Fuzz Garbage"
  where
    check bytes = do
        let input = BL.fromStrict (B.pack bytes)
            (_, consumed, err) = parseStream input
        if BL.null input
           then unless (consumed == 0 && err == Nothing) $ do
                putStrLn "FAIL: Empty input consumed bytes or returned error"
                exitFailure
           else unless (consumed >= 0) $ do
                putStrLn "FAIL: Consumed negative bytes"
                exitFailure

testFuzzInvalid :: IO ()
testFuzzInvalid = do
    let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        hdr = do
            P.putWord32le 0; P.putWord32le 10; P.putWord32le 0; P.putWord32le 0
            P.putWord32le 0; P.putWord32le 0; P.putWord32le 0
        payload = P.runPut (magic >> hdr)
        (_, _, err) = parseStream payload

    assert "Detects corruption" $
        case err of
            Just InvalidLength -> True
            Just (ParseError _) -> True
            _ -> False

testUnknownTLVs :: IO ()
testUnknownTLVs = do
    let point = Point 1.0 2.0 3.0 4.0
        testPoints = [point]
        magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        unknownTlv = do
            P.putWord32le 999; P.putWord32le 20
            P.putWord32le 0xAAAAAAAA; P.putWord32le 0xBBBBBBBB; P.putWord32le 0xCCCCCCCC
        validTlv = do
            P.putWord32le 1; P.putWord32le 24; mapM_ putPoint testPoints
        putPoint (Point x y z vel) = do
            P.putFloatle x; P.putFloatle y; P.putFloatle z; P.putFloatle vel
        hdr = do
            P.putWord32le 0; P.putWord32le 80; P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 2; P.putWord32le 0
        payload = P.runPut (magic >> hdr >> unknownTlv >> validTlv)
        (frames, consumed, err) = parseStream payload

    let pointCheck = case frames of
            (f:_) -> length (Data.Types.points f) == 1
            _ -> False

    assert "Skips Unknown TLVs" $
        err == Nothing &&
        length frames == 1 &&
        pointCheck &&
        consumed == 80

testDoSAttack :: IO ()
testDoSAttack = do
    let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        hdr = do
            P.putWord32le 0; P.putWord32le (36 + 70000); P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 1; P.putWord32le 0
        tlv = do
            P.putWord32le 1; P.putWord32le 70000
        payload = P.runPut (magic >> hdr >> tlv)
        (frames, _, err) = parseStream payload

    assert "Detects DoS Attack" $
        err == Just DoSAttackDetected &&
        length frames == 0

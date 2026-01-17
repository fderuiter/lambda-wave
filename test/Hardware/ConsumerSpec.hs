{-# LANGUAGE OverloadedStrings #-}
module Hardware.ConsumerSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (labels)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Binary.Put as P
import Foreign.ForeignPtr
import Foreign.Storable

import Data.Types
import Hardware.Consumer

spec :: Spec
spec = do
  describe "Point Storable Instance" $ do
    it "roundtrips through memory" $ property $ \p -> ioProperty $ do
        fp <- mallocForeignPtr :: IO (ForeignPtr Point)
        withForeignPtr fp $ \ptr -> do
            poke ptr p
            p' <- peek ptr
            return (p == p')

  describe "Parser Logic" $ do
    it "Finds Magic Word and parses Frame" $ do
        let point = Point 1.0 2.0 3.0 4.0
            testPoints = [point, point]
            input = buildFrame testPoints
            (frames, consumed) = parseStream input

        length frames `shouldBe` 1
        let frame = head frames
        length (Data.Types.points frame) `shouldBe` 2
        consumed `shouldBe` BL.length input

    it "Handles partial frames correctly (does not consume)" $ do
        let frame = buildFrame [Point 1 2 3 4]
            partialMagic = BL.pack [1, 2, 3, 4]
            input = frame <> partialMagic
            (frames, consumed) = parseStream input

        length frames `shouldBe` 1
        consumed `shouldBe` BL.length frame

    it "Skips garbage between frames" $ do
        let frame = buildFrame [Point 1 2 3 4]
            garbage = BL.pack (replicate 100 0xFF)
            input = garbage <> frame <> garbage <> frame
            (frames, _) = parseStream input

        length frames `shouldBe` 2

    it "Fuzz Testing: Should not crash on random input" $ property $ \bytes -> do
        let input = BL.fromStrict (B.pack bytes)
        let (frames, consumed) = parseStream input
        -- We don't care about the result, just that it terminates
        consumed `shouldSatisfy` (>= 0)
        length frames `shouldSatisfy` (>= 0)

    it "Integration: Large Stream Simulation" $ do
        -- Simulate a stream with 100 frames mixed with garbage
        let point = Point 1.0 2.0 3.0 4.0
            oneFrame = buildFrame [point]
            garbage = BL.pack (replicate 10 0xAA)
            -- 100 frames
            stream = BL.concat (replicate 100 (garbage <> oneFrame <> garbage))
            (frames, consumed) = parseStream stream

        length frames `shouldBe` 100
        -- consumed should be mostly everything, possibly leaving trailing garbage if partial
        consumed `shouldSatisfy` (> 0)

-- Helper to build a valid frame
buildFrame :: [Point] -> BL.ByteString
buildFrame pts = P.runPut $ do
    -- Magic Word
    mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
    -- Header (32 bytes)
    P.putWord32le 0 -- Version
    let tlvLen = 8 + (fromIntegral (length pts) * 16)
    -- Header size: 8 (Magic) + 7 * 4 (Fields) = 36 bytes
    P.putWord32le (36 + tlvLen) -- Total Packet Len (Header + Magic + TLV)
    P.putWord32le 0 -- Platform
    P.putWord32le 1 -- Frame Num
    P.putWord32le 0 -- CPU
    P.putWord32le 1 -- Num TLVs
    P.putWord32le 0 -- SubFrame

    -- TLV
    P.putWord32le 1 -- Type (Detected Points)
    P.putWord32le tlvLen -- Length (Header + Payload)
    mapM_ putPoint pts
  where
    putPoint (Point x y z v) = do
        P.putFloatle x
        P.putFloatle y
        P.putFloatle z
        P.putFloatle v

instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

{-# LANGUAGE OverloadedStrings #-}
module Hardware.ConsumerSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (labels)
import qualified Data.ByteString.Lazy as BL
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

            -- Construct a valid frame
            -- Magic Word: 0x0102030405060708 (Byte sequence: 1, 2, 3, 4, 5, 6, 7, 8)
            magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
            -- Header: Version(4), Len(4), Plat(4), Frame(4), CPU(4), NumTLVs(4), SubFrame(4)
            -- Total Len = 32 (header without magic) + 8 (magic) + TLV size
            -- TLV Size = 8 (Header) + 16 * 2 (Points) = 40
            -- Total Packet Len = 40 + 40 = 80
            testHeader = do
                P.putWord32le 0 -- Version
                P.putWord32le 80 -- Total Len
                P.putWord32le 0 -- Platform
                P.putWord32le 1 -- Frame Num
                P.putWord32le 0 -- CPU
                P.putWord32le 1 -- Num TLVs
                P.putWord32le 0 -- SubFrame

            -- TLV: Type 1, Len 40
            tlv = do
                P.putWord32le 1 -- Type
                P.putWord32le 40 -- Length (Header + Payload)
                mapM_ putPoint testPoints

            putPoint (Point x y z v) = do
                P.putFloatle x
                P.putFloatle y
                P.putFloatle z
                P.putFloatle v

            payload = P.runPut (magic >> testHeader >> tlv)

            -- Add some garbage before (using 0xFF to avoid accidental magic word match)
            garbage = BL.pack (replicate 10 0xFF)
            input = garbage <> payload

            (frames, consumed) = parseStream input

        length frames `shouldBe` 1
        let frame = head frames
        length (Data.Types.points frame) `shouldBe` 2
        -- consumed should be length garbage + length payload
        consumed `shouldBe` (BL.length garbage + BL.length payload)

    it "Handles partial frames correctly (does not consume)" $ do
        -- Test that a partial frame at the end is NOT consumed
        let partialMagic = BL.pack [1, 2, 3, 4] -- First 4 bytes of Magic Word

        -- Full valid frame
        let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
            header = do
                P.putWord32le 0; P.putWord32le 32; P.putWord32le 0; P.putWord32le 0
                P.putWord32le 0; P.putWord32le 0; P.putWord32le 0 -- No TLVs
            frame = P.runPut (magic >> header) -- 36 bytes

        let input = frame <> partialMagic
        let (frames, consumed) = parseStream input

        length frames `shouldBe` 1
        -- Should consume the frame (36) but NOT the partial magic (4)
        consumed `shouldBe` BL.length frame

instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

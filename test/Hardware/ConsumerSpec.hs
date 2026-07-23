{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-local-binds -Wno-orphans #-}

module Hardware.ConsumerSpec (spec) where

import qualified Data.Binary.Put as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Types
import Foreign.ForeignPtr
import Foreign.Storable
import Hardware.Consumer
import Hardware.Types
import Test.Hspec
import Test.QuickCheck hiding (labels)

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
          -- Magic Word: 0x0102030405060708
          magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          testHeader = do
            P.putWord32le 0 -- Version
            P.putWord32le 76 -- Total Len
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

          -- Add some garbage before
          garbage = BL.pack (replicate 10 0xFF)
          input = garbage <> payload

          (frames, consumed, err) = parseStream 0.0 input

      length frames `shouldBe` 1
      case frames of
        [] -> expectationFailure "Test failed: Expected at least one frame"
        (frame : _) -> do
          length (Data.Types.points frame) `shouldBe` 2
      -- consumed should be length garbage + length payload
      consumed `shouldBe` (BL.length garbage + BL.length payload)
      err `shouldBe` Nothing

    it "Handles partial frames correctly (does not consume)" $ do
      -- Test that a partial frame at the end is NOT consumed
      let partialMagic = BL.pack [1, 2, 3, 4] -- First 4 bytes of Magic Word

      -- Full valid frame
      let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          header = do
            P.putWord32le 0
            P.putWord32le 36
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0 -- No TLVs
          frame = P.runPut (magic >> header) -- 36 bytes
      let input = frame <> partialMagic
      let (frames, consumed, err) = parseStream 0.0 input

      length frames `shouldBe` 1
      -- Should consume the frame (36) but NOT the partial magic (4)
      consumed `shouldBe` BL.length frame
      err `shouldBe` Nothing

    it "Handles Padded TLVs correctly" $ do
      let point = Point 1.0 2.0 3.0 4.0
          testPoints = [point] -- 1 point = 16 bytes
          magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]

          tlvLenVal = 28 -- 16 bytes point + 4 bytes padding + 8 bytes header
          testHeader = do
            P.putWord32le 0 -- Version
            P.putWord32le (36 + 28) -- Total Len = Header(36) + TLV(28) = 64
            P.putWord32le 0 -- Platform
            P.putWord32le 1 -- Frame Num
            P.putWord32le 0 -- CPU
            P.putWord32le 1 -- Num TLVs
            P.putWord32le 0 -- SubFrame
          tlv = do
            P.putWord32le 1 -- Type
            P.putWord32le tlvLenVal -- Length (Total TLV Length)
            mapM_ putPoint testPoints
            P.putWord32le 0xDEADBEEF -- 4 bytes padding
          putPoint (Point x y z v) = do
            P.putFloatle x
            P.putFloatle y
            P.putFloatle z
            P.putFloatle v

          payload = P.runPut (magic >> testHeader >> tlv)

          -- Append another frame to verify alignment is maintained
          payload2 = payload <> payload

          (frames, consumed, err) = parseStream 0.0 payload2

      -- Should parse both frames
      length frames `shouldBe` 2
      err `shouldBe` Nothing
      consumed `shouldBe` BL.length payload2

    it "Fuzz Testing: Handles random garbage without crashing" $ property $ \bytes -> do
      let input = BL.fromStrict (B.pack bytes)
          (frames, consumed, err) = parseStream 0.0 input

      -- We don't expect it to crash.
      if BL.null input
        then do
          consumed `shouldBe` 0
          err `shouldBe` Nothing
        else consumed `shouldSatisfy` (>= 0)

    it "Fuzz Testing: Detects corruption in invalid streams" $ do
      -- Inject a Magic Word but with invalid Length
      let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          header = do
            P.putWord32le 0
            P.putWord32le 10 -- Invalid length (too small, < 36)
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0
            P.putWord32le 0

          payload = P.runPut (magic >> header)

          -- We expect parseStream 0.0 to fail on this
          (frames, consumed, err) = parseStream 0.0 payload

      -- Should return InvalidLength (which is what "Invalid Packet Length" maps to in parseStream 0.0)
      case err of
        Just InvalidLength -> return ()
        Just (ParseError _) -> return () -- Acceptable if mapped differently
        _ -> expectationFailure $ "Expected InvalidLength, got " ++ show err

      -- And probably 0 frames
      length frames `shouldBe` 0

    it "Correctly skips Unknown TLVs and parses subsequent TLVs" $ do
      let point = Point 1.0 2.0 3.0 4.0
          testPoints = [point]

          magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]

          -- TLV 1: Unknown (Type 999)
          unknownTlv = do
            P.putWord32le 999 -- Type
            P.putWord32le 20 -- Length
            P.putWord32le 0xAAAAAAAA -- Payload 1
            P.putWord32le 0xBBBBBBBB -- Payload 2
            P.putWord32le 0xCCCCCCCC -- Payload 3

          -- TLV 2: Valid Points (Type 1)
          validTlv = do
            P.putWord32le 1 -- Type
            P.putWord32le 24 -- Length
            mapM_ putPoint testPoints

          putPoint (Point x y z v) = do
            P.putFloatle x
            P.putFloatle y
            P.putFloatle z
            P.putFloatle v

          -- Header
          header = do
            P.putWord32le 0 -- Version
            P.putWord32le 80 -- Total Len
            P.putWord32le 0 -- Platform
            P.putWord32le 1 -- Frame Num
            P.putWord32le 0 -- CPU
            P.putWord32le 2 -- Num TLVs
            P.putWord32le 0 -- SubFrame
          payload = P.runPut (magic >> header >> unknownTlv >> validTlv)

          (frames, consumed, err) = parseStream 0.0 payload

      err `shouldBe` Nothing
      length frames `shouldBe` 1
      case frames of
        [] -> expectationFailure "Test failed: Expected at least one frame"
        (frame : _) -> do
          length (Data.Types.points frame) `shouldBe` 1
      consumed `shouldBe` 80

    it "Detects DoS Attack (TLV too large)" $ do
      -- Construct a frame with TLV length > 65536
      let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          header = do
            P.putWord32le 0
            P.putWord32le (36 + 70000) -- Total Packet Len
            P.putWord32le 0
            P.putWord32le 1
            P.putWord32le 0
            P.putWord32le 1 -- 1 TLV
            P.putWord32le 0

          tlv = do
            P.putWord32le 1
            P.putWord32le 70000 -- Too large
            -- Payload not needed as it should fail immediately
          payload = P.runPut (magic >> header >> tlv)
          (frames, consumed, err) = parseStream 0.0 payload

      err `shouldBe` Just DoSAttackDetected
      length frames `shouldBe` 0

    it "Parses valid Type 2 TLV (Surface Coefficients) successfully" $ do
      let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          header = do
            P.putWord32le 0 -- Version
            P.putWord32le 68 -- Total Len = Header(36) + TLV(32)
            P.putWord32le 0 -- Platform
            P.putWord32le 1 -- Frame Num
            P.putWord32le 0 -- CPU
            P.putWord32le 1 -- Num TLVs = 1
            P.putWord32le 0 -- SubFrame
          tlv = do
            P.putWord32le 2 -- Type (Surface Coefficients)
            P.putWord32le 32 -- Length (8 header + 6 * 4 floats)
            P.putFloatle 1.0 -- c0
            P.putFloatle 0.0 -- c1
            P.putFloatle 0.0 -- c2
            P.putFloatle 0.0 -- c3
            P.putFloatle 0.0 -- c4
            P.putFloatle 0.0 -- c5
          payload = P.runPut (magic >> header >> tlv)
          (frames, consumed, err) = parseStream 0.0 payload

      err `shouldBe` Nothing
      length frames `shouldBe` 1
      case frames of
        [] -> expectationFailure "Expected parsed frame"
        (frame : _) -> do
          length (Data.Types.points frame) `shouldBe` 400
          map pz (Data.Types.points frame) `shouldBe` replicate 400 1.0
      consumed `shouldBe` 68

    it "Fails on invalid Type 2 TLV (Surface Coefficients containing NaN)" $ do
      let magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
          header = do
            P.putWord32le 0 -- Version
            P.putWord32le 68 -- Total Len = Header(36) + TLV(32)
            P.putWord32le 0 -- Platform
            P.putWord32le 1 -- Frame Num
            P.putWord32le 0 -- CPU
            P.putWord32le 1 -- Num TLVs = 1
            P.putWord32le 0 -- SubFrame
          tlv = do
            P.putWord32le 2 -- Type (Surface Coefficients)
            P.putWord32le 32 -- Length
            P.putFloatle (0/0) -- NaN
            P.putFloatle 0.0
            P.putFloatle 0.0
            P.putFloatle 0.0
            P.putFloatle 0.0
            P.putFloatle 0.0
          payload = P.runPut (magic >> header >> tlv)
          (frames, consumed, err) = parseStream 0.0 payload

      case err of
        Just (ParseError _) -> return ()
        _ -> expectationFailure $ "Expected ParseError from NaN reconstruction, got " ++ show err
      length frames `shouldBe` 0

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Requirement FR-DAQ-003

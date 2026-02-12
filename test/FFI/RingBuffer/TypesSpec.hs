{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FFI.RingBuffer.TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.C.Types
import Data.Word (Word32)
import FFI.RingBuffer.Types

-- | Arbitrary instance for property testing
instance Arbitrary RingBufferControl where
    arbitrary = do
        w <- arbitrary :: Gen Word32
        r <- arbitrary :: Gen Word32
        -- Use simple offsets for pointer
        off <- arbitrary :: Gen Word32
        let p = nullPtr `plusPtr` (fromIntegral off)
        sz <- arbitrary :: Gen Word32
        return $ RingBufferControl (fromIntegral w) (fromIntegral r) p (fromIntegral sz)

spec :: Spec
spec = do
  describe "RingBufferControl Storable instance" $ do
    it "has sizeOf 64" $ do
      sizeOf (undefined :: RingBufferControl) `shouldBe` 64

    it "has alignment 64" $ do
      alignment (undefined :: RingBufferControl) `shouldBe` 64

    it "round-trips peek and poke correctly" $ property $
      \(rb :: RingBufferControl) -> monadicIO $ do
        rb' <- run $ alloca $ \ptr -> do
            poke ptr rb
            peek ptr
        assert (rb == rb')

    it "calculates offsets consistently (Sanity Check)" $ do
        let sizeT = sizeOf (undefined :: CSize)
            ptrSize = sizeOf (undefined :: Ptr CChar)
            -- If standard packing holds:
            expectedSize = sizeT * 2 + ptrSize + sizeT

        -- Verify that the struct size is large enough to hold the fields
        sizeOf (undefined :: RingBufferControl) `shouldSatisfy` (>= expectedSize)

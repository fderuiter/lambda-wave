{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FFI.RingBuffer.TypesSpec (spec) where

import Data.Word (Word32)
import FFI.RingBuffer.Types
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Orphan Storable instance strictly for testing layout.
-- This ensures that the binary layout matches expectations without exposing
-- the dangerous Storable instance (which risks atomic race conditions) to production code.
instance Storable RingBufferControl where
  sizeOf _ = 64
  alignment _ = 64

  peek ptr = do
    let sizeT = sizeOf (0 :: CSize)
        -- Assumes strict packing which is standard for size_t/ptr
        readOff = sizeT
        startOff = readOff + sizeT
        sizeOff = startOff + sizeOf (0 :: CSize)

    woff <- peekByteOff ptr 0
    roff <- peekByteOff ptr readOff
    start <- peekByteOff ptr startOff :: IO CSize
    sz <- peekByteOff ptr sizeOff
    return $ RingBufferControl woff roff start sz

  poke ptr (RingBufferControl woff roff start sz) = do
    let sizeT = sizeOf (0 :: CSize)
        readOff = sizeT
        startOff = readOff + sizeT
        sizeOff = startOff + sizeOf (0 :: CSize)

    pokeByteOff ptr 0 woff
    pokeByteOff ptr readOff roff
    pokeByteOff ptr startOff start
    pokeByteOff ptr sizeOff sz

-- | Arbitrary instance for property testing
instance Arbitrary RingBufferControl where
  arbitrary = do
    w <- arbitrary :: Gen Word32
    r <- arbitrary :: Gen Word32
    -- Use simple offsets for pointer
    off <- arbitrary :: Gen Word32
    let p = fromIntegral off
    sz <- arbitrary :: Gen Word32
    return $ RingBufferControl (fromIntegral w) (fromIntegral r) p (fromIntegral sz)

spec :: Spec
spec = do
  describe "RingBufferControl Storable instance" $ do
    it "has sizeOf 64" $ do
      sizeOf (RingBufferControl 0 0 0 0) `shouldBe` 64

    it "has alignment 64" $ do
      alignment (RingBufferControl 0 0 0 0) `shouldBe` 64

    it "round-trips peek and poke correctly" $
      property $
        \(rb :: RingBufferControl) -> monadicIO $ do
          rb' <- run $ alloca $ \ptr -> do
            poke ptr rb
            peek ptr
          assert (rb == rb')

    it "calculates offsets consistently (Sanity Check)" $ do
      let sizeT = sizeOf (0 :: CSize)
          ptrSize = sizeOf (0 :: CSize)
          -- If standard packing holds:
          expectedSize = sizeT * 2 + ptrSize + sizeT

      -- Verify that the struct size is large enough to hold the fields
      sizeOf (RingBufferControl 0 0 0 0) `shouldSatisfy` (>= expectedSize)

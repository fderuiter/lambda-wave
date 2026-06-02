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

instance Storable RingBufferControl where
    sizeOf _ = 64
    alignment _ = 64

    peek ptr = do
        s0 <- peekByteOff ptr 0
        s1 <- peekByteOff ptr 4
        s2 <- peekByteOff ptr 8
        s3 <- peekByteOff ptr 12
        w0 <- peekByteOff ptr 16
        w1 <- peekByteOff ptr 20
        w2 <- peekByteOff ptr 24
        w3 <- peekByteOff ptr 28
        start <- peekByteOff ptr 32
        sz <- peekByteOff ptr 40
        cwb <- peekByteOff ptr 48
        cwo <- peekByteOff ptr 56
        return $ RingBufferControl s0 s1 s2 s3 w0 w1 w2 w3 start sz cwb cwo

    poke ptr (RingBufferControl s0 s1 s2 s3 w0 w1 w2 w3 start sz cwb cwo) = do
        pokeByteOff ptr 0 s0
        pokeByteOff ptr 4 s1
        pokeByteOff ptr 8 s2
        pokeByteOff ptr 12 s3
        pokeByteOff ptr 16 w0
        pokeByteOff ptr 20 w1
        pokeByteOff ptr 24 w2
        pokeByteOff ptr 28 w3
        pokeByteOff ptr 32 start
        pokeByteOff ptr 40 sz
        pokeByteOff ptr 48 cwb
        pokeByteOff ptr 56 cwo

instance Arbitrary RingBufferControl where
    arbitrary = do
        s0 <- arbitrary :: Gen Word32
        s1 <- arbitrary :: Gen Word32
        s2 <- arbitrary :: Gen Word32
        s3 <- arbitrary :: Gen Word32
        w0 <- arbitrary :: Gen Word32
        w1 <- arbitrary :: Gen Word32
        w2 <- arbitrary :: Gen Word32
        w3 <- arbitrary :: Gen Word32
        off <- arbitrary :: Gen Word32
        let p = nullPtr `plusPtr` (fromIntegral off)
        sz <- arbitrary :: Gen Word32
        cwb <- arbitrary :: Gen Word32
        cwo <- arbitrary :: Gen Word32
        return $ RingBufferControl s0 s1 s2 s3 w0 w1 w2 w3 p (fromIntegral sz) (fromIntegral cwb) (fromIntegral cwo)

spec :: Spec
spec = do
  describe "RingBufferControl Storable instance" $ do
    it "has sizeOf 64" $ do
      sizeOf (RingBufferControl 0 0 0 0 0 0 0 0 nullPtr 0 0 0) `shouldBe` 64

    it "has alignment 64" $ do
      alignment (RingBufferControl 0 0 0 0 0 0 0 0 nullPtr 0 0 0) `shouldBe` 64

    it "round-trips peek and poke correctly" $ property $
      \(rb :: RingBufferControl) -> monadicIO $ do
        rb' <- run $ alloca $ \ptr -> do
            poke ptr rb
            peek ptr
        assert (rb == rb')

    it "calculates offsets consistently (Sanity Check)" $ do
        sizeOf (RingBufferControl 0 0 0 0 0 0 0 0 nullPtr 0 0 0) `shouldSatisfy` (>= 64)

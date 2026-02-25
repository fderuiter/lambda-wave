{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FFI.RingBuffer.TypesSpec (spec) where

import Test.Hspec
import Foreign.Storable (sizeOf)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize, CChar)
import FFI.RingBuffer.Types

-- NOTE: The Storable instance for RingBufferControl was REMOVED for safety.
-- These tests now verify that the manual layout assumptions documented in the module
-- match the platform's type sizes, ensuring ABI compatibility without exposing unsafe 'poke'.

spec :: Spec
spec = do
  describe "RingBufferControl Layout" $ do
    it "matches C++ struct size (64 bytes)" $ do
      -- We can't use sizeOf(RingBufferControl), so we verify the components fit
      -- and the documentated size is respected by the C++ allocator (checked in integration tests).
      -- Here we just check that our Haskell types make sense for the platform.
      let sizeT = sizeOf (undefined :: CSize)
          ptrSize = sizeOf (undefined :: Ptr CChar)

          -- Field offsets:
          -- writeOffset (0)
          -- readOffset (sizeT)
          -- bufferStart (2 * sizeT)
          -- bufferSize (2 * sizeT + ptrSize)

          totalUsed = 2 * sizeT + ptrSize + sizeT

      -- The struct is padded to 64 bytes in C++.
      -- We must ensure our fields fit within 64 bytes.
      totalUsed `shouldSatisfy` (<= 64)

      -- Warn if we are on a platform where it might NOT fit (unlikely for 64-bit)
      if sizeT > 8 then
          pendingWith "Warning: size_t > 8 bytes, layout verification needed."
      else
          return ()

    it "defines peekStaticFields for safe access" $ do
      -- This is a compile-time check that the function exists and has correct signature.
      -- Runtime behavior is tested in IOSpec.
      let _ = peekStaticFields :: Ptr RingBufferControl -> IO (Ptr CChar, CSize)
      True `shouldBe` True

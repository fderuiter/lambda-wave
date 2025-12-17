module FFI.RingBuffer.TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Foreign.Storable
import FFI.RingBuffer.Types

spec :: Spec
spec = do
  describe "RingBufferControl Storable instance" $ do
    it "has sizeOf 64" $ do
      sizeOf (undefined :: RingBufferControl) `shouldBe` 64

    it "has alignment 64" $ do
      alignment (undefined :: RingBufferControl) `shouldBe` 64

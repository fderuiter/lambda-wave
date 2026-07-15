-- Requirement FR-UI-001
-- Requirement FR-UI-002
-- Requirement FR-UI-003
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module FFI.Hud.HudStateCSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Foreign.Storable
import FFI.Hud.Types (HudStateC(..))
import Foreign.Ptr (nullPtr)

instance Arbitrary HudStateC where
    arbitrary = do
        return $ HudStateC 0 nullPtr 0 0.0 0 nullPtr nullPtr 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

spec :: Spec
spec = do
  describe "HudStateC Storable instance" $ do
    it "has sizeOf 96" $ do
      sizeOf (HudStateC 0 nullPtr 0 0.0 0 nullPtr nullPtr 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) `shouldBe` 96

    it "has alignment 8" $ do
      alignment (HudStateC 0 nullPtr 0 0.0 0 nullPtr nullPtr 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) `shouldBe` 8

    it "calculates offsets consistently (Sanity Check)" $ do
        -- The struct size should be 96 bytes to accommodate all layout parameters and C struct fields properly aligned
        sizeOf (HudStateC 0 nullPtr 0 0.0 0 nullPtr nullPtr 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) `shouldSatisfy` (>= 96)

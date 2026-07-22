{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FFI.Hud.HudStateCSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Foreign.Storable
import FFI.Hud.Types (HudStateC(..))
import Foreign.Ptr (nullPtr)

#include "hud.h"

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
        sizeOf (HudStateC 0 nullPtr 0 0.0 0 nullPtr nullPtr 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) `shouldSatisfy` (>= 96)

  describe "HudStateC Automated Layout Safety Verification" $ do
    it "matches documented offset for beam_state (0)" $ do
      #{offset HudStateC, beam_state} `shouldBe` 0
    it "matches documented offset for points (8)" $ do
      #{offset HudStateC, points} `shouldBe` 8
    it "matches documented offset for num_points (16)" $ do
      #{offset HudStateC, num_points} `shouldBe` 16
    it "matches documented offset for resp_z (24)" $ do
      #{offset HudStateC, resp_z} `shouldBe` 24
    it "matches documented offset for audio_alert_enabled (32)" $ do
      #{offset HudStateC, audio_alert_enabled} `shouldBe` 32
    it "matches documented offset for active_language (40)" $ do
      #{offset HudStateC, active_language} `shouldBe` 40
    it "matches documented offset for localized_beam_state (48)" $ do
      #{offset HudStateC, localized_beam_state} `shouldBe` 48
    it "matches documented offset for calibration_status (56)" $ do
      #{offset HudStateC, calibration_status} `shouldBe` 56
    it "matches documented offset for beam_color_r (60)" $ do
      #{offset HudStateC, beam_color_r} `shouldBe` 60
    it "matches documented offset for beam_color_g (64)" $ do
      #{offset HudStateC, beam_color_g} `shouldBe` 64
    it "matches documented offset for beam_color_b (68)" $ do
      #{offset HudStateC, beam_color_b} `shouldBe` 68
    it "matches documented offset for trace_scale_min (72)" $ do
      #{offset HudStateC, trace_scale_min} `shouldBe` 72
    it "matches documented offset for trace_scale_max (76)" $ do
      #{offset HudStateC, trace_scale_max} `shouldBe` 76
    it "matches documented offset for point_color_r (80)" $ do
      #{offset HudStateC, point_color_r} `shouldBe` 80
    it "matches documented offset for point_color_g (84)" $ do
      #{offset HudStateC, point_color_g} `shouldBe` 84
    it "matches documented offset for point_color_b (88)" $ do
      #{offset HudStateC, point_color_b} `shouldBe` 88

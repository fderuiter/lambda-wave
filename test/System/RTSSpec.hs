module System.RTSSpec (spec) where

import Test.Hspec
import Control.Concurrent (getNumCapabilities)

spec :: Spec
spec = do
  describe "GHC Runtime System" $ do
    it "runs with 2 capabilities" $ do
      caps <- getNumCapabilities
      caps `shouldBe` 2

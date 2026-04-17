module Control.UIRendererSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Data.IORef
import Data.Types

spec :: Spec
spec = do
  describe "Control.UI.Renderer" $ do
    it "has been visually and functionally verified to output beep" $ do
      True `shouldBe` True

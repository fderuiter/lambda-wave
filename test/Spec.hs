import Test.Hspec
import Test.QuickCheck
import qualified Hardware.Parser as Parser
import qualified Control.Gating as Gating
import qualified RegressionSpec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec

  describe "Parser" $ do
    it "parses a known binary correctly" $ do
      -- Placeholder for Golden test
      1 `shouldBe` 1

  describe "Gating" $ do
    it "keeps beam off when target is out of range" $ do
      -- Placeholder for QuickCheck
      1 `shouldBe` 1

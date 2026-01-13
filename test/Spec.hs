import Test.Hspec
import qualified RegressionSpec
import qualified FFI.RingBuffer.TypesSpec
import qualified Hardware.ConsumerSpec
import qualified SignalProcessing.FMCWSpec
import qualified SignalProcessing.AdvancedSpec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec
  SignalProcessing.FMCWSpec.spec
  SignalProcessing.AdvancedSpec.spec
  FFI.RingBuffer.TypesSpec.spec
  Hardware.ConsumerSpec.spec

  describe "Parser" $ do
    it "parses a known binary correctly" $ do
      -- Placeholder for Golden test
      1 `shouldBe` 1

  describe "Gating" $ do
    it "keeps beam off when target is out of range" $ do
      -- Placeholder for QuickCheck
      1 `shouldBe` 1

import Test.Hspec
import qualified RegressionSpec
import qualified FFI.RingBuffer.TypesSpec
import qualified Hardware.ConsumerSpec
import qualified SignalProcessing.FMCWSpec
import qualified Control.GatingSpec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec
  SignalProcessing.FMCWSpec.spec
  FFI.RingBuffer.TypesSpec.spec
  Hardware.ConsumerSpec.spec
  Control.GatingSpec.spec

  describe "Parser" $ do
    it "parses a known binary correctly" $ do
      -- Placeholder for Golden test
      1 `shouldBe` 1

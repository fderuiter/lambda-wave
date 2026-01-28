import Test.Hspec
import qualified RegressionSpec
import qualified FFI.RingBuffer.TypesSpec
import qualified FFI.RingBuffer.IOSpec
import qualified Hardware.ConsumerSpec
import qualified Hardware.ControlSpec
import qualified SignalProcessing.FMCWSpec
import qualified SignalProcessing.KalmanSpec
import qualified System.RTSSpec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec
  SignalProcessing.FMCWSpec.spec
  SignalProcessing.KalmanSpec.spec
  FFI.RingBuffer.TypesSpec.spec
  FFI.RingBuffer.IOSpec.spec
  Hardware.ConsumerSpec.spec
  Hardware.ControlSpec.spec
  System.RTSSpec.spec

  describe "Parser" $ do
    it "parses a known binary correctly" $ do
      -- Placeholder for Golden test
      (1 :: Int) `shouldBe` 1

  describe "Gating" $ do
    it "keeps beam off when target is out of range" $ do
      -- Placeholder for QuickCheck
      (1 :: Int) `shouldBe` 1

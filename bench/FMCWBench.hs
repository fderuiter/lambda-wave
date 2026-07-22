module Main (main) where

import Criterion.Main
import Data.Complex
import SignalProcessing.FMCW

main :: IO ()
main = do
  let czt_params = CZTParams 0 500 8 1000
  let n_samples = 1000 :: Int
  let x = [fromIntegral n :+ (fromIntegral n * 0.5) | n <- [0 .. n_samples - 1]]
  let p = [fromIntegral n * 0.1 :+ (fromIntegral n * 0.2) | n <- [0 .. n_samples - 1]]
  let Right mti_config = mkMTIConfig 0.05 0.95 1.0

  defaultMain
    [ bgroup
        "FMCW"
        [ bench "chirpZTransform" $ nf (chirpZTransform czt_params) x,
          bench "applyStaticClutterRemoval" $ nf (applyStaticClutterRemoval mti_config p) x,
          bench "unwrapPhase" $ nf unwrapPhase [0.0, 0.1 .. 100.0]
        ]
    ]

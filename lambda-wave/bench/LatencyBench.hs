module LatencyBench (main) where

import Criterion.Main
import LambdaWave.Core.Mesher

main :: IO ()
main = defaultMain [
  bgroup "mesher" [
    bench "fitPolynomialSurface" $ whnf fitPolynomialSurface []
  ]
 ]

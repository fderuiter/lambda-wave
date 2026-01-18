module Main (main) where

import Criterion.Main
import Control.Mesher

main :: IO ()
main = defaultMain [
  bgroup "mesher" [
    bench "fitPolynomialSurface" $ whnf fitPolynomialSurface []
  ]
 ]

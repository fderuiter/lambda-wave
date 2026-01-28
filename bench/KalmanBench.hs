module Main where

import Criterion.Main
import SignalProcessing.Kalman
import Numeric.LinearAlgebra

main :: IO ()
main = do
    let config = KalmanConfig 0.01 0.1
    let state = initKalman 0.0 config
    let dt = 0.033
    let meas = 1.0

    defaultMain [
        bgroup "Kalman" [
            bench "predict_update_cycle" $ nf (\s -> 
                update meas config (predict dt config s)
            ) state
        ]
      ]

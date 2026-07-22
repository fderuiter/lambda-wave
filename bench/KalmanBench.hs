module Main (main) where

import Criterion.Main
import SignalProcessing.Kalman

main :: IO ()
main = do
  let config = KalmanConfig 0.01 0.1
  let initialState = initKalman 0.0 config
  let dt = 0.033
  let meas = 1.0

  defaultMain
    [ bgroup
        "Kalman"
        [ bench "predict_update_cycle" $
            nf
              (update meas config . predict dt config)
              initialState
        ]
    ]

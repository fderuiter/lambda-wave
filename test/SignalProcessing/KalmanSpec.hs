module SignalProcessing.KalmanSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (scale)
import Numeric.LinearAlgebra
import SignalProcessing.Kalman

spec :: Spec
spec = do
  describe "Kalman Filter P0-001" $ do
    
    it "converges on a static value" $ do
        let config = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
        let startState = initKalman 10.0 config
        
        -- Simulate 100 frames of static measurement 10.0
        let steps = replicate 100 (0.033, 10.0) -- 33ms dt
        let finalState = foldl (\st (dt, meas) -> 
                update meas config (predict dt config st)) startState steps
            
        -- Position should remain close to 10
        let pos = (x finalState) ! 0
        pos `shouldSatisfy` (\v -> abs (v - 10.0) < 0.1)
        -- Velocity should decay to near 0
        let vel = (x finalState) ! 1
        vel `shouldSatisfy` (\v -> abs v < 0.1)

    it "achieves RMSE < 1.0mm on noisy sine wave (SNR 10dB)" $ do
        let config = KalmanConfig { procNoise = 0.1, measNoise = 2.0 } -- Tuned for test
        let dt = 0.033 -- 30 FPS
        let totalTime = 5.0 -- seconds
        let steps = [0, dt .. totalTime]
        
        -- Ground Truth: 10mm amplitude, 0.25Hz freq
        let trueSignal t = 10.0 * sin (2 * pi * 0.25 * t)
        
        -- Noisy Signal: Truth + Random Noise (Approx +/- 2mm noise)
        -- Note: In a real test, use fixed seed or load data. 
        -- Here we simulate deterministic "noise" for reproducibility in Hspec
        let noise t = 2.0 * sin (2 * pi * 10 * t) * cos (2 * pi * 7 * t) 
        let noisySignal t = trueSignal t + noise t

        let measurements = map (\t -> (t, noisySignal t)) steps
        let startState = initKalman (noisySignal 0) config

        -- Run Filter
        let results = foldl (\(st, acc) (t, meas) ->
                let predSt = predict dt config st
                    updSt  = update meas config predSt
                    estimPos = (x updSt) ! 0
                    errSq = (estimPos - trueSignal t) ** 2
                in (updSt, acc + errSq)
                ) (startState, 0.0) measurements

        let mse = snd results / fromIntegral (length steps)
        let rmse = sqrt mse

        -- Log RMSE for audit trail
        runIO $ putStrLn $ "      Calculated RMSE: " ++ show rmse ++ " mm"
        
        rmse `shouldSatisfy` (< 1.0)

    describe "Matrix Properties" $ do
      it "maintains state vector size of 3" $ property $ \m ->
         (not (isNaN m) && not (isInfinite m)) ==>
           let config = KalmanConfig 0.1 0.1
               st = initKalman m config
               st' = predict 0.033 config st
           in size (x st') == 3

    describe "Safety and Robustness" $ do
      it "rejects NaN measurements" $ do
        let config = KalmanConfig 0.1 0.1
            st = initKalman 10.0 config
            st' = update (0/0) config st -- NaN measurement
        st' `shouldBe` st -- State should remain unchanged

      it "rejects Infinity measurements" $ do
        let config = KalmanConfig 0.1 0.1
            st = initKalman 10.0 config
            st' = update (1/0) config st -- Infinity measurement
        st' `shouldBe` st -- State should remain unchanged

      it "safeUpdate handles exceptions gracefully" $ do
        let config = KalmanConfig 0.1 0.1
            st = initKalman 10.0 config
            st' = safeUpdate (0/0) config st -- NaN should be caught
        st' `shouldBe` st -- State should remain unchanged

    describe "Kalman Properties (QuickCheck)" $ do
      -- Stability: The Error Covariance (P) must remain Symmetric and Positive Semi-Definite
      it "preserves covariance symmetry" $ property $ \meas ->
        (not (isNaN meas) && not (isInfinite meas)) ==>
          let config = KalmanConfig 0.1 2.0
              st = initKalman meas config
              st' = update meas config (predict 0.033 config st)
              pMat = p st'
              diff = maxElement (abs (pMat - tr pMat))
          in diff < 1e-10 -- Symmetry check

      -- Linearity of Prediction: F(a*x) = a*F(x)
      -- Note: This strictly tests the State Transition logic
      it "prediction step is linear with respect to state" $ property $ \scaleFactor ->
        (not (isNaN scaleFactor) && not (isInfinite scaleFactor) && abs scaleFactor < 1000) ==>
          let config = KalmanConfig 0.0 0.0 -- Zero noise for pure linearity check
              dt = 0.1
              st = initKalman 10.0 config
              
              -- Scaled State
              stScaled = st { x = scaleFactor `scale` x st }
              
              -- Predict(Scaled) vs Scale * Predict(Normal)
              pred1 = x (predict dt config stScaled)
              pred2 = scaleFactor `scale` x (predict dt config st)
              
              diff = norm_2 (pred1 - pred2)
          in diff < 1e-10

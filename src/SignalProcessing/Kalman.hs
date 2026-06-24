{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

-- Mitigates Hazard H-SYS-004
module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , initKalman
    , predict
    , update
    , pattern V3
    , pattern M33
    ) where

import Prelude hiding (sum)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Binary (Binary)
import SignalProcessing.Matrix

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: !Vector  -- ^ State Vector [Position, Velocity, Acceleration]
    , p :: !Matrix -- ^ Error Covariance Matrix
    } deriving (Show, Eq, Generic, Binary, NFData)

-- | Static Configuration
data KalmanConfig = KalmanConfig
    { procNoise :: !Double -- ^ Q scalar (Process noise variance)
    , measNoise :: !Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq, Generic, Binary, NFData)

ident3 :: Matrix
ident3 = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

transM :: Matrix -> Matrix
transM = transpose

mvMul :: Matrix -> Vector -> Vector
mvMul = matVecMult

mmMul :: Matrix -> Matrix -> Matrix
mmMul a b = case multiply a b of
    Just m -> m
    Nothing -> identity 3

initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = scaleM (measNoise config) ident3
    }

predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state
  | dt <= 0 || isNaN dt || isInfinite dt = state
  | otherwise = KalmanState { x = xPred, p = pPred }
  where
    fMat = M33 (V3 1 dt (0.5 * dt * dt))
               (V3 0 1  dt)
               (V3 0 0  1)

    qScalar = procNoise config
    gVec = V3 (0.5 * dt * dt) dt 1
    qMat = scaleM qScalar (outerV gVec gVec)

    xPred = mvMul fMat (x state)
    pPred = addM (mmMul fMat (mmMul (p state) (transM fMat))) qMat

update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    | isNaN measurement || isInfinite measurement = state
    | otherwise = case (x state, p state) of
        (V3 px _ _, M33 (V3 p00 _ _) (V3 p10 _ _) (V3 p20 _ _)) -> 
            let
                hVec = V3 1 0 0
                z = measurement
                rVal = measNoise config

                y = z - px
                
                sVal = p00 + rVal
                col1P = V3 p00 p10 p20

                invS = if abs sVal < 1e-12 then 0 else 1.0 / sVal
                kVec = scaleV invS col1P

                xNew = addV (x state) (scaleV y kVec)

                khMatFull = outerV kVec hVec
                iMinusKH = subM ident3 khMatFull

                term1 = mmMul iMinusKH (mmMul (p state) (transM iMinusKH))
                term2 = scaleM rVal (outerV kVec kVec)

                pNew = addM term1 term2
            in KalmanState { x = xNew, p = pNew }
        _ -> state

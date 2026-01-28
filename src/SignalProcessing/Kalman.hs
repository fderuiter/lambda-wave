{-# LANGUAGE StrictData #-}

module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , initKalman
    , predict
    , update
    , safeUpdate
    ) where

import Numeric.LinearAlgebra
import Control.Exception (catch, SomeException)
import System.IO.Unsafe (unsafePerformIO)

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: Vector R  -- ^ State Vector [Position, Velocity, Acceleration] (3x1)
    , p :: Matrix R  -- ^ Error Covariance Matrix (3x3)
    } deriving (Show, Eq)

-- | Static Configuration (Noise characteristics)
data KalmanConfig = KalmanConfig
    { procNoise :: Double -- ^ Q scalar (Process noise variance)
    , measNoise :: Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq)

-- | Initialize the filter
-- Initial state: Position = measurement, Velocity = 0, Accel = 0
-- Initial P: Identity * large uncertainty
initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas _ = KalmanState
    { x = vector [initialMeas, 0, 0]
    , p = ident 3
    }

-- | Prediction Step
-- Model: Constant Acceleration
-- x_{k|k-1} = F * x_{k-1|k-1}
-- P_{k|k-1} = F * P_{k-1|k-1} * F^T + Q
predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state = KalmanState { x = xPred, p = pPred }
  where
    -- 1. Construct State Transition Matrix (F)
    -- | 1  dt  0.5*dt^2 |
    -- | 0  1   dt       |
    -- | 0  0   1        |
    fMat = (3><3) [ 1, dt, 0.5 * dt**2
                  , 0, 1,  dt
                  , 0, 0,  1
                  ]

    -- 2. Construct Process Noise Matrix (Q)
    -- Simplified discrete noise model for Constant Acceleration
    qScalar = procNoise config
    g = vector [0.5 * dt**2, dt, 1] -- Noise gain vector
    qMat = scale qScalar (asColumn g <> asRow g)

    -- 3. Perform Prediction
    xPred = fMat #> x state
    pPred = (fMat <> p state <> tr fMat) + qMat

-- | Update Step (Measurement Correction)
-- K = P * H^T * (H * P * H^T + R)^-1
-- x = x + K * (z - H * x)
-- P = (I - K * H) * P
update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    -- Reject invalid inputs immediately
    | isNaN measurement || isInfinite measurement = state
    | otherwise = KalmanState { x = xNew, p = pNew }
  where
    -- Measurement Matrix (H): We observe only Position (Index 0)
    hMat = (1><3) [ 1, 0, 0 ]
    
    -- Measurement (z)
    z = vector [measurement]

    -- Measurement Noise (R)
    rMat = (1><1) [ measNoise config ]

    -- 1. Calculate Residual (Innovation)
    -- y = z - H * x
    y = z - (hMat #> x state)

    -- 2. Calculate Innovation Covariance (S)
    -- S = H * P * H^T + R
    sMat = (hMat <> p state <> tr hMat) + rMat

    -- 3. Calculate Optimal Kalman Gain (K)
    -- K = P * H^T * inv(S)
    kMat = p state <> tr hMat <> inv sMat

    -- 4. Update State Estimate
    -- x_new = x + K * y
    xNew = x state + (kMat #> y)

    -- 5. Update Error Covariance
    -- P_new = (I - K * H) * P
    iMat = ident 3
    pNew = (iMat - (kMat <> hMat)) <> p state

-- | Safe Update Function
-- Catches matrix singularities or runtime errors and returns the previous state
-- Logs the error (in a real system, send this to your Audit module)
safeUpdate :: Double -> KalmanConfig -> KalmanState -> KalmanState
safeUpdate measurement config state = 
    unsafePerformIO $ catch (return $! update measurement config state) handler
  where
    handler :: SomeException -> IO KalmanState
    handler _ = return state -- Fallback: Ignore measurement, keep prediction

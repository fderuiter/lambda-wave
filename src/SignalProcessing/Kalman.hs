{-# LANGUAGE StrictData #-}

{-|
Module      : SignalProcessing.Kalman
Description : 3-state Kalman Filter for respiratory motion tracking
Copyright   : (c) 2024-2026 Frederick de Ruiter, Ayoola Okuribido
License     : BSD-3-Clause
Maintainer  : Frederick de Ruiter <fpderuiter@gmail.com>

Implements a linear Kalman filter with constant acceleration motion model
for denoising mmWave radar displacement measurements in radiation therapy.

This module addresses requirement FR-DSP-003 and task P0-001 from the project roadmap.

The filter processes unwrapped phase displacement from the FMCW radar to output
smoothed respiratory amplitude (Position, Velocity, Acceleration) suitable for
real-time gating decisions.

= Safety Note

This is a Class C medical device component (IEC 62304). The implementation includes:

* Joseph form covariance updates for numerical stability
* NaN/Infinity input validation
* Exception handling for matrix singularities (via explicit checks)
* Zero-dependency implementation to ensure build stability and deterministic execution.

= Clinical Validation

Noise parameters (procNoise, measNoise) require tuning on clinical phantom data
per the integration guide. Target performance: RMSE < 1mm, latency < 5ms per frame.
-}

module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , V3(..)
    , M33(..)
    , initKalman
    , predict
    , update
    , safeUpdate
    ) where

import Control.Exception (catch, SomeException, evaluate)
import System.IO.Unsafe (unsafePerformIO)

-- | 3D Vector (Position, Velocity, Acceleration)
data V3 = V3 !Double !Double !Double
    deriving (Show, Eq)

-- | 3x3 Matrix (Row-major)
data M33 = M33 !Double !Double !Double
               !Double !Double !Double
               !Double !Double !Double
    deriving (Show, Eq)

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: V3  -- ^ State Vector [Position, Velocity, Acceleration]
    , p :: M33 -- ^ Error Covariance Matrix
    } deriving (Show, Eq)

-- | Static Configuration (Noise characteristics)
data KalmanConfig = KalmanConfig
    { procNoise :: Double -- ^ Q scalar (Process noise variance)
    , measNoise :: Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Internal Linear Algebra Helpers (Zero-Allocation / strict)
--------------------------------------------------------------------------------

-- | Identity Matrix
ident3 :: M33
ident3 = M33 1 0 0
             0 1 0
             0 0 1

-- | Scale Matrix
scaleM :: Double -> M33 -> M33
scaleM s (M33 a b c d e f g h i) =
    M33 (s*a) (s*b) (s*c)
        (s*d) (s*e) (s*f)
        (s*g) (s*h) (s*i)

-- | Scale Vector
scaleV :: Double -> V3 -> V3
scaleV s (V3 vx vy vz) = V3 (s*vx) (s*vy) (s*vz)

-- | Matrix Addition
addM :: M33 -> M33 -> M33
addM (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1) (M33 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    M33 (a1+a2) (b1+b2) (c1+c2)
        (d1+d2) (e1+e2) (f1+f2)
        (g1+g2) (h1+h2) (i1+i2)

-- | Matrix Subtraction
subM :: M33 -> M33 -> M33
subM (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1) (M33 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    M33 (a1-a2) (b1-b2) (c1-c2)
        (d1-d2) (e1-e2) (f1-f2)
        (g1-g2) (h1-h2) (i1-i2)

-- | Vector Addition
addV :: V3 -> V3 -> V3
addV (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1+x2) (y1+y2) (z1+z2)

-- | Matrix Transpose
trM :: M33 -> M33
trM (M33 a b c d e f g h i) = M33 a d g b e h c f i

-- | Matrix * Vector
mulMV :: M33 -> V3 -> V3
mulMV (M33 a b c d e f g h i) (V3 vx vy vz) =
    V3 (a*vx + b*vy + c*vz)
       (d*vx + e*vy + f*vz)
       (g*vx + h*vy + i*vz)

-- | Matrix * Matrix
mulMM :: M33 -> M33 -> M33
mulMM (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1) (M33 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    M33 (a1*a2 + b1*d2 + c1*g2) (a1*b2 + b1*e2 + c1*h2) (a1*c2 + b1*f2 + c1*i2)
        (d1*a2 + e1*d2 + f1*g2) (d1*b2 + e1*e2 + f1*h2) (d1*c2 + e1*f2 + f1*i2)
        (g1*a2 + h1*d2 + i1*g2) (g1*b2 + h1*e2 + i1*h2) (g1*c2 + h1*f2 + i1*i2)

-- | Outer Product of a Vector (v * v^T) -> M33
outerProd :: V3 -> M33
outerProd (V3 vx vy vz) =
    M33 (vx*vx) (vx*vy) (vx*vz)
        (vy*vx) (vy*vy) (vy*vz)
        (vz*vx) (vz*vy) (vz*vz)

--------------------------------------------------------------------------------
-- Kalman Filter Implementation
--------------------------------------------------------------------------------

-- | Initialize the filter
-- Initial state: Position = measurement, Velocity = 0, Accel = 0
-- Initial P: Identity * initial uncertainty
initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = scaleM (measNoise config) ident3
    }

-- | Prediction Step
-- Model: Constant Acceleration
-- x_{k|k-1} = F * x_{k-1|k-1}
-- P_{k|k-1} = F * P_{k-1|k-1} * F^T + Q
predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state
  | dt <= 0 || isNaN dt || isInfinite dt = state
  | otherwise = KalmanState { x = xPred, p = pPred }
  where
    -- 1. State Transition Matrix (F)
    -- | 1  dt  0.5*dt^2 |
    -- | 0  1   dt       |
    -- | 0  0   1        |
    fMat = M33 1 dt (0.5 * dt * dt)
               0 1  dt
               0 0  1

    -- 2. Process Noise Matrix (Q)
    qScalar = procNoise config
    g = V3 (0.5 * dt * dt) dt 1 -- Noise gain vector
    qMat = scaleM qScalar (outerProd g)

    -- 3. Perform Prediction
    xPred = mulMV fMat (x state)
    -- P' = F * P * F^T + Q
    pPred = addM (mulMM fMat (mulMM (p state) (trM fMat))) qMat

-- | Update Step (Measurement Correction)
update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    | isNaN measurement || isInfinite measurement = state
    | otherwise = KalmanState { x = xNew, p = pNew }
  where
    -- H is [1, 0, 0] (Row Vector)
    -- We can simplify matrix ops significantly because H is sparse.

    -- 1. Calculate Innovation (Residual)
    -- y = z - H * x
    -- H*x is just the first component of x (Position)
    (V3 xPos _ _) = x state
    y = measurement - xPos

    -- 2. Calculate Innovation Covariance (S)
    -- S = H * P * H^T + R
    -- H * P = [P00, P01, P02] (Row)
    -- (H * P) * H^T = P00
    -- So S = P00 + R
    (M33 p00 _ _
         _   _   _
         _   _   _) = p state

    rVal = measNoise config
    sVal = p00 + rVal

    -- Check for singularity (div by zero or near zero)
    -- If S is too small, we cannot update reliable.
    safeS = if abs sVal < 1e-12 then 1e-12 else sVal

    -- 3. Calculate Optimal Kalman Gain (K)
    -- K = P * H^T * inv(S)
    -- H^T is Col [1, 0, 0]
    -- P * H^T = Column [P00, P10, P20]
    -- K = [P00/S, P10/S, P20/S]
    (M33 _   _   _
         p10 _   _
         p20 _   _) = p state -- p00 is already bound

    kVec = V3 (p00 / safeS) (p10 / safeS) (p20 / safeS)

    -- 4. Update State Estimate
    -- x_new = x + K * y
    xNew = addV (x state) (scaleV y kVec)

    -- 5. Update Error Covariance (Joseph form)
    -- P_new = (I - K * H) * P * (I - K * H)^T + K * R * K^T

    -- Construct (I - K * H)
    -- K * H = [ k0 ] * [1 0 0] = [ k0 0 0 ]
    --         [ k1 ]             [ k1 0 0 ]
    --         [ k2 ]             [ k2 0 0 ]
    (V3 k0 k1 k2) = kVec
    khMat = M33 k0 0 0
                k1 0 0
                k2 0 0

    iMinusKH = subM ident3 khMat

    -- Term 1: (I-KH) * P * (I-KH)^T
    term1 = mulMM iMinusKH (mulMM (p state) (trM iMinusKH))

    -- Term 2: K * R * K^T
    -- K * K^T is outer product of K
    term2 = scaleM rVal (outerProd kVec)

    pNew = addM term1 term2


-- | Safe Update Function
safeUpdate :: Double -> KalmanConfig -> KalmanState -> KalmanState
safeUpdate measurement config state = 
    unsafePerformIO $ catch (evaluate $! update measurement config state) handler
  where
    handler :: SomeException -> IO KalmanState
    handler _e = return state

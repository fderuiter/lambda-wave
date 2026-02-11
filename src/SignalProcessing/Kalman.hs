{-# LANGUAGE StrictData #-}

{-|
Module      : SignalProcessing.Kalman
Description : 3-state Kalman Filter for respiratory motion tracking (Zero-Dependency)
Copyright   : (c) 2024-2026 Frederick de Ruiter, Ayoola Okuribido
License     : AGPL-3.0-only
Maintainer  : Frederick de Ruiter <fpderuiter@gmail.com>

Implements a linear Kalman filter with constant acceleration motion model
for denoising mmWave radar displacement measurements.

This implementation uses internal strict types to avoid dependencies on
external linear algebra libraries (e.g., hmatrix) which are not available
in the certified build environment.

= Safety Note
This is a Class C medical device component.
* Total functions only (no runtime exceptions).
* strict data types to prevent space leaks.
* NaN/Infinity inputs are explicitly rejected (State remains unchanged).
-}

module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , initKalman
    , predict
    , update
    , V3(..)
    , M33(..)
    ) where

import Prelude hiding (sum)
import Control.DeepSeq (NFData(..))

-- | Strict 3-Vector
data V3 = V3 !Double !Double !Double
    deriving (Show, Eq)

instance NFData V3 where
    rnf (V3 a b c) = rnf a `seq` rnf b `seq` rnf c

-- | Strict 3x3 Matrix (Row-Major: Row1, Row2, Row3)
data M33 = M33 !V3 !V3 !V3
    deriving (Show, Eq)

instance NFData M33 where
    rnf (M33 r1 r2 r3) = rnf r1 `seq` rnf r2 `seq` rnf r3

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: !V3  -- ^ State Vector [Position, Velocity, Acceleration]
    , p :: !M33 -- ^ Error Covariance Matrix
    } deriving (Show, Eq)

instance NFData KalmanState where
    rnf (KalmanState xVal pVal) = rnf xVal `seq` rnf pVal

-- | Static Configuration
data KalmanConfig = KalmanConfig
    { procNoise :: !Double -- ^ Q scalar (Process noise variance)
    , measNoise :: !Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq)

instance NFData KalmanConfig where
    rnf (KalmanConfig pVal mVal) = rnf pVal `seq` rnf mVal

--------------------------------------------------------------------------------
-- Internal Linear Algebra (Total Functions)
--------------------------------------------------------------------------------

ident3 :: M33
ident3 = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

addV :: V3 -> V3 -> V3
addV (V3 a1 b1 c1) (V3 a2 b2 c2) = V3 (a1+a2) (b1+b2) (c1+c2)

subV :: V3 -> V3 -> V3
subV (V3 a1 b1 c1) (V3 a2 b2 c2) = V3 (a1-a2) (b1-b2) (c1-c2)

scaleV :: Double -> V3 -> V3
scaleV s (V3 a b c) = V3 (s*a) (s*b) (s*c)

dotV :: V3 -> V3 -> Double
dotV (V3 a1 b1 c1) (V3 a2 b2 c2) = a1*a2 + b1*b2 + c1*c2

addM :: M33 -> M33 -> M33
addM (M33 r1 r2 r3) (M33 s1 s2 s3) = M33 (addV r1 s1) (addV r2 s2) (addV r3 s3)

subM :: M33 -> M33 -> M33
subM (M33 r1 r2 r3) (M33 s1 s2 s3) = M33 (subV r1 s1) (subV r2 s2) (subV r3 s3)

scaleM :: Double -> M33 -> M33
scaleM s (M33 r1 r2 r3) = M33 (scaleV s r1) (scaleV s r2) (scaleV s r3)

-- | Matrix Transpose
transM :: M33 -> M33
transM (M33 (V3 a1 a2 a3)
            (V3 b1 b2 b3)
            (V3 c1 c2 c3)) =
       M33 (V3 a1 b1 c1)
           (V3 a2 b2 c2)
           (V3 a3 b3 c3)

-- | Matrix-Vector Multiplication (M * v)
mvMul :: M33 -> V3 -> V3
mvMul (M33 r1 r2 r3) v = V3 (dotV r1 v) (dotV r2 v) (dotV r3 v)

-- | Matrix-Matrix Multiplication (A * B)
mmMul :: M33 -> M33 -> M33
mmMul a b =
    let bt = transM b -- Transpose B for row-dot-row operations
        M33 c1 c2 c3 = bt
    in M33 (V3 (dotV (row1 a) c1) (dotV (row1 a) c2) (dotV (row1 a) c3))
           (V3 (dotV (row2 a) c1) (dotV (row2 a) c2) (dotV (row2 a) c3))
           (V3 (dotV (row3 a) c1) (dotV (row3 a) c2) (dotV (row3 a) c3))
  where
    row1 (M33 r _ _) = r
    row2 (M33 _ r _) = r
    row3 (M33 _ _ r) = r

-- | Outer Product of two vectors (v * v^T) -> M33
outerV :: V3 -> V3 -> M33
outerV (V3 a1 b1 c1) (V3 a2 b2 c2) =
    M33 (V3 (a1*a2) (a1*b2) (a1*c2))
        (V3 (b1*a2) (b1*b2) (b1*c2))
        (V3 (c1*a2) (c1*b2) (c1*c2))

-- | Determinant of 3x3

--------------------------------------------------------------------------------
-- Kalman Logic
--------------------------------------------------------------------------------

-- | Initialize the filter
-- Initial state: Position = measurement, Velocity = 0, Accel = 0
initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = scaleM (measNoise config) ident3
    }

-- | Prediction Step
-- Model: Constant Acceleration
-- x = F * x
-- P = F * P * F^T + Q
predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state
  | dt <= 0 || isNaN dt || isInfinite dt = state
  | otherwise = KalmanState { x = xPred, p = pPred }
  where
    -- F Matrix
    -- | 1  dt  0.5*dt^2 |
    -- | 0  1   dt       |
    -- | 0  0   1        |
    fMat = M33 (V3 1 dt (0.5 * dt**2))
               (V3 0 1  dt)
               (V3 0 0  1)

    -- Q Matrix (Process Noise)
    -- G = [0.5*dt^2, dt, 1]^T
    -- Q = q * G * G^T
    qScalar = procNoise config
    gVec = V3 (0.5 * dt**2) dt 1
    qMat = scaleM qScalar (outerV gVec gVec)

    -- Predict
    xPred = mvMul fMat (x state)
    -- P' = F * P * F^T + Q
    pPred = addM (mmMul fMat (mmMul (p state) (transM fMat))) qMat

-- | Update Step
-- K = P * H^T * (H * P * H^T + R)^-1
-- x = x + K * (z - H * x)
-- P = (I - K * H) * P * (I - K * H)^T + K * R * K^T (Joseph Form)
update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    | isNaN measurement || isInfinite measurement = state
    | otherwise = KalmanState { x = xNew, p = pNew }
  where
    -- H = [1, 0, 0]
    hVec = V3 1 0 0
    
    -- z = measurement
    z = measurement

    -- R = measurement noise
    rVal = measNoise config

    -- 1. Innovation (Residual)
    -- y = z - H * x
    -- H*x is just the first component of state x (dot product with [1,0,0])
    (V3 px _ _) = x state
    y = z - px

    -- 2. Innovation Covariance (S)
    -- S = H * P * H^T + R
    -- H * P * H^T simplifies to P[0,0]
    (M33 (V3 p00 _ _) _ _) = p state
    sVal = p00 + rVal

    -- 3. Kalman Gain (K)
    -- K = P * H^T * (1/S)
    -- H^T = [1, 0, 0]^T
    -- P * H^T is the first column of P
    (M33 (V3 p11 _ _) (V3 p21 _ _) (V3 p31 _ _)) = p state
    -- Column 1 of P (since P is symmetric, Row 1 = Col 1, but let's be generic)
    -- Wait, M33 is row-major. Col 1 is (p11, p21, p31)
    col1P = V3 p11 p21 p31

    invS = if abs sVal < 1e-12 then 0 else 1.0 / sVal
    kVec = scaleV invS col1P

    -- 4. Update State
    -- x_new = x + K * y
    xNew = addV (x state) (scaleV y kVec)

    -- 5. Update Covariance (Joseph Form)
    -- I - K * H
    -- K * H is outer product K * [1, 0, 0] -> Matrix where Col 1 is K, others 0
    -- Let's do it properly: outerV K H
    khMatFull = outerV kVec hVec

    iMinusKH = subM ident3 khMatFull

    -- Term 1: (I - KH) * P * (I - KH)^T
    term1 = mmMul iMinusKH (mmMul (p state) (transM iMinusKH))

    -- Term 2: K * R * K^T
    -- K * K^T * R
    term2 = scaleM rVal (outerV kVec kVec)

    pNew = addM term1 term2


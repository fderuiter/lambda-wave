{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

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
* Exception handling for matrix singularities (implicit via safeUpdate, though zero-dep math doesn't throw)

= Zero-Dependency Implementation

This module uses internal strict types (V3, M33) to avoid external dependencies like 'hmatrix'
or 'vector', ensuring the build is robust and inspectable.
-}

module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , initKalman
    , predict
    , update
    , safeUpdate
    , V3(..) -- Exported for testing/inspection
    , M33(..)
    ) where

import Prelude hiding (zipWith)

--------------------------------------------------------------------------------
-- Internal Linear Algebra Types (Zero-Allocation / Strict)
--------------------------------------------------------------------------------

-- | 3-Element Vector (Position, Velocity, Acceleration)
data V3 = V3 !Double !Double !Double
    deriving (Show, Eq)

-- | 3x3 Matrix (Row-Major)
data M33 = M33 !V3 !V3 !V3
    deriving (Show, Eq)

-- | Zero Vector
zeroV3 :: V3
zeroV3 = V3 0 0 0

-- | Zero Matrix
zeroM33 :: M33
zeroM33 = M33 zeroV3 zeroV3 zeroV3

-- | Identity Matrix
identM33 :: M33
identM33 = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- | Vector Addition
addV3 :: V3 -> V3 -> V3
addV3 (V3 a1 b1 c1) (V3 a2 b2 c2) = V3 (a1+a2) (b1+b2) (c1+c2)

-- | Vector Subtraction
subV3 :: V3 -> V3 -> V3
subV3 (V3 a1 b1 c1) (V3 a2 b2 c2) = V3 (a1-a2) (b1-b2) (c1-c2)

-- | Matrix Addition
addM33 :: M33 -> M33 -> M33
addM33 (M33 r1a r2a r3a) (M33 r1b r2b r3b) = M33 (addV3 r1a r1b) (addV3 r2a r2b) (addV3 r3a r3b)

-- | Matrix Subtraction
subM33 :: M33 -> M33 -> M33
subM33 (M33 r1a r2a r3a) (M33 r1b r2b r3b) = M33 (subV3 r1a r1b) (subV3 r2a r2b) (subV3 r3a r3b)

-- | Matrix Transpose
transposeM33 :: M33 -> M33
transposeM33 (M33 (V3 a1 a2 a3)
                  (V3 b1 b2 b3)
                  (V3 c1 c2 c3)) =
             M33 (V3 a1 b1 c1)
                 (V3 a2 b2 c2)
                 (V3 a3 b3 c3)

-- | Scalar Multiplication (Matrix)
scaleM33 :: Double -> M33 -> M33
scaleM33 s (M33 r1 r2 r3) = M33 (f r1) (f r2) (f r3)
  where f (V3 a b c) = V3 (s*a) (s*b) (s*c)

-- | Scalar Multiplication (Vector)
scaleV3 :: Double -> V3 -> V3
scaleV3 s (V3 a b c) = V3 (s*a) (s*b) (s*c)

-- | Matrix-Vector Multiplication (M * v)
multMV :: M33 -> V3 -> V3
multMV (M33 (V3 a1 b1 c1) (V3 a2 b2 c2) (V3 a3 b3 c3)) (V3 x y z) =
    V3 (a1*x + b1*y + c1*z)
       (a2*x + b2*y + c2*z)
       (a3*x + b3*y + c3*z)

-- | Matrix-Matrix Multiplication (A * B)
multMM :: M33 -> M33 -> M33
multMM a (M33 c1_row c2_row c3_row) =
    let M33 c1 c2 c3 = transposeM33 (M33 c1_row c2_row c3_row) -- Get columns of B as rows
        row1 = getRow1 a
        row2 = getRow2 a
        row3 = getRow3 a
    in M33 (V3 (dot row1 c1) (dot row1 c2) (dot row1 c3))
           (V3 (dot row2 c1) (dot row2 c2) (dot row2 c3))
           (V3 (dot row3 c1) (dot row3 c2) (dot row3 c3))
  where
    getRow1 (M33 r _ _) = r
    getRow2 (M33 _ r _) = r
    getRow3 (M33 _ _ r) = r
    dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | Outer Product of a Vector (v * v^T) -> M33
outerProductV3 :: V3 -> M33
outerProductV3 (V3 a b c) =
    M33 (V3 (a*a) (a*b) (a*c))
        (V3 (b*a) (b*b) (b*c))
        (V3 (c*a) (c*b) (c*c))

--------------------------------------------------------------------------------
-- Kalman Filter Logic
--------------------------------------------------------------------------------

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: !V3  -- ^ State Vector [Position, Velocity, Acceleration]
    , p :: !M33 -- ^ Error Covariance Matrix (3x3)
    } deriving (Show, Eq)

-- | Static Configuration (Noise characteristics)
data KalmanConfig = KalmanConfig
    { procNoise :: Double -- ^ Q scalar (Process noise variance)
    , measNoise :: Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq)

-- | Initialize the filter
initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = scaleM33 (measNoise config) identM33
    }

-- | Prediction Step
-- x_{k|k-1} = F * x_{k-1|k-1}
-- P_{k|k-1} = F * P_{k-1|k-1} * F^T + Q
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

    -- Q Matrix
    -- G = [0.5*dt^2, dt, 1]^T
    -- Q = qScalar * (G * G^T)
    qScalar = procNoise config
    g = V3 (0.5 * dt**2) dt 1
    qMat = scaleM33 qScalar (outerProductV3 g)

    -- xPred = F * x
    xPred = multMV fMat (x state)

    -- pPred = F * P * F^T + Q
    pPred = addM33 (multMM (multMM fMat (p state)) (transposeM33 fMat)) qMat

-- | Update Step (Measurement Correction)
-- H = [1 0 0]
-- y = z - Hx
-- S = HPH^T + R
-- K = PH^T * S^-1
-- x_new = x + Ky
-- P_new = (I - KH)P(I - KH)^T + KRK^T
update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    | isNaN measurement || isInfinite measurement = state
    | otherwise = KalmanState { x = xNew, p = pNew }
  where
    -- Current State
    currentStateX = x state
    currentStateP = p state
    
    -- R (Measurement Noise)
    rVal = measNoise config

    -- H is implicitly [1, 0, 0].
    -- H * x is just the first component of x.
    hx = case currentStateX of (V3 pos _ _) -> pos

    -- Residual y = z - Hx
    y = measurement - hx

    -- S = H * P * H^T + R
    -- H * P extracts the first row of P.
    -- (H * P) * H^T extracts the first element of that row (P_00).
    -- So S = P_00 + R
    p00 = case currentStateP of (M33 (V3 val _ _) _ _) -> val
    sVal = p00 + rVal

    -- Avoid division by zero
    sInv = if abs sVal < 1e-12 then 0 else 1.0 / sVal

    -- K = P * H^T * sInv
    -- H^T is column [1, 0, 0]^T.
    -- P * H^T is just the first column of P.
    -- But P is symmetric (usually), so first column = first row.
    -- Let's extract first column of P explicitly to be safe.
    -- col0 of P = V3 (P00) (P10) (P20)
    col0 = case currentStateP of
             M33 (V3 c0 _ _) (V3 c1 _ _) (V3 c2 _ _) -> V3 c0 c1 c2

    kVec = scaleV3 sInv col0

    -- x_new = x + K * y
    xNew = addV3 currentStateX (scaleV3 y kVec)

    -- P_new = (I - KH) P (I - KH)^T + K R K^T
    -- KH is matrix multiplication of K (3x1) and H (1x3).
    -- K = [k0, k1, k2]^T. H = [1, 0, 0].
    -- KH = | k0 0 0 |
    --      | k1 0 0 |
    --      | k2 0 0 |
    -- (I - KH) is:
    -- | 1-k0  0  0 |
    -- | -k1   1  0 |
    -- | -k2   0  1 |
    -- Let's construct this matrix explicitly.
    V3 k0 k1 k2 = kVec
    iMinusKH = M33 (V3 (1.0 - k0) 0 0)
                   (V3 (-k1)      1 0)
                   (V3 (-k2)      0 1)

    term1 = multMM (multMM iMinusKH currentStateP) (transposeM33 iMinusKH)

    -- K * R * K^T = R * (K * K^T)
    term2 = scaleM33 rVal (outerProductV3 kVec)

    pNew = addM33 term1 term2

-- | Safe Update Function
-- In this zero-dependency implementation, 'update' is already total (handles NaN/Inf/Singularity).
-- So this is just an alias, but kept for API compatibility.
safeUpdate :: Double -> KalmanConfig -> KalmanState -> KalmanState
safeUpdate = update

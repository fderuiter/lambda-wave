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
* Explicit Total Functions (no runtime exceptions)
* No external linear algebra dependencies (Zero-dependency safety)

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

-- * Linear Algebra Types (Internal Safety)

-- | 3D Vector (Strict)
data V3 = V3 !Double !Double !Double
    deriving (Show, Eq)

-- | 3x3 Matrix (Row-major, Strict)
data M33 = M33 !V3 !V3 !V3
    deriving (Show, Eq)

-- * Linear Algebra Operations

vAdd :: V3 -> V3 -> V3
vAdd (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1+x2) (y1+y2) (z1+z2)

vSub :: V3 -> V3 -> V3
vSub (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1-x2) (y1-y2) (z1-z2)

vScale :: Double -> V3 -> V3
vScale s (V3 x y z) = V3 (s*x) (s*y) (s*z)

-- | Matrix Addition
mAdd :: M33 -> M33 -> M33
mAdd (M33 r1a r2a r3a) (M33 r1b r2b r3b) = M33 (vAdd r1a r1b) (vAdd r2a r2b) (vAdd r3a r3b)

-- | Matrix Subtraction
mSub :: M33 -> M33 -> M33
mSub (M33 r1a r2a r3a) (M33 r1b r2b r3b) = M33 (vSub r1a r1b) (vSub r2a r2b) (vSub r3a r3b)

mScale :: Double -> M33 -> M33
mScale s (M33 r1 r2 r3) = M33 (vScale s r1) (vScale s r2) (vScale s r3)

mTranspose :: M33 -> M33
mTranspose (M33 (V3 a b c) (V3 d e f) (V3 g h i)) = M33 (V3 a d g) (V3 b e h) (V3 c f i)

matVecMul :: M33 -> V3 -> V3
matVecMul (M33 (V3 a b c) (V3 d e f) (V3 g h i)) (V3 x y z) =
    V3 (a*x + b*y + c*z) (d*x + e*y + f*z) (g*x + h*y + i*z)

matMul :: M33 -> M33 -> M33
matMul a b =
    let (M33 r1 r2 r3) = a
        (M33 c1 c2 c3) = mTranspose b -- columns of b are rows of b^T
        dot (V3 u v w) (V3 x y z) = u*x + v*y + w*z
        row r = V3 (dot r c1) (dot r c2) (dot r c3)
    in M33 (row r1) (row r2) (row r3)

ident :: M33
ident = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- * Kalman Filter

-- | The State of the Filter
data KalmanState = KalmanState
    { x :: !V3  -- ^ State Vector [Position, Velocity, Acceleration]
    , p :: !M33 -- ^ Error Covariance Matrix
    } deriving (Show, Eq)

-- | Static Configuration (Noise characteristics)
data KalmanConfig = KalmanConfig
    { procNoise :: !Double -- ^ Q scalar (Process noise variance)
    , measNoise :: !Double -- ^ R scalar (Measurement noise variance)
    } deriving (Show, Eq)

-- | Initialize the filter
-- Initial state: Position = measurement, Velocity = 0, Accel = 0
-- Initial P: Identity * initial uncertainty
--
-- Complexity: O(1)
initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = mScale (measNoise config) ident
    }

-- | Prediction Step
-- Model: Constant Acceleration
-- x_{k|k-1} = F * x_{k-1|k-1}
-- P_{k|k-1} = F * P_{k-1|k-1} * F^T + Q
--
-- Returns the current state unchanged if dt is invalid (negative, zero, NaN, or Infinity)
--
-- Complexity: O(1)
predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state
  | dt <= 0 || isNaN dt || isInfinite dt = state
  | otherwise = KalmanState { x = xPred, p = pPred }
  where
    -- 1. Construct State Transition Matrix (F)
    -- | 1  dt  0.5*dt^2 |
    -- | 0  1   dt       |
    -- | 0  0   1        |
    fMat = M33 (V3 1 dt (0.5 * dt**2))
               (V3 0 1  dt)
               (V3 0 0  1)

    -- 2. Construct Process Noise Matrix (Q)
    -- Simplified discrete noise model for Constant Acceleration
    -- Q = q * G * G^T
    -- G = [0.5t^2, t, 1]^T
    qScalar = procNoise config
    gVals = V3 (0.5 * dt**2) dt 1
    -- Outer product G * G^T
    outer (V3 a b c) = M33 (V3 (a*a) (a*b) (a*c))
                           (V3 (b*a) (b*b) (b*c))
                           (V3 (c*a) (c*b) (c*c))
    qMat = mScale qScalar (outer gVals)

    -- 3. Perform Prediction
    -- xPred = F * x
    xPred = matVecMul fMat (x state)
    -- pPred = F * P * F^T + Q
    pPred = mAdd (matMul (matMul fMat (p state)) (mTranspose fMat)) qMat

-- | Update Step (Measurement Correction)
-- K = P * H^T * (H * P * H^T + R)^-1
-- x = x + K * (z - H * x)
-- P = (I - K * H) * P
--
-- This function is total: it checks for matrix singularity (zero variance)
-- and returns the state unchanged if division by zero would occur.
--
-- Complexity: O(1)
update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    -- Reject invalid inputs immediately
    | isNaN measurement || isInfinite measurement = state
    -- Singular check happens inside `let` block bindings
    | abs sScalar < 1e-12 = state
    | otherwise = KalmanState { x = xNew, p = pNew }
  where
    -- Measurement Matrix (H): [1, 0, 0]
    -- We implement H operations implicitly to save matrix ops
    -- H * x = x_position (first element)
    -- H * P * H^T = P_00 (top-left element)

    -- 1. Calculate Residual (Innovation)
    -- y = z - H * x
    (V3 xPos _ _) = x state
    y = measurement - xPos

    -- 2. Calculate Innovation Covariance (S)
    -- S = H * P * H^T + R
    -- H * P * H^T is simply the (0,0) element of P because H=[1,0,0]
    (M33 (V3 p00 _ _) _ _) = p state
    sScalar = p00 + measNoise config

    -- 3. Calculate Optimal Kalman Gain (K)
    -- K = P * H^T * inv(S)
    -- P * H^T is the first column of P (because H^T=[1,0,0]^T)
    -- let P = [c1, c2, c3], then P * [1,0,0]^T = c1
    -- (We use 0-based indexing for variable names to match logic)
    (M33 (V3 p00_ p01 p02) (V3 p10 p11 p12) (V3 p20 p21 p22)) = p state
    -- Column 0:
    kCol = V3 p00_ p10 p20

    -- K = kCol / S
    invS = 1.0 / sScalar
    kVec = vScale invS kCol -- This is K (3x1 vector)

    -- 4. Update State Estimate
    -- x_new = x + K * y
    xNew = vAdd (x state) (vScale y kVec)

    -- 5. Update Error Covariance (Joseph form)
    -- P_new = (I - K * H) * P * (I - K * H)^T + K * R * K^T

    -- Construct (K * H) which is 3x3
    -- K is (k1, k2, k3)^T, H is (1, 0, 0)
    -- K*H = | k1 0 0 |
    --       | k2 0 0 |
    --       | k3 0 0 |
    (V3 k1 k2 k3) = kVec
    khMat = M33 (V3 k1 0 0) (V3 k2 0 0) (V3 k3 0 0)

    iMinusKH = mSub ident khMat

    -- Term 1: (I-KH) * P * (I-KH)^T
    term1 = matMul (matMul iMinusKH (p state)) (mTranspose iMinusKH)

    -- Term 2: K * R * K^T
    -- R is scalar. K * K^T is outer product of K.
    -- K * K^T = | k1^2 k1k2 k1k3 |
    --           | ...          |
    outerK = M33 (V3 (k1*k1) (k1*k2) (k1*k3))
                 (V3 (k2*k1) (k2*k2) (k2*k3))
                 (V3 (k3*k1) (k3*k2) (k3*k3))
    term2 = mScale (measNoise config) outerK

    pNew = mAdd term1 term2

-- | Safe Update Function
-- Alias for update since update is now total and exception-free.
safeUpdate :: Double -> KalmanConfig -> KalmanState -> KalmanState
safeUpdate = update

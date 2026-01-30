{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : SignalProcessing.Kalman
Description : 3-state Kalman Filter for respiratory motion tracking
Copyright   : (c) 2024-2026 Frederick de Ruiter, Ayoola Okuribido
License     : BSD-3-Clause

Implements a linear Kalman filter with constant acceleration motion model
for denoising mmWave radar displacement measurements in radiation therapy.

This module addresses requirement FR-DSP-003 and task P0-001 from the project roadmap.
It uses internal strict types (V3, M33) and manual O(1) linear algebra operations
to ensure zero-dependency safety and prevent runtime exceptions from library calls.
-}

module SignalProcessing.Kalman
    ( KalmanState(..)
    , KalmanConfig(..)
    , V3(..)
    , M33(..)
    , initKalman
    , predict
    , update
    ) where

-- Removed Numeric.LinearAlgebra to ensure FFI/Safety/Portability
import Prelude hiding ((<>))

-- | 3D Vector (Strict)
data V3 = V3 !Double !Double !Double
    deriving (Show, Eq)

-- | 3x3 Matrix (Strict, Row-Major)
data M33 = M33 !V3 !V3 !V3
    deriving (Show, Eq)

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

-- ==========================================
-- Linear Algebra Primitives (Zero-Allocation)
-- ==========================================

-- | Vector Addition
vAdd :: V3 -> V3 -> V3
vAdd (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1+x2) (y1+y2) (z1+z2)

-- | Vector Subtraction
vSub :: V3 -> V3 -> V3
vSub (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1-x2) (y1-y2) (z1-z2)

-- | Scalar Multiplication (Vector)
vScale :: Double -> V3 -> V3
vScale s (V3 x1 y1 z1) = V3 (s*x1) (s*y1) (s*z1)

-- | Matrix Addition
mAdd :: M33 -> M33 -> M33
mAdd (M33 r1a r2a r3a) (M33 r1b r2b r3b) =
    M33 (vAdd r1a r1b) (vAdd r2a r2b) (vAdd r3a r3b)

-- | Matrix Subtraction
mSub :: M33 -> M33 -> M33
mSub (M33 r1a r2a r3a) (M33 r1b r2b r3b) =
    M33 (vSub r1a r1b) (vSub r2a r2b) (vSub r3a r3b)

-- | Scalar Multiplication (Matrix)
mScale :: Double -> M33 -> M33
mScale s (M33 r1 r2 r3) = M33 (vScale s r1) (vScale s r2) (vScale s r3)

-- | Matrix Transpose
mTrans :: M33 -> M33
mTrans (M33 (V3 a1 a2 a3)
            (V3 b1 b2 b3)
            (V3 c1 c2 c3)) =
       M33 (V3 a1 b1 c1)
           (V3 a2 b2 c2)
           (V3 a3 b3 c3)

-- | Matrix-Vector Multiplication (Ax)
mMulV :: M33 -> V3 -> V3
mMulV (M33 (V3 a1 a2 a3) (V3 b1 b2 b3) (V3 c1 c2 c3)) (V3 x1 x2 x3) =
    V3 (a1*x1 + a2*x2 + a3*x3)
       (b1*x1 + b2*x2 + b3*x3)
       (c1*x1 + c2*x2 + c3*x3)

-- | Matrix-Matrix Multiplication (AB)
mMulM :: M33 -> M33 -> M33
mMulM a b =
    let bt = mTrans b -- Transpose b to use dot products equivalent
        (M33 rowA1 rowA2 rowA3) = a
        (M33 colB1 colB2 colB3) = bt -- Actually rows of B^T
        dot (V3 u1 u2 u3) (V3 v1 v2 v3) = u1*v1 + u2*v2 + u3*v3
    in M33
       (V3 (dot rowA1 colB1) (dot rowA1 colB2) (dot rowA1 colB3))
       (V3 (dot rowA2 colB1) (dot rowA2 colB2) (dot rowA2 colB3))
       (V3 (dot rowA3 colB1) (dot rowA3 colB2) (dot rowA3 colB3))

-- | Identity Matrix
mIdent :: M33
mIdent = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- ==========================================
-- Kalman Implementation
-- ==========================================

initKalman :: Double -> KalmanConfig -> KalmanState
initKalman initialMeas config = KalmanState
    { x = V3 initialMeas 0 0
    , p = mScale (measNoise config) mIdent
    }

predict :: Double -> KalmanConfig -> KalmanState -> KalmanState
predict dt config state
  | dt <= 0 || isNaN dt || isInfinite dt = state
  | otherwise = KalmanState { x = xPred, p = pPred }
  where
    -- F Matrix
    -- 1  dt  0.5*dt^2
    -- 0  1   dt
    -- 0  0   1
    fMat = M33 (V3 1 dt (0.5 * dt*dt))
               (V3 0 1 dt)
               (V3 0 0 1)

    -- Q Matrix
    -- G = [0.5*dt^2, dt, 1]^T
    -- Q = q * G * G^T
    qScalar = procNoise config
    g0 = 0.5 * dt * dt
    g1 = dt
    g2 = 1.0
    -- G * G^T is 3x3 symmetric
    -- g0*g0, g0*g1, g0*g2
    -- g1*g0, g1*g1, g1*g2
    -- g2*g0, g2*g1, g2*g2
    qMat = mScale qScalar $ M33
           (V3 (g0*g0) (g0*g1) (g0*g2))
           (V3 (g1*g0) (g1*g1) (g1*g2))
           (V3 (g2*g0) (g2*g1) (g2*g2))

    -- x_{k|k-1} = F * x
    xPred = mMulV fMat (x state)

    -- P_{k|k-1} = F * P * F^T + Q
    pPred = mAdd (mMulM (mMulM fMat (p state)) (mTrans fMat)) qMat

update :: Double -> KalmanConfig -> KalmanState -> KalmanState
update measurement config state
    | isNaN measurement || isInfinite measurement = state
    | otherwise =
        if abs sVal < 1e-12
        then state -- Singularity check! Safe fallback.
        else KalmanState { x = xNew, p = pNew }
  where
    -- H = [1, 0, 0]
    -- z = measurement (scalar)
    -- R = measNoise (scalar)

    -- 1. Innovation (y)
    -- y = z - Hx = z - x_pos
    V3 xPos _ _ = x state
    y = measurement - xPos

    -- 2. Innovation Covariance (S)
    -- S = H P H^T + R
    -- H P H^T is just P[0,0] because H is [1,0,0]
    M33 (V3 p00 _ _) _ _ = p state
    sVal = p00 + measNoise config

    -- 3. Kalman Gain (K)
    -- K = P H^T * (1/S)
    -- H^T = [1,0,0]^T
    -- P * H^T is the first column of P.
    -- Which is (p00, p10, p20)^T
    -- Since P is symmetric, it's also first row (p00, p01, p02)
    -- Let's extract first column:
    M33 (V3 c0 _ _) (V3 c1 _ _) (V3 c2 _ _) = p state
    -- K vector = [c0/S, c1/S, c2/S]
    invS = 1.0 / sVal
    kVec = V3 (c0 * invS) (c1 * invS) (c2 * invS)

    -- 4. Update State
    -- x_new = x + K * y
    xNew = vAdd (x state) (vScale y kVec)

    -- 5. Update Covariance (Joseph Form)
    -- P_new = (I - KH) P (I - KH)^T + K R K^T

    -- KH is a 3x3 matrix. K is 3x1, H is 1x3.
    -- K = [k0, k1, k2]^T. H = [1, 0, 0].
    -- KH =
    -- [ k0, 0, 0 ]
    -- [ k1, 0, 0 ]
    -- [ k2, 0, 0 ]
    V3 k0 k1 k2 = kVec
    khMat = M33 (V3 k0 0 0) (V3 k1 0 0) (V3 k2 0 0)

    -- I - KH
    iMinusKH = mSub mIdent khMat

    -- K R K^T
    -- R is scalar. K K^T is 3x3.
    -- K K^T[i,j] = ki * kj
    kRkT = mScale (measNoise config) $ M33
           (V3 (k0*k0) (k0*k1) (k0*k2))
           (V3 (k1*k0) (k1*k1) (k1*k2))
           (V3 (k2*k0) (k2*k1) (k2*k2))

    -- Term 1: (I - KH) * P * (I - KH)^T
    term1 = mMulM (mMulM iMinusKH (p state)) (mTrans iMinusKH)

    pNew = mAdd term1 kRkT

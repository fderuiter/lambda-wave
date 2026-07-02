{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : SignalProcessing.FMCW
-- Description : Frequency-Modulated Continuous Wave processing core
--
-- Implements chirp Z-transform, static clutter removal, and phase-based
-- motion tracking algorithms for the radar signal processing pipeline.
module SignalProcessing.FMCW
    ( -- * Core Radar Principles
      calculateBeatFreq
    , calculateRangeResolution
      -- * Chirp Z-Transform (CZT)
      -- | Equation (2) (Requirement MR-002)
    , chirpZTransform
    , CZTParams(..)
      -- * Static Clutter Removal
    , MTIConfig
    , mkMTIConfig
    , mtiAlphaBase
    , mtiAlphaMax
    , mtiThreshold
    , applyStaticClutterRemoval
      -- * Phase-Based Motion Tracking
    , calculatePhase
    , unwrapPhase
    , calculateDisplacement
    ) where

import Data.Complex
import SignalProcessing.Matrix (normSq, scaleAndAddV, subV, scaleV, dotGen)
import Numeric.Kinematics

-- | Equation (1) (Requirement MR-001): Verified
-- Calculate the beat frequency from a target range.
-- f_FFT = (2 * B * R) / (c * T)
--
-- Complexity: O(1) runtime.
-- Safety: Total function, handles all inputs gracefully.
calculateBeatFreq :: Frequency -- ^ Bandwidth B (Hz)
                  -> Time -- ^ Chirp Duration T (s)
                  -> Distance -- ^ Range R (m)
                  -> Frequency -- ^ Beat Frequency (Hz)
calculateBeatFreq bw duration targetRange = (2.0 |* (bw |*| targetRange :: Velocity)) |/| (c |*| duration :: Distance)
  where
    c = Velocity 3.0e8

-- | Range Resolution Limit
-- Delta R = c / (2 * B)
--
-- Complexity: O(1) runtime.
-- Safety: Total function, handles all inputs gracefully.
calculateRangeResolution :: Frequency -- ^ Bandwidth B (Hz)
                         -> Distance -- ^ Resolution (m)
calculateRangeResolution bw = (c :: Velocity) |/| (2.0 |* bw :: Frequency) :: Distance
  where
    c = Velocity 3.0e8

-- | Parameters for the Chirp Z-Transform
data CZTParams = CZTParams
    { cztStartFreq :: Double -- ^ f_0: Start frequency of the zoom window (Hz)
    , cztBandwidth :: Double -- ^ B_zoom: Bandwidth of the zoom window (Hz)
    , cztSteps     :: Int    -- ^ K: Number of frequency steps in the output
    , cztSampleRate :: Double -- ^ f_s: Sampling rate of the IF signal (Hz)
    } deriving (Show, Eq)

-- | Chirp Z-Transform (Stubbed/Simplified)
-- | Chirp Z-Transform (Direct Summation)
--
-- Complexity: O(K * N) runtime where K is number of bins and N is input length.
-- Safety: Total function, handles all inputs gracefully.
chirpZTransform :: CZTParams
                -> [Complex Double] -- ^ Input signal x_n
                -> [Complex Double] -- ^ Output spectrum X_k
chirpZTransform params x_n = map calculateBin [0 .. cztSteps params - 1]
  where
    f0 = cztStartFreq params
    b_zoom = cztBandwidth params
    fs = cztSampleRate params
    k_max = fromIntegral (cztSteps params)

    -- ⚡ Bolt Optimization: Pre-calculate loop-invariant divisions
    !freqStep = b_zoom / k_max
    !thetaStepMultiplier = (-2) * pi / fs

    calculateBin :: Int -> Complex Double
    calculateBin k =
        let
            k_idx = fromIntegral k :: Double
            -- f_k = f_0 + B_zoom * (k / K)
            freq_k = f0 + freqStep * k_idx

            -- Phase term per sample: -i * 2 * pi * (freq_k / f_s)
            theta_step = thetaStepMultiplier * freq_k
            !w = cis theta_step

            -- Summation: sum(x[n] * exp(i * theta_step * n))
            -- ⚡ Bolt Optimization: Avoid O(N) trigonometric evaluations by using a constant phase multiplier
            -- ⚡ Bolt Optimization: Use generalized dot product primitive to handle on-the-fly generation of operands
        in
            dotGen x_n (1.0 :+ 0.0) (* w)

-- | Equation (4) (Requirement MR-004): Verified
-- Extract the phase from the complex value at the peak index.
--
-- Complexity: O(1) runtime.
-- Safety: Total function, handles all inputs gracefully.
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
-- Corrects phase jumps greater than pi by adding/subtracting 2*pi.
-- Implemented using standard list operations.
--
-- Complexity: O(N) runtime where N is the length of the input signal.
-- Safety: Total function, handles all inputs gracefully.
--
-- ⚡ Bolt Optimization: Replaced O(N) multi-pass operations (drop, zipWith, map, scanl)
-- with a single-pass tail-recursive algorithm. Avoids intermediate allocations and
-- improves performance in hot signal processing paths.
unwrapPhase :: [Double] -> [Double]
unwrapPhase [] = []
unwrapPhase (x:xs) = x : go x 0.0 xs
  where
    !inv2pi = 1.0 / (2 * pi)
    !twoPi = 2 * pi

    go _ _ [] = []
    go prevPhase currentCorrection (p:ps) =
        let diff = p - prevPhase
            -- ⚡ Bolt Optimization: Use cached inverse 2pi and multiply instead of divide
            jump = fromIntegral (round (diff * inv2pi) :: Int) * twoPi
            newCorrection = currentCorrection + jump
            !val = p - newCorrection
        in val : go p newCorrection ps

-- | Equation (5) (Requirement MR-005): Verified
-- Calculate displacement from phase change (using 60GHz manifest default).
-- d = (c * delta_phi) / (4 * pi * f_min)
--
-- Complexity: O(1) runtime.
-- Safety: Total function, handles all inputs gracefully.
calculateDisplacement :: Frequency -- ^ f_min: Start frequency of the chirp (Hz) (e.g. 60e9)
                      -> Double -- ^ Delta Phi: Phase change (radians)
                      -> Distance -- ^ Displacement d (m)
calculateDisplacement f_min delta_phi = ((delta_phi |* c) :: Velocity) |/| (((4 * pi) |* f_min) :: Frequency) :: Distance
  where
    c = Velocity 3.0e8

-- | Requirement FR-DSP-001: Static Clutter Removal
-- Implements an Exponential Moving Average (EMA) high-pass filter.
-- Dynamically adjusts alpha based on real-time signal variance (motion metric).
--
-- Complexity: O(N) runtime where N is the number of bins.
-- Safety: Total function, handles all inputs gracefully.

-- | Parameters for Adaptive MTI Filter
data MTIConfig = MTIConfig
    { mtiAlphaBase :: !Double  -- ^ Base learning rate (standard motion)
    , mtiAlphaMax  :: !Double  -- ^ Max learning rate (static environment)
    , mtiThreshold :: !Double  -- ^ Motion variance threshold
    } deriving (Show, Eq)

-- | Smart constructor for MTIConfig with validation.
-- Enforces: 0 <= mtiAlphaBase <= mtiAlphaMax <= 1 and isFinite mtiThreshold
mkMTIConfig :: Double -> Double -> Double -> Either String MTIConfig
mkMTIConfig alphaBase alphaMax threshold
    | not (isFinite alphaBase) = Left "mtiAlphaBase must be finite"
    | not (isFinite alphaMax) = Left "mtiAlphaMax must be finite"
    | not (isFinite threshold) = Left "mtiThreshold must be finite"
    | alphaBase < 0.0 = Left "mtiAlphaBase must be >= 0"
    | alphaBase > 1.0 = Left "mtiAlphaBase must be <= 1"
    | alphaMax < 0.0 = Left "mtiAlphaMax must be >= 0"
    | alphaMax > 1.0 = Left "mtiAlphaMax must be <= 1"
    | alphaBase > alphaMax = Left "mtiAlphaBase must be <= mtiAlphaMax"
    | otherwise = Right (MTIConfig alphaBase alphaMax threshold)
  where
    isFinite x = not (isNaN x || isInfinite x)

applyStaticClutterRemoval :: MTIConfig               -- ^ Filter configuration
                          -> [Complex Double]        -- ^ Previous Mean (State)
                          -> [Complex Double]        -- ^ Current Frame Input
                          -> ([Complex Double], [Complex Double]) -- ^ (New Mean, Output Frame)
-- ⚡ Bolt Optimization: Replaced O(N) multi-pass `zipWith` chain with single-pass
-- strict left fold to prevent intermediate thunk allocations and avoid stack growth.
applyStaticClutterRemoval config prevMean input =
    if null prevMean
    then goInit input
    else
        let !motionMetric = calculateMotionMetric prevMean input
            !alpha = if motionMetric < mtiThreshold config
                     then mtiAlphaMax config
                     else mtiAlphaBase config
            !alphaC = alpha :+ 0
            !oneMinusAlphaC = (1.0 - alpha) :+ 0

            !means = scaleAndAddV oneMinusAlphaC prevMean alphaC input
            !outputs = subV input means
        in (means, outputs)
  where
    calculateMotionMetric prevs inputs = normSq (subV inputs prevs)

    goInit inputs =
        let !means = inputs
            !outputs = scaleV (0 :+ 0) inputs
        in (means, outputs)

-- Requirement FR-DSP-004

-- Requirement PR-ACC-01

{-# LANGUAGE StrictData #-}
module SignalProcessing.FMCW
    ( -- * Core Radar Principles
      calculateBeatFreq
    , calculateRangeResolution
      -- * Chirp Z-Transform (CZT)
    , chirpZTransform
    , CZTParams(..)
      -- * Static Clutter Removal
    , applyStaticClutterRemoval
      -- * Phase-Based Motion Tracking
    , calculatePhase
    , unwrapPhase
    , calculateDisplacement
    ) where

import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VS

-- | Equation (1): Verified
-- Calculate the beat frequency from a target range.
-- f_FFT = (2 * B * R) / (c * T)
calculateBeatFreq :: Double -- ^ Bandwidth B (Hz)
                  -> Double -- ^ Chirp Duration T (s)
                  -> Double -- ^ Range R (m)
                  -> Double -- ^ Beat Frequency (Hz)
calculateBeatFreq bw duration targetRange = (2 * bw * targetRange) / (c * duration)
  where
    c = 3.0e8

-- | Range Resolution Limit
-- Delta R = c / (2 * B)
calculateRangeResolution :: Double -- ^ Bandwidth B (Hz)
                         -> Double -- ^ Resolution (m)
calculateRangeResolution bw = c / (2 * bw)
  where
    c = 3.0e8

-- | Parameters for the Chirp Z-Transform
data CZTParams = CZTParams
    { cztStartFreq :: Double -- ^ f_0: Start frequency of the zoom window (Hz)
    , cztBandwidth :: Double -- ^ B_zoom: Bandwidth of the zoom window (Hz)
    , cztSteps     :: Int    -- ^ K: Number of frequency steps in the output
    , cztSampleRate :: Double -- ^ f_s: Sampling rate of the IF signal (Hz)
    } deriving (Show, Eq)

-- | Equation (2): Corrected & Verified
-- X_{k, CZT} = sum_{n=0}^{N-1} x_n * exp(-i * 2 * pi * n * (f_0 + B_zoom * k / K) / f_s)
--
-- Note: This is a direct implementation of the summation. For large N and K,
-- a formulation using convolution (Bluestein's algorithm) would be faster,
-- but the prompt asks for the specific equation implementation.
chirpZTransform :: CZTParams
                -> Vector (Complex Double) -- ^ Input signal x_n
                -> Vector (Complex Double) -- ^ Output spectrum X_k
chirpZTransform params x_n = fromList [ calculateBin k | k <- [0 .. k_max - 1] ]
  where
    n_samples = size x_n
    k_max = cztSteps params
    f0 = cztStartFreq params
    b_zoom = cztBandwidth params
    fs = cztSampleRate params

    -- Helper to calculate the value for a specific frequency bin k
    calculateBin :: Int -> Complex Double
    calculateBin k =
        let
            k_idx = fromIntegral k
            k_total = fromIntegral k_max

            -- f_k = f_0 + B_zoom * (k / K)
            freq_k = f0 + b_zoom * (k_idx / k_total)

            -- Phase term: -i * 2 * pi * n * (freq_k / f_s)
            -- We want exp(phase_term)
            -- Argument for cis is: -2 * pi * n * freq_k / f_s
            theta_scale = (-2 * pi * freq_k) / fs

            -- Create a vector of complex exponentials for each n
            -- exp_vec[n] = cis (theta_scale * n)
            exp_vec = cmap (\n -> cis (theta_scale * n)) (fromList [0 .. fromIntegral (n_samples - 1)])

        in
            -- Dot product of input signal and the complex exponentials
            -- hmatrix <.> is Hermitian (sum(conj(x) * y)), so we must conjugate x_n first
            -- to get sum(x_n * exp_vec).
            conj x_n <.> exp_vec

-- | Equation (4): Verified
-- Extract the phase from the complex value at the peak index.
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
-- Corrects phase jumps greater than pi by adding/subtracting 2*pi.
-- p[n]_unwrapped = p[n] - 2 * pi * round((p[n] - p[n-1]) / (2 * pi))_accumulated
unwrapPhase :: Vector Double -> Vector Double
unwrapPhase inputPhase
    | VS.null inputPhase = inputPhase
    | otherwise = inputPhase - corrections
  where
    -- Calculate differences between consecutive phases: p[i] - p[i-1]
    diffs = VS.zipWith (-) (VS.tail inputPhase) (VS.init inputPhase)

    -- Calculate required jumps (multiples of 2*pi)
    -- If diff is around 2*pi, we want to subtract 2*pi.
    -- If diff is around -2*pi, we want to add 2*pi (subtract -2*pi).
    jumps = VS.map (\d -> (fromIntegral (round (d / (2 * pi)) :: Int) :: Double) * (2 * pi)) diffs

    -- Cumulative correction
    corrections = VS.scanl (+) 0.0 jumps

-- | Equation (5): Verified
-- Calculate displacement from phase change.
-- d = (c * delta_phi) / (4 * pi * f_min)
calculateDisplacement :: Double -- ^ f_min: Start frequency of the chirp (Hz) (e.g. 77e9)
                      -> Double -- ^ Delta Phi: Phase change (radians)
                      -> Double -- ^ Displacement d (m)
calculateDisplacement f_min delta_phi = (c * delta_phi) / (4 * pi * f_min)
  where
    c = 3.0e8

-- | Requirement FR-DSP-001: Static Clutter Removal
-- Implements an Exponential Moving Average (EMA) high-pass filter to remove
-- static background (clutter) from the range profile.
--
-- Mean[k] = (1 - alpha) * Mean[k-1] + alpha * Input[k]
-- Output[k] = Input[k] - Mean[k]
--
-- This ensures that static objects (constant amplitude/phase over time) are
-- subtracted, leaving only moving targets.
applyStaticClutterRemoval :: Double                  -- ^ Alpha (Learning Rate, e.g., 0.05)
                          -> Vector (Complex Double) -- ^ Previous Mean (State)
                          -> Vector (Complex Double) -- ^ Current Frame Input
                          -> (Vector (Complex Double), Vector (Complex Double)) -- ^ (New Mean, Output Frame)
applyStaticClutterRemoval alpha prevMean input = (newMean, output)
  where
    -- Weighted sum: (1-alpha)*prev + alpha*input
    -- Note: scale requires the scalar to match the element type (Complex Double)
    newMean = scale ((1.0 - alpha) :+ 0) prevMean + scale (alpha :+ 0) input
    output = input - newMean

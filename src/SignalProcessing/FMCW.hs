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

import Data.Complex
import Data.List (scanl')

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
chirpZTransform :: CZTParams
                -> [Complex Double] -- ^ Input signal x_n
                -> [Complex Double] -- ^ Output spectrum X_k
chirpZTransform params x_n = [ calculateBin k | k <- [0 .. k_max - 1] ]
  where
    n_samples = length x_n
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
            theta_scale = (-2 * pi * freq_k) / fs

            -- Create a list of complex exponentials for each n
            -- exp_vec[n] = cis (theta_scale * n)
            exp_vec = [ cis (theta_scale * fromIntegral n) | n <- [0 .. n_samples - 1] ]

        in
            -- Sum product: sum (x_n * exp_vec)
            sum $ zipWith (*) x_n exp_vec

-- | Equation (4): Verified
-- Extract the phase from the complex value.
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
-- Corrects phase jumps greater than pi by adding/subtracting 2*pi.
-- p[n]_unwrapped = p[n] - 2 * pi * round((p[n] - p[n-1]) / (2 * pi))_accumulated
unwrapPhase :: [Double] -> [Double]
unwrapPhase [] = []
unwrapPhase phases = zipWith (-) phases corrections
  where
    -- Calculate differences between consecutive phases: p[i] - p[i-1]
    diffs = zipWith (-) (tail phases) (init phases)

    -- Calculate required jumps (multiples of 2*pi)
    -- If diff is around 2*pi, we want to subtract 2*pi.
    -- If diff is around -2*pi, we want to add 2*pi (subtract -2*pi).
    jumps = map (\d -> fromIntegral (round (d / (2 * pi)) :: Int) * (2 * pi)) diffs

    -- Cumulative correction
    -- First element needs 0 correction. subsequent depend on jumps.
    -- scanl' creates a list starting with 0, then 0+j0, 0+j0+j1...
    corrections = scanl' (+) 0.0 jumps

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
applyStaticClutterRemoval :: Double                  -- ^ Alpha (Learning Rate, e.g., 0.05)
                          -> [Complex Double] -- ^ Previous Mean (State)
                          -> [Complex Double] -- ^ Current Frame Input
                          -> ([Complex Double], [Complex Double]) -- ^ (New Mean, Output Frame)
applyStaticClutterRemoval alpha prevMean input = (newMean, output)
  where
    -- Weighted sum: (1-alpha)*prev + alpha*input
    cAlpha = alpha :+ 0
    cOneMinusAlpha = (1.0 - alpha) :+ 0

    newMean = zipWith (\p i -> cOneMinusAlpha * p + cAlpha * i) prevMean input
    output = zipWith (-) input newMean

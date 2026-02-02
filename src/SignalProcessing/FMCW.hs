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

-- | Equation (1): Verified
-- Calculate the beat frequency from a target range.
calculateBeatFreq :: Double -> Double -> Double -> Double
calculateBeatFreq bw duration targetRange = (2 * bw * targetRange) / (c * duration)
  where
    c = 3.0e8

-- | Range Resolution Limit
calculateRangeResolution :: Double -> Double
calculateRangeResolution bw = 3.0e8 / (2 * bw)

-- | Parameters for the Chirp Z-Transform
data CZTParams = CZTParams
    { cztStartFreq :: Double
    , cztBandwidth :: Double
    , cztSteps     :: Int
    , cztSampleRate :: Double
    } deriving (Show, Eq)

-- | Equation (2): Corrected & Verified (Pure Haskell)
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
            freq_k = f0 + b_zoom * (k_idx / k_total)

            -- Phase term: -i * 2 * pi * n * (freq_k / f_s)
            theta_scale = (-2 * pi * freq_k) / fs

            -- exp_vec[n] = cis (theta_scale * n)
            exp_vec = [ cis (theta_scale * fromIntegral n) | n <- [0 .. n_samples - 1] ]

        in
            -- Dot product: sum(conj(x_n) * exp_vec)
            sum (zipWith (*) (map conjugate x_n) exp_vec)

-- | Equation (4): Verified
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
unwrapPhase :: [Double] -> [Double]
unwrapPhase inputPhase
    | null inputPhase = inputPhase
    | otherwise = zipWith (-) inputPhase corrections
  where
    -- Calculate differences: p[i] - p[i-1]
    diffs = zipWith (-) (tail inputPhase) (init inputPhase)

    -- Calculate required jumps
    jumps = map (\d -> fromIntegral (round (d / (2 * pi)) :: Int) * (2 * pi)) diffs

    -- Cumulative correction
    corrections = scanl (+) 0.0 jumps

-- | Equation (5): Verified
calculateDisplacement :: Double -> Double -> Double
calculateDisplacement f_min delta_phi = (c * delta_phi) / (4 * pi * f_min)
  where
    c = 3.0e8

-- | Requirement FR-DSP-001: Static Clutter Removal
applyStaticClutterRemoval :: Double
                          -> [Complex Double]
                          -> [Complex Double]
                          -> ([Complex Double], [Complex Double])
applyStaticClutterRemoval alpha prevMean input = (newMean, output)
  where
    -- Weighted sum: (1-alpha)*prev + alpha*input
    -- Pure lists, zipWith
    newMean = zipWith (\p i -> ((1.0 - alpha) :+ 0) * p + (alpha :+ 0) * i) prevMean input

    -- Output = Input - Mean
    output = zipWith (-) input newMean

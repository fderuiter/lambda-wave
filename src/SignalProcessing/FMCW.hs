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

import Numeric.Simple

-- | Equation (1): Verified
calculateBeatFreq :: Double -> Double -> Double -> Double
calculateBeatFreq bw duration targetRange = (2 * bw * targetRange) / (c * duration)
  where c = 3.0e8

-- | Range Resolution Limit
calculateRangeResolution :: Double -> Double
calculateRangeResolution bw = c / (2 * bw)
  where c = 3.0e8

data CZTParams = CZTParams
    { cztStartFreq :: Double
    , cztBandwidth :: Double
    , cztSteps     :: Int
    , cztSampleRate :: Double
    } deriving (Show, Eq)

-- | Equation (2): Corrected & Verified
chirpZTransform :: CZTParams
                -> Vector (Complex Double)
                -> Vector (Complex Double)
chirpZTransform params x_n = fromList [ calculateBin k | k <- [0 .. k_max - 1] ]
  where
    n_samples = size x_n
    k_max = cztSteps params
    f0 = cztStartFreq params
    b_zoom = cztBandwidth params
    fs = cztSampleRate params

    calculateBin :: Int -> Complex Double
    calculateBin k =
        let
            k_idx = fromIntegral k
            k_total = fromIntegral k_max
            freq_k = f0 + b_zoom * (k_idx / k_total)
            theta_scale = (-2 * pi * freq_k) / fs

            -- Create a vector of complex exponentials for each n
            exp_vec = cmap (\n -> cis (theta_scale * fromIntegral n)) (fromList [0 .. (n_samples - 1)])

        in
            -- Dot product: sum(conj(x) * y)
            conj x_n <.> exp_vec

-- | Equation (4): Verified
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
unwrapPhase :: Vector Double -> Vector Double
unwrapPhase (Vector inputPhase)
    | null inputPhase = Vector []
    | otherwise = Vector (zipWith (-) inputPhase corrections)
  where
    -- Calculate differences: p[i] - p[i-1]
    -- zipWith (-) (tail) (init)
    diffs = zipWith (-) (drop 1 inputPhase) inputPhase

    -- Calculate required jumps
    jumps = map (\d -> fromIntegral (round (d / (2 * pi)) :: Int) * (2 * pi)) diffs

    -- Cumulative correction
    corrections = scanl (+) 0.0 jumps

-- | Equation (5): Verified
calculateDisplacement :: Double -> Double -> Double
calculateDisplacement f_min delta_phi = (c * delta_phi) / (4 * pi * f_min)
  where c = 3.0e8

-- | Requirement FR-DSP-001: Static Clutter Removal
applyStaticClutterRemoval :: Double
                          -> Vector (Complex Double)
                          -> Vector (Complex Double)
                          -> (Vector (Complex Double), Vector (Complex Double))
applyStaticClutterRemoval alpha prevMean input = (newMean, output)
  where
    -- Weighted sum: (1-alpha)*prev + alpha*input
    -- cmap to scale. Need to treat (1-alpha) as Complex for multiplication.
    term1 = cmap (\x -> ((1.0 - alpha) :+ 0) * x) prevMean
    term2 = cmap (\x -> (alpha :+ 0) * x) input

    newMean = term1 + term2
    output = input - newMean

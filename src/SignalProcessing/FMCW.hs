{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
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

-- | Chirp Z-Transform (Stubbed/Simplified)
-- | Chirp Z-Transform (Direct Summation)
chirpZTransform :: CZTParams
                -> [Complex Double] -- ^ Input signal x_n
                -> [Complex Double] -- ^ Output spectrum X_k
chirpZTransform params x_n = map calculateBin [0 .. cztSteps params - 1]
  where
    f0 = cztStartFreq params
    b_zoom = cztBandwidth params
    fs = cztSampleRate params
    k_max = fromIntegral (cztSteps params)

    calculateBin :: Int -> Complex Double
    calculateBin k =
        let
            k_idx = fromIntegral k :: Double
            -- f_k = f_0 + B_zoom * (k / K)
            freq_k = f0 + b_zoom * (k_idx / k_max)

            -- Phase term per sample: -i * 2 * pi * (freq_k / f_s)
            theta_step = ((-2) * pi * freq_k) / fs

            -- Summation: sum(x[n] * exp(i * theta_step * n))
            -- ⚡ Bolt Optimization: Added bang patterns to prevent O(N) thunk buildup
            summation !acc _ [] = acc
            summation !acc !n (val:rest) =
                let !phaseTerm = theta_step * fromIntegral n
                    !term = cis phaseTerm
                in summation (acc + val * term) (n + 1) rest
        in
            summation (0 :+ 0) (0 :: Int) x_n

-- | Equation (4): Verified
-- Extract the phase from the complex value at the peak index.
calculatePhase :: Complex Double -> Double
calculatePhase = phase

-- | Requirement FR-DSP-002: Phase Unwrapping
-- Corrects phase jumps greater than pi by adding/subtracting 2*pi.
-- Implemented using standard list operations.
--
-- ⚡ Bolt Optimization: Replaced O(N) multi-pass operations (drop, zipWith, map, scanl)
-- with a single-pass tail-recursive algorithm. Avoids intermediate allocations and
-- improves performance in hot signal processing paths.
unwrapPhase :: [Double] -> [Double]
unwrapPhase [] = []
unwrapPhase (x:xs) = x : go x 0.0 xs
  where
    go _ _ [] = []
    go prevPhase currentCorrection (p:ps) =
        let diff = p - prevPhase
            jump = fromIntegral (round (diff / (2 * pi)) :: Int) * (2 * pi)
            newCorrection = currentCorrection + jump
        in (p - newCorrection) : go p newCorrection ps

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
-- Implements an Exponential Moving Average (EMA) high-pass filter.
applyStaticClutterRemoval :: Double                  -- ^ Alpha (Learning Rate, e.g., 0.05)
                          -> [Complex Double]        -- ^ Previous Mean (State)
                          -> [Complex Double]        -- ^ Current Frame Input
                          -> ([Complex Double], [Complex Double]) -- ^ (New Mean, Output Frame)
-- ⚡ Bolt Optimization: Replaced O(N) multi-pass `zipWith` chain with single-pass
-- guarded recursion to prevent intermediate thunk allocations and improve stream fusion.
applyStaticClutterRemoval alpha prevMean input =
    if null prevMean
    then goInit input
    else go prevMean input
  where
    !alphaC = alpha :+ 0
    !oneMinusAlphaC = (1.0 - alpha) :+ 0

    goInit [] = ([], [])
    goInit (i:is) =
        let !(m, o) = (i, 0 :+ 0)
            (ms, os) = goInit is
        in (m : ms, o : os)

    go [] _ = ([], [])
    go _ [] = ([], [])
    go (p:ps) (i:is) =
        let !m = oneMinusAlphaC * p + alphaC * i
            !o = i - m
            (ms, os) = go ps is
        in (m : ms, o : os)

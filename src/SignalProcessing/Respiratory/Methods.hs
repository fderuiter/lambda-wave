{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}
module SignalProcessing.Respiratory.Methods
  ( -- * 2.0 Core Methodologies in Signal Processing
    weightedAverageHeight
  , snrImprovement
  , LockInResult(..)
  , lockInDetection
  , timeDelayAnalysis
    -- * Helpers
  , radix2FFT
  ) where

import Data.Complex
import Data.Types (Point3D(..))
import Numeric.LinearAlgebra hiding (find, i, magnitude)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Complex as Complex

-- | 2.1 The Weighted Average Height (WAH) for Surrogate Signal Generation
-- Transforms high-density 3D surface data into a single 1D respiratory signal.
--
-- Formula: V_resp(t) = (1/N_mesh) * Sum(Z_i(t))
weightedAverageHeight :: [Point3D] -- ^ List of vertices in the ROI
                      -> Double    -- ^ Returns V_resp: Average height (mm usually)
weightedAverageHeight [] = 0
weightedAverageHeight pts = sum (map pz pts) / fromIntegral (length pts)

-- | 2.2 Statistical Noise Suppression via Spatial Averaging
-- Returns the theoretical SNR improvement factor based on the number of measurement points.
--
-- Formula: Improvement proportional to sqrt(N)
snrImprovement :: Int -- ^ N: Number of measurement points (vertices)
               -> Double -- ^ Returns SNR improvement factor
snrImprovement n = sqrt (fromIntegral n)

-- | Result of Lock-In Detection
data LockInResult = LockInResult
  { magnitude :: Double
  , phase     :: Double
  , output    :: Double -- ^ Scaled output signal
  } deriving (Show, Eq)

-- | 2.3 Lock-In Detection for Noise Rejection
-- Extracts signal amplitude and phase using dual-phase lock-in amplification logic.
--
-- Workflow:
-- 1. Mixing: Multiply signal by In-Phase and Quadrature references.
-- 2. LPF/Averaging: Extract DC components (V_x, V_y).
-- 3. Calculate Magnitude (R) and Phase (theta).
-- 4. Scale output based on sensitivity.
lockInDetection :: Vector Double -- ^ V_sig: Input signal vector
                -> Vector Double -- ^ Ref_I: In-Phase reference vector (cos)
                -> Vector Double -- ^ Ref_Q: Quadrature reference vector (sin)
                -> Double        -- ^ S: Sensitivity of the amplifier
                -> Double        -- ^ V_fs: Full-scale voltage
                -> LockInResult
lockInDetection vSig refI refQ sens vFs =
  let
    n = size vSig
    -- DC component extraction via averaging (equivalent to LPF)
    -- V_x = Mean(V_sig * Ref_I) corresponds to (Vs * Vr / 2) * cos(delta_phi)
    vx = (vSig <.> refI) / fromIntegral n
    vy = (vSig <.> refQ) / fromIntegral n

    r = sqrt (vx**2 + vy**2)
    theta = atan2 vy vx

    -- Scaled Output: R_scaled = (V_fs * V_r / (2 * S)) * V_s
    -- Since our calculated 'r' is effectively (Vs * Vr / 2),
    -- R_scaled = (V_fs / S) * r
    rScaled = (vFs / sens) * r
  in LockInResult r theta rScaled


-- | 2.4 Time-Delay Analysis using Fourier Transforms
-- Calculates the optimal time delay (latency) between two signals using Cross-Correlation via FFT.
--
-- Workflow:
-- 1. Pad signals to next power of 2 to ensure Radix-2 FFT compatibility.
-- 2. FFT of both signals.
-- 3. Multiply Y(f) by Conj(X(f)) to get Cross-Correlation in freq domain.
-- 4. IFFT to get time-domain Cross-Correlation.
-- 5. Find peak index.
timeDelayAnalysis :: Vector Double -- ^ Signal A (Measured)
                  -> Vector Double -- ^ Signal B (Reference)
                  -> Int           -- ^ Returns delay in samples
timeDelayAnalysis sigA sigB =
  let
    nOrig = max (size sigA) (size sigB)
    nPad = nextPowerOf2 nOrig

    -- Pad signals with zeros to nPad
    -- vjoin [vector, constant 0 (padding)]
    -- Explicit type signature to avoid ambiguous type variable error or needing FlexibleContexts globally (though I added it)
    pad :: Vector Double -> Int -> Vector Double
    pad v n = vjoin [v, konst 0 (n - size v)]

    paddedA = pad sigA nPad
    paddedB = pad sigB nPad

    -- Convert to complex. cmap is used for element-wise mapping.
    cA = cmap (\x -> x :+ 0) paddedA
    cB = cmap (\x -> x :+ 0) paddedB

    fftA = radix2FFT cA
    fftB = radix2FFT cB

    -- Cross-correlation: FFT(A) * Conj(FFT(B))
    -- Vector (Complex Double) is an instance of Num, so (*) is element-wise.
    crossSpec = fftA * cmap conjugate fftB

    ccTime = radix2IFFT crossSpec

    -- Find index of maximum magnitude
    mags = cmap Complex.magnitude ccTime :: Vector Double
    maxIdx = maxIndex mags

    -- Handle wrapping for negative delays
    -- If maxIdx > N/2, it corresponds to a negative lag (-(N - maxIdx))
    delay = if maxIdx > (nPad `div` 2)
            then maxIdx - nPad
            else maxIdx
  in delay

-- | Internal: Calculate next power of 2
nextPowerOf2 :: Int -> Int
nextPowerOf2 n =
  let go k = if k >= n then k else go (k * 2)
  in go 1

-- | Internal: Simple Radix-2 Coole-Tukey FFT
-- Assumes input size is a power of 2.
radix2FFT :: Vector (Complex Double) -> Vector (Complex Double)
radix2FFT input
  | n <= 1 = input
  | otherwise =
      let
        half = n `div` 2
        -- Split into even and odd indices
        -- hmatrix vectors are 0-indexed.
        evens = LA.fromList [ input ! (2*i) | i <- [0..half-1] ]
        odds  = LA.fromList [ input ! (2*i+1) | i <- [0..half-1] ]

        fftEvens = radix2FFT evens
        fftOdds  = radix2FFT odds

        twiddles = LA.fromList [ cis (-2 * pi * fromIntegral k / fromIntegral n) | k <- [0..half-1] ]

        t = twiddles * fftOdds

        firstHalf  = fftEvens + t
        secondHalf = fftEvens - t
      in vjoin [firstHalf, secondHalf]
  where
    n = size input

-- | Internal: Inverse FFT
radix2IFFT :: Vector (Complex Double) -> Vector (Complex Double)
radix2IFFT input =
  let
    n = fromIntegral (size input)
    -- Conjugate input
    conjInput = cmap conjugate input
    -- Forward FFT
    f = radix2FFT conjInput
    -- Conjugate again and scale by 1/N
    conjOutput = cmap conjugate f
  in scale (recip (n :+ 0)) conjOutput

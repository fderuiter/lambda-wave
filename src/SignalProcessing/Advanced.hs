{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
module SignalProcessing.Advanced
    ( -- * The "Fonzi" Formula Stack
      -- ** Step 1: The "Zoom" (Bluestein CZT)
      chirpZTransformBluestein
    , CZTParams(..)
      -- ** Step 2: The "Motion" (Phase Extraction)
    , extractPhase
      -- ** Step 3: The "Unwrapping" (Differential Phase Unwrapping)
    , unwrapPhase
    , calculateDisplacementAdvanced
    , calculateDisplacementAccumulated
      -- ** Step 4: The "Targeting" (MIMO Beamforming)
    , beamform
    , AntennaConfig(..)
    ) where

import Numeric.LinearAlgebra
import Data.Complex
import Control.Monad.ST
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- | Parameters for the Chirp Z-Transform
data CZTParams = CZTParams
    { cztA :: Complex Double -- ^ A: Starting point ($A_0 e^{j 2\pi \theta_0}$)
    , cztW :: Complex Double -- ^ W: Step size ($W_0 e^{j 2\pi \phi_0}$)
    , cztN :: Int            -- ^ N: Number of input samples
    , cztK :: Int            -- ^ K: Number of output frequency bins
    } deriving (Show, Eq)

-- | Step 1: The "Zoom" (Bluestein's Algorithm for CZT)
--
-- Implements the formula:
-- \[ X_k = \sum_{n=0}^{N-1} x(n) A^{-n} W^{nk} \]
--
-- Using the efficient convolution method (O((N+K) log(N+K))):
-- \[ X_k = W^{k^2/2} \left( (x(n) A^{-n} W^{n^2/2}) * W^{-n^2/2} \right)_k \]
--
-- Note: Uses internal Cooley-Tukey FFT implementation since hmatrix core lacks it.
chirpZTransformBluestein :: CZTParams -> Vector (Complex Double) -> Vector (Complex Double)
chirpZTransformBluestein params x
    | size x /= n = error $ "Input size " ++ show (size x) ++ " does not match cztN " ++ show n
    | otherwise = result
  where
    n = cztN params
    k_bins = cztK params
    a = cztA params
    w = cztW params

    -- Choose convolution length L >= N + K - 1. Power of 2 is best for FFT.
    m_len = n + k_bins - 1
    l_len = nextPowerOf2 m_len

    -- Helper: W^(x/2)
    -- We compute W^(i^2/2). Since i is integer, i^2 is integer.
    w_pow :: Int -> Complex Double
    w_pow i = w ** (fromIntegral (i * i) / 2.0)

    -- Sequence y_n = x(n) * A^(-n) * W^(n^2/2)
    -- for n = 0 to N-1
    y_vec :: Vector (Complex Double)
    y_vec = fromList [ (x ! n_idx) * (a ** (fromIntegral (-n_idx))) * w_pow n_idx | n_idx <- [0 .. n - 1] ]

    -- Sequence h_n = W^(-n^2/2)
    -- We need h for indices in convolution corresponding to k - n.
    -- The circular buffer range for indices [-(N-1) ... (K-1)].
    -- In a length L buffer:
    -- Index i (0 to K-1) maps to i.
    -- Index i (-(N-1) to -1) maps to L+i.
    h_buf :: Vector (Complex Double)
    h_buf = runSTVector $ do
        vec <- newVector (0 :+ 0) l_len
        -- Fill for 0 to K-1
        mapM_ (\i -> writeVector vec i (w ** (fromIntegral (-(i * i)) / 2.0))) [0 .. k_bins - 1]
        -- Fill for -(N-1) to -1
        mapM_ (\i -> writeVector vec (l_len - i) (w ** (fromIntegral (-(i * i)) / 2.0))) [1 .. n - 1]
        return vec

    -- Pad y to length L
    y_padded = vjoin [y_vec, konst (0 :+ 0) (l_len - n)]

    -- Perform FFTs using internal implementation
    fft_y = customFFT y_padded
    fft_h = customFFT h_buf

    -- Convolution in frequency domain
    g_freq = fft_y * fft_h
    g_time = customIFFT g_freq

    -- Extract relevant part of g and multiply by W^(k^2/2)
    -- g[k] corresponds to the convolution sum for k.
    result = fromList [ (g_time ! k_idx) * w_pow k_idx | k_idx <- [0 .. k_bins - 1] ]

    nextPowerOf2 :: Int -> Int
    nextPowerOf2 val = go 1
      where go acc = if acc >= val then acc else go (acc * 2)

    -- Helper to create vector in ST
    runSTVector :: (forall s. ST s (VM.STVector s (Complex Double))) -> Vector (Complex Double)
    runSTVector action = V.create action

    newVector :: Complex Double -> Int -> ST s (VM.STVector s (Complex Double))
    newVector val len = VM.replicate len val

    writeVector :: VM.STVector s (Complex Double) -> Int -> Complex Double -> ST s ()
    writeVector = VM.write

-- | Custom Radix-2 Cooley-Tukey FFT
-- Expects input size to be a power of 2.
customFFT :: Vector (Complex Double) -> Vector (Complex Double)
customFFT v
    | n <= 1 = v
    | otherwise = result
  where
    n = size v
    halfN = n `div` 2

    -- Extract even and odd indexed elements
    -- Note: hmatrix slicing might be inefficient if used recursively excessively,
    -- but for moderate N it's okay. Using lists for recursion might be cleaner but Vector is fine.
    -- Constructing via list for simplicity and correctness over raw slicing optimization here.
    l = toList v
    evenL = stride l
    oddL = stride (drop 1 l)

    stride [] = []
    stride [x] = [x]
    stride (x:_:xs) = x : stride xs

    evenVec = fromList evenL
    oddVec = fromList oddL

    fftEven = customFFT evenVec
    fftOdd = customFFT oddVec

    -- Combine
    -- k from 0 to n/2 - 1
    -- T = exp(-2pi i k / N) * Odd_k
    -- Res_k = Even_k + T
    -- Res_{k+n/2} = Even_k - T

    combine k =
        let t = cis (-2 * pi * fromIntegral k / fromIntegral n) * (fftOdd ! k)
            e = fftEven ! k
        in (e + t, e - t)

    pairs = map combine [0 .. halfN - 1]
    (firstHalf, secondHalf) = unzip pairs

    result = fromList (firstHalf ++ secondHalf)

-- | Custom IFFT
-- IFFT(x) = (1/N) * conj(FFT(conj(x)))
customIFFT :: Vector (Complex Double) -> Vector (Complex Double)
customIFFT v = scale (1.0 / fromIntegral n) (cmap conjugate (customFFT (cmap conjugate v)))
  where n = size v

-- | Step 2: The "Motion" (Phase Extraction)
--
-- Extracts phase \(\phi = \arctan(Q/I)\).
-- Returns value in range \((-\pi, \pi]\).
extractPhase :: Complex Double -> Double
extractPhase = phase

-- | Step 3: The "Unwrapping" (Differential Phase Unwrapping)
--
-- Computes \(\Delta \phi = \phi_n - \phi_{n-1}\) handling wrap-around.
--
-- Logic:
-- If \(\Delta \phi > \pi\), subtract \(2\pi\).
-- If \(\Delta \phi < -\pi\), add \(2\pi\).
unwrapPhase :: Double -- ^ Current Phase
            -> Double -- ^ Previous Phase
            -> Double -- ^ Delta Phi (Unwrapped)
unwrapPhase phi_curr phi_prev =
    let diff = phi_curr - phi_prev
    in if diff > pi
       then diff - 2 * pi
       else if diff < -pi
            then diff + 2 * pi
            else diff

-- | Calculate displacement from unwrapped delta phi.
--
-- \(D[n] = D[n-1] + \frac{\lambda}{4\pi} \Delta \phi[n]\)
--
-- This function returns the *change* in displacement for the current step.
-- The caller must accumulate it.
calculateDisplacementAdvanced :: Double -- ^ Wavelength (lambda) in mm
                              -> Double -- ^ Unwrapped Delta Phi
                              -> Double -- ^ Delta Displacement (mm)
calculateDisplacementAdvanced lambda delta_phi =
    (lambda / (4 * pi)) * delta_phi

-- | Helper to process a list of phases and return absolute displacements.
calculateDisplacementAccumulated :: Double -> [Double] -> [Double]
calculateDisplacementAccumulated lambda phases =
    scanl (\acc (curr, prev) -> acc + calculateDisplacementAdvanced lambda (unwrapPhase curr prev)) 0 (zip phases (0 : phases))
    -- Note: This assumes initial phase is 0 for the "prev" of the first element.

-- | Configuration for Antenna Array
data AntennaConfig = AntennaConfig
    { antennaCount :: Int
    , antennaSpacing :: Double -- ^ Distance d between antennas (mm or same unit as lambda)
    , signalWavelength :: Double -- ^ Lambda
    } deriving (Show, Eq)

-- | Step 4: The "Targeting" (MIMO Beamforming)
--
-- Formula:
-- \[ y(t) = \sum_{k=1}^{M} w_k \cdot x_k(t) \cdot e^{-j \frac{2\pi}{\lambda} d_k \sin(\theta)} \]
--
-- Inputs:
-- * `theta`: Steering angle (radians, 0 is broadside).
-- * `weights`: Optional tapering weights \(w_k\). If Nothing, assumes 1.0.
-- * `signals`: Matrix of signals. Rows = Antennas, Cols = Time samples.
--
-- Returns:
-- * Vector of beamformed time-domain signal.
beamform :: AntennaConfig
         -> Double -- ^ Theta (radians)
         -> Maybe (Vector Double) -- ^ Optional weights w_k
         -> Matrix (Complex Double) -- ^ Signals x_k(t) (Rows: Antennas, Cols: Time)
         -> Vector (Complex Double)
beamform config theta mbWeights signals
    | rows signals /= m = error "Signal rows must match antenna count"
    | maybe False (\w -> size w /= m) mbWeights = error "Weights size must match antenna count"
    | otherwise = result
  where
    m = antennaCount config
    d = antennaSpacing config
    lam = signalWavelength config

    -- Steering vector elements
    -- v_k = exp(-j * 2*pi/lambda * k*d * sin(theta))
    -- Note: Prompt says "d_k distance between antennas". Assuming uniform linear array.
    -- Distance of k-th antenna from reference (0-th) is k*d.
    steeringVec :: Vector (Complex Double)
    steeringVec = fromList
        [ cis ((-2 * pi / lam) * (fromIntegral k * d) * sin theta)
        | k <- [0 .. m - 1]
        ]

    realWeights :: Vector (Complex Double)
    realWeights = case mbWeights of
        Just w -> fromList [ (w ! k :+ 0) * (steeringVec ! k) | k <- [0 .. m - 1] ]
        Nothing -> steeringVec

    -- Result y(t) = sum_k ( Weight_k * Signal_k(t) )
    -- equivalent to realWeights <# signals (Vector-Matrix multiplication)
    result = realWeights <# signals

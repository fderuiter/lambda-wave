{-# LANGUAGE StrictData #-}
module SignalProcessing.Interference
    ( reconstructInterference
    , l1RegularizedSmoothing
    , InterferenceParams(..)
    ) where

import Numeric.LinearAlgebra
import Data.Complex
import Foreign.Storable (Storable)

data InterferenceParams = InterferenceParams
    { ipSampleRate :: Double
    , ipNeighbors  :: Int
    , ipLambda     :: Double -- ^ Regularization parameter for L1
    } deriving (Show, Eq)

-- | Time-Domain Interference Reconstruction (TIR)
reconstructInterference :: InterferenceParams
                        -> Vector (Complex Double) -- ^ Input frequency domain signal (FFT result)
                        -> Int                     -- ^ Index of interference peak (k_i)
                        -> Vector (Complex Double) -- ^ Synthesized interference signal in Time Domain
reconstructInterference params spectrum k_i = synthesized
  where
    n = size spectrum
    fs = ipSampleRate params

    valC = spectrum ! k_i
    valL = if k_i > 0 then spectrum ! (k_i - 1) else 0
    valR = if k_i < n - 1 then spectrum ! (k_i + 1) else 0

    magC = magnitude valC
    magL = magnitude valL
    magR = magnitude valR

    -- Using Quinn's First Estimator for Gaussian windows approximation:
    alpha1 = magR / magC
    alpha2 = magL / magC
    d1 = alpha1 / (1 + alpha1)
    d2 = alpha2 / (1 + alpha2)
    delta_k = if magR > magL then d1 else -d2

    -- Frequency estimate
    f_i = (fromIntegral k_i + delta_k) * fs / fromIntegral n

    -- Amplitude and Phase recovery
    a_i = magC
    phi_i = phase valC

    timeIndices = fromList [0 .. fromIntegral (n - 1)] :: Vector Double
    synthesized = cmap (\t -> (a_i :+ 0) * cis (2 * pi * f_i * t / fs + phi_i)) timeIndices


-- | L1 Norm-Regularized Least Squares (ADMM)
l1RegularizedSmoothing :: InterferenceParams
                       -> Vector (Complex Double) -- ^ Corrupted Time Signal (y) with zeros/interp at interference
                       -> Vector Double           -- ^ Mask (1.0 for valid data, 0.0 for missing/interference)
                       -> Vector (Complex Double) -- ^ Recovered Time Signal
l1RegularizedSmoothing params y maskVec = recovered
  where
    n = size y
    lambda = ipLambda params

    -- Pre-calculate DFT matrices once to avoid O(N^2) inside loop
    -- In a real production system with hmatrix-gsl, we would use native FFT.
    -- Here we construct the matrix once.
    dftMat = fromLists [ [ cis (-2 * pi * fromIntegral (r * c) / fromIntegral n) | c <- [0..n-1] ] | r <- [0..n-1] ]
    idftMat = fromLists [ [ cis (2 * pi * fromIntegral (r * c) / fromIntegral n) | c <- [0..n-1] ] | r <- [0..n-1] ]
    scaleFactor = 1.0 / fromIntegral n

    -- Helper FFT functions using pre-calculated matrices
    fft' v = dftMat #> v
    ifft' v = scale scaleFactor (idftMat #> v)

    -- Iteration count
    maxIter = 50

    loop :: Int -> Vector (Complex Double) -> Vector (Complex Double)
    loop 0 currY = currY
    loop k currY =
        let
            -- 1. FFT
            spec = fft' currY

            -- 2. Soft Threshold
            -- S_lambda(v) = v * max(0, 1 - lambda/|v|)
            specThresh = cmap (\v ->
                let mag = magnitude v
                    thresh = max 0 (1 - lambda / mag)
                in v * (thresh :+ 0)) spec

            -- 3. IFFT
            timeEst = ifft' specThresh

            -- 4. Enforce consistency with observed data
            -- y_next[i] = if mask[i]==1 then y[i] else timeEst[i]
            yNext = zipVectorWith3 (\m obs est -> if m > 0.5 then obs else est) maskVec y timeEst
        in loop (k - 1) yNext

    recovered = loop maxIter y

zipVectorWith3 :: (Storable a, Storable b, Storable c, Storable d)
               => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipVectorWith3 f va vb vc = fromList $ zipWith3 f (toList va) (toList vb) (toList vc)

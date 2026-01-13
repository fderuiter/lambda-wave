{-# LANGUAGE StrictData #-}
module SignalProcessing.OpticalFlow
    ( calculateVelocityField
    , iterativeDemonsUpdate
    , interpolateVolume
    , pcaMotionModel
    , OpticalFlowParams(..)
    ) where

import Numeric.LinearAlgebra

data OpticalFlowParams = OpticalFlowParams
    { ofAlpha :: Double -- ^ Regularization factor (homogenization)
    , ofIterations :: Int
    } deriving (Show, Eq)

-- | Calculate Velocity Field (v)
-- v = - (Grad I . dI/dV) / ||Grad I||^2
calculateVelocityField :: Matrix R -- ^ Image I(x)
                       -> Matrix R -- ^ dI/dV (Temporal Gradient approx)
                       -> (Matrix R, Matrix R) -- ^ (Vx, Vy)
calculateVelocityField img dIdV = (vx, vy)
  where
    _ = size img -- suppress unused warning

    gx = calculateGradientX img
    gy = calculateGradientY img

    normSq = (gx * gx) + (gy * gy)

    -- Avoid division by zero by adding epsilon
    epsilon = 1e-6
    denominator = cmap (\x -> if x < epsilon then epsilon else x) normSq

    -- Scale factor: -dI/dV / denominator
    -- Elementwise division
    scale = elementwiseDiv (scaleMat (-1) dIdV) denominator

    vx = elementwiseMul gx scale
    vy = elementwiseMul gy scale

calculateGradientX :: Matrix R -> Matrix R
calculateGradientX m = fromLists diffs
  where
    ls = toLists m
    diffs = map diffRow ls
    diffRow row = zipWith (\next prev -> (next - prev) / 2.0) (tail (tail row) ++ [last row]) (head row : init (init row))

calculateGradientY :: Matrix R -> Matrix R
calculateGradientY m = tr (calculateGradientX (tr m))

-- | Iterative Update Scheme (Demons Algorithm)
iterativeDemonsUpdate :: OpticalFlowParams
                      -> Matrix R -- ^ Static Image (Reference)
                      -> Matrix R -- ^ Moving Image (Target Phase)
                      -> (Matrix R, Matrix R) -- ^ Current Flow (Ux, Uy)
                      -> (Matrix R, Matrix R) -- ^ Updated Flow
iterativeDemonsUpdate params static moving (ux, uy) = (ux + dux, uy + duy)
  where
    alpha = ofAlpha params

    -- Warp moving image by current flow
    warped = warpImage moving ux uy

    -- Difference intensity
    diff = warped - static

    -- Gradients of static image
    gx = calculateGradientX static
    gy = calculateGradientY static

    normSq = (gx * gx) + (gy * gy)
    diffSq = elementwiseMul diff diff
    alphaSq = alpha * alpha

    denominator = normSq + (scaleMat alphaSq diffSq)

    denomSafe = cmap (\x -> if x < 1e-6 then 1e-6 else x) denominator

    term = elementwiseDiv diff denomSafe

    dux = elementwiseMul (scaleMat (-1) gx) term
    duy = elementwiseMul (scaleMat (-1) gy) term

scaleMat :: Double -> Matrix R -> Matrix R
scaleMat s m = cmap (*s) m

elementwiseMul :: Matrix R -> Matrix R -> Matrix R
elementwiseMul a b = reshape (cols a) $ (flatten a) * (flatten b)

elementwiseDiv :: Matrix R -> Matrix R -> Matrix R
elementwiseDiv a b = reshape (cols a) $ (flatten a) / (flatten b)


-- | Warp image using flow (Bilinear Interpolation)
warpImage :: Matrix R -> Matrix R -> Matrix R -> Matrix R
warpImage img ux uy = fromLists [ [ sample r c | c <- [0..cs-1] ] | r <- [0..rs-1] ]
  where
    (rs, cs) = size img

    sample r c = interpolate (fromIntegral r + at r c uy) (fromIntegral c + at r c ux)

    at r c m = m `atIndex` (r, c)

    interpolate r' c' =
        let r0 = floor r'
            c0 = floor c'
            r1 = r0 + 1
            c1 = c0 + 1
            dr = r' - fromIntegral r0
            dc = c' - fromIntegral c0

            val r_ c_
                | r_ < 0 || r_ >= rs || c_ < 0 || c_ >= cs = 0
                | otherwise = img `atIndex` (r_, c_)

            v00 = val r0 c0
            v01 = val r0 c1
            v10 = val r1 c0
            v11 = val r1 c1

        in (1-dr)*(1-dc)*v00 + (1-dr)*dc*v01 + dr*(1-dc)*v10 + dr*dc*v11

-- | Interpolate Volume / Image using Velocity Field
-- Generates I_interpolated = I_ref warped by (v * delta_volume)
interpolateVolume :: OpticalFlowParams
                  -> Matrix R -- ^ Reference Image I
                  -> (Matrix R, Matrix R) -- ^ Velocity Field (Vx, Vy)
                  -> Double   -- ^ Delta Tidal Volume (scalar scale for motion)
                  -> Matrix R -- ^ Interpolated Image
interpolateVolume _ img (vx, vy) deltaV = warpImage img ux uy
  where
    -- Motion u = v * deltaV
    ux = scaleMat deltaV vx
    uy = scaleMat deltaV vy

-- | PCA-based Motion Reconstruction (Placeholder)
-- Decomposes motion vectors into principal motion bases.
-- Returns (Mean Motion Field, Principal Components)
pcaMotionModel :: [(Matrix R, Matrix R)] -> Int -> ((Matrix R, Matrix R), [Vector R])
pcaMotionModel flows k = (meanFlow, components)
  where
    n = length flows
    (rows, cols) = size (fst (head flows))

    -- Flatten flows into vectors: [vx1...vxN, vy1...vyN]
    flattenFlow (vx, vy) = vjoin [flatten vx, flatten vy]

    allFlows = map flattenFlow flows

    -- Calculate Mean
    sumFlow = foldl1 (+) allFlows
    meanVec = scale (1.0 / fromIntegral n) sumFlow

    -- Recover mean flow matrices (unflatten)
    splitIdx = rows * cols
    meanVx = reshape cols (subVector 0 splitIdx meanVec)
    meanVy = reshape cols (subVector splitIdx splitIdx meanVec)
    meanFlow = (meanVx, meanVy)

    -- Centered data
    centered = map (\f -> f - meanVec) allFlows
    dataMat = fromRows centered -- Matrix where each row is an observation

    -- SVD: Data = U S V'
    -- Principal components are rows of V' (or columns of V)
    -- hmatrix svd returns (u, s, v) where v is V (columns are eigenvectors of A'A)
    -- We want eigenvectors of Covariance matrix ~ Data' * Data
    -- If we use compactSVD of Data, right singular vectors are the principal components.

    (_, _, v) = compactSVD dataMat

    -- Extract top k components (columns of v)
    components = toColumns (takeColumns k v)

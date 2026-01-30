module Control.Mesher (fitPolynomialSurface) where

import Data.Types
import SignalProcessing.SimpleLA (leastSquares)

-- | Fits a polynomial surface to the points
-- z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | length pts < 6 = replicate 6 0.0 -- Not enough points
    | otherwise = leastSquares designMatrix zs
  where
    -- Extract coordinates
    zs = map pz pts

    -- Build Matrix A (Design Matrix)
    -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
    -- We construct it row by row, then pass to leastSquares which expects Matrix (list of rows)
    designMatrix = map (\p ->
        [ 1.0
        , px p
        , py p
        , (px p)^(2::Int)
        , (px p)*(py p)
        , (py p)^(2::Int)
        ]) pts

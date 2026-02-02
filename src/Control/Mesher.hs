module Control.Mesher (fitPolynomialSurface) where

import Data.Types
import Numeric.Simple (solveLS)
import Data.Maybe (fromMaybe)

-- | Fits a polynomial surface to the points
-- z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | length pts < 6 = replicate 6 0.0 -- Not enough points
    | otherwise = fromMaybe (replicate 6 0.0) $ do
        -- Construct Matrix A
        -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
        let rows = map (\p -> [1, px p, py p, (px p)^(2::Int), (px p)*(py p), (py p)^(2::Int)]) pts
        let b = map pz pts

        -- Solve least squares: x = (A^T A)^-1 A^T b
        solveLS rows b

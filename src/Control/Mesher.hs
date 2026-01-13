module Control.Mesher (fitPolynomialSurface) where

import Data.Types
import Numeric.LinearAlgebra
import qualified SignalProcessing.Fitting as SPF

-- | Fits a polynomial surface to the points
-- z = c0 + c1*x + c2*y + c3*x^2 + c4*y^2 + c5*xy
-- Uses the implementation in SignalProcessing.Fitting.
--
-- NOTE: The coefficient order is [c0, c1(x), c2(y), c3(x^2), c4(y^2), c5(xy)].
-- This is different from previous legacy implementation which might have swapped xy and y^2.
-- Ensure consumers are aware of this standard order.
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts = toList (SPF.fitSurfaceBiQuadratic pts)

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|
Module      : Numeric.Simple
Description : Pure Haskell Linear Algebra (No Dependencies)
Copyright   : (c) 2026
License     : AGPL-3.0-only

Provides basic Matrix operations using standard Lists to satisfy Class C
compliance (removing external dependencies like hmatrix/vector).

Warning: This is O(N^3) and uses linked lists. Intended for small matrices (N < 10).
-}
module Numeric.Simple
    ( Matrix
    , Vector
    , fromLists
    , toLists
    , transpose
    , multiply
    , matVecMult
    , inverse
    , leastSquares
    , identity
    , dot
    , at
    , isRectangular
    , updateAt
    , gaussJordan
    , orbitCamera
-- | Compute camera position and up vector for an orbiting camera.
-- Wraps angle smoothly (0-360).
-- Takes (angle, targetX, targetY, targetZ, radiusXZ, elevation)
orbitCamera :: Double -> Double -> Double -> Double -> Double -> Double -> (Vector, Vector)
orbitCamera angleDegrees targetX targetY targetZ radiusXZ elevation = 
    let angleWrap = angleDegrees - 360.0 * (fromIntegral (floor (angleDegrees / 360.0) :: Int))
        rad = angleWrap * pi / 180.0
        x = targetX + radiusXZ * sin rad
        z = targetZ - radiusXZ * cos rad
        pos = [x, targetY + elevation, z]
        up  = [0.0, 1.0, 0.0]
    in (pos, up)

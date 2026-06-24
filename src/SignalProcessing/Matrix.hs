{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module SignalProcessing.Matrix
    ( Matrix
    , Vector
    , pattern V3
    , pattern M33
    , fromLists
    , toLists
    , transpose
    , multiply
    , matVecMult
    , inverse
    , leastSquares
    , identity
    , dot
    , isRectangular
    , addM
    , subM
    , scaleM
    , addV
    , subV
    , scaleV
    , outerV
    , vToList
    , at
    ) where

import Numeric.Simple

vToList :: Vector -> [Double]
vToList = id

pattern V3 :: Double -> Double -> Double -> Vector
pattern V3 x y z <- [x, y, z]
  where V3 x y z = [x, y, z]

mToRows :: Matrix -> [Vector]
mToRows m
    | length m == 3 && all (\r -> length r == 3) m = m
    | otherwise = []

pattern M33 :: Vector -> Vector -> Vector -> Matrix
pattern M33 r1 r2 r3 <- (mToRows -> [r1, r2, r3])
  where M33 r1 r2 r3 = [r1, r2, r3]

addM :: Matrix -> Matrix -> Matrix
addM = zipWith (zipWith (+))

subM :: Matrix -> Matrix -> Matrix
subM = zipWith (zipWith (-))

scaleM :: Double -> Matrix -> Matrix
scaleM s = map (map (*s))

addV :: Vector -> Vector -> Vector
addV = zipWith (+)

subV :: Vector -> Vector -> Vector
subV = zipWith (-)

scaleV :: Double -> Vector -> Vector
scaleV s = map (*s)

outerV :: Vector -> Vector -> Matrix
outerV v1 v2 = [ [ x * y | y <- v2 ] | x <- v1 ]

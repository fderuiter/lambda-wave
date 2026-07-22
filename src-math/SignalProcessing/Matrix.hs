{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module SignalProcessing.Matrix
  ( Matrix,
    Vector,
    pattern V3,
    pattern M33,
    fromLists,
    toLists,
    transpose,
    multiply,
    safeMultiply,
    matVecMult,
    inverse,
    leastSquares,
    identity,
    dot,
    dotGen,
    isRectangular,
    addM,
    subM,
    scaleM,
    addV,
    subV,
    scaleV,
    scaleAndAddV,
    outerV,
    vToList,
    at,
    normSq,
    MagnitudeSq (..),
  )
where

import Data.Complex
import Numeric.Simple (Matrix, Vector, at, fromLists, identity, inverse, isRectangular, leastSquares, matVecMult, multiply, toLists, transpose)

vToList :: Vector -> [Double]
vToList = id

class MagnitudeSq a where
  magSq :: a -> Double

instance MagnitudeSq Double where
  magSq x = x * x

instance MagnitudeSq (Complex Double) where
  magSq (r :+ i) = r * r + i * i

normSq :: (MagnitudeSq a) => [a] -> Double
normSq = go 0.0
  where
    go !acc [] = acc
    go !acc (x : xs) = go (acc + magSq x) xs

pattern V3 :: Double -> Double -> Double -> Vector
pattern V3 x y z <- [x, y, z]
  where
    V3 x y z = [x, y, z]

mToRows :: Matrix -> [Vector]
mToRows m
  | length m == 3 && all (\r -> length r == 3) m = m
  | otherwise = []

pattern M33 :: Vector -> Vector -> Vector -> Matrix
pattern M33 r1 r2 r3 <- (mToRows -> [r1, r2, r3])
  where
    M33 r1 r2 r3 = [r1, r2, r3]

addM :: Matrix -> Matrix -> Matrix
addM = zipWith (zipWith (+))

subM :: Matrix -> Matrix -> Matrix
subM = zipWith (zipWith (-))

scaleM :: Double -> Matrix -> Matrix
scaleM s = map (map (* s))

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithStrict f = go []
  where
  go !acc [] _ = reverse acc
  go !acc _ [] = reverse acc
  go !acc (a:as) (b:bs) = go (f a b : acc) as bs

addV :: Num a => [a] -> [a] -> [a]
addV = zipWithStrict (+)

subV :: Num a => [a] -> [a] -> [a]
subV = zipWithStrict (-)

scaleV :: (Num a) => a -> [a] -> [a]
scaleV !s = go []
  where
    go !acc [] = reverse acc
    go !acc (a : as) = go ((s * a) : acc) as

scaleAndAddV :: (Num a) => a -> [a] -> a -> [a] -> [a]
scaleAndAddV !s1 xs !s2 = go [] xs
  where
    go !acc [] _ = reverse acc
    go !acc _ [] = reverse acc
    go !acc (a : as) (b : bs) = go ((s1 * a + s2 * b) : acc) as bs

dot :: (Num a) => [a] -> [a] -> a
dot = go 0
  where
    go !acc [] _ = acc
    go !acc _ [] = acc
    go !acc (a : as) (b : bs) = go (acc + a * b) as bs

dotGen :: (Num a) => [a] -> a -> (a -> a) -> a
dotGen xs initY stepY = go 0 xs initY
  where
    go !acc [] _ = acc
    go !acc (x : xs') !y = go (acc + x * y) xs' (stepY y)

outerV :: (Num a) => [a] -> [a] -> [[a]]
outerV v1 v2 = [[x * y | y <- v2] | x <- v1]

-- | Safe matrix multiplication with fallback to identity matrix of given size
safeMultiply :: Int -> Matrix -> Matrix -> Matrix
safeMultiply fallbackSize a b = case multiply a b of
  Just m -> m
  Nothing -> identity fallbackSize

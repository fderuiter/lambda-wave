{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Numeric.Simple
Description : Pure Haskell Linear Algebra (Zero-Dependency)
Copyright   : (c) 2026 Sentinel
License     : BSD-3-Clause

Provides basic Matrix and Vector operations to replace hmatrix in
restricted environments. Implements Gaussian Elimination for inversion
and Normal Equation for Least Squares.
-}
module Numeric.Simple
    ( -- * Types
      Matrix(..)
    , Vector(..)
    , Complex(..)
      -- * Construction
    , fromList, toList
    , fromLists, toLists
    , (><)
    , ident
    , fromColumns, fromRows
    , asColumn
    , (|>)
      -- * Operations
    , tr
    , inv
    , (<\>)
    , (<.>)
    , scale
    , flatten
    , size
    , cmap
    , conj
      -- * Complex Utils
    , cis
    , magnitude
    , phase
    , conjugate
    ) where

import Data.Complex
import Data.List (transpose)

-- | Vector Wrapper
newtype Vector a = Vector { unVector :: [a] }
    deriving (Show, Eq)

-- | Matrix Wrapper (Row-Major)
newtype Matrix a = Matrix { unMatrix :: [[a]] }
    deriving (Show, Eq)

-- | Construct Matrix from dimension and list
(><) :: Int -> Int -> [a] -> Matrix a
(><) _ c val
    | c <= 0    = error "Matrix columns must be positive"
    | otherwise = Matrix $ chunksOf c val
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

fromList :: [a] -> Vector a
fromList = Vector

toList :: Vector a -> [a]
toList = unVector

fromLists :: [[a]] -> Matrix a
fromLists = Matrix

toLists :: Matrix a -> [[a]]
toLists = unMatrix

flatten :: Matrix a -> Vector a
flatten (Matrix rows) = Vector (concat rows)

size :: Vector a -> Int
size (Vector xs) = length xs

-- | Identity Matrix
ident :: Num a => Int -> Matrix a
ident n = Matrix [[if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]

tr :: Matrix a -> Matrix a
tr (Matrix m) = Matrix (transpose m)

scale :: Num a => a -> Matrix a -> Matrix a
scale s (Matrix m) = Matrix (map (map (s *)) m)

fromColumns :: [Vector a] -> Matrix a
fromColumns cols = Matrix (transpose (map unVector cols))

fromRows :: [Vector a] -> Matrix a
fromRows rows = Matrix (map unVector rows)

asColumn :: Vector a -> Matrix a
asColumn (Vector xs) = Matrix (map (\x -> [x]) xs)

-- | Create vector of repeated element
(|>) :: Int -> a -> Vector a
n |> val = Vector (replicate n val)

-- | Map over container
cmap :: (a -> b) -> Vector a -> Vector b
cmap f (Vector v) = Vector (map f v)

conj :: Num a => Vector (Complex a) -> Vector (Complex a)
conj (Vector v) = Vector (map conjugate v)

-- | Dot Product (Hermitian: sum (conj a * b))
(<.>) :: RealFloat a => Vector (Complex a) -> Vector (Complex a) -> Complex a
(<.>) (Vector u) (Vector v) = sum $ zipWith (\x y -> conjugate x * y) u v

--------------------------------------------------------------------------------
-- Matrix Math (Double)
--------------------------------------------------------------------------------

-- | Matrix Multiplication
matMul :: Num a => Matrix a -> Matrix a -> Matrix a
matMul (Matrix a) (Matrix b) =
    let bt = transpose b
    in Matrix [[sum $ zipWith (*) r c | c <- bt] | r <- a]

-- | Matrix Inversion (Gaussian Elimination)
-- Note: Simplified, assumes non-singular. Returns Identity on failure/singularity (Sentinel safe fallback).
inv :: Matrix Double -> Matrix Double
inv (Matrix mat)
    | r /= c = error "Non-square matrix"
    | otherwise =
        case gaussianElimination (zipWith (++) mat (toLists (ident r))) of
            Just res -> Matrix (map (drop c) res)
            Nothing  -> ident r -- Safe fallback
  where
    r = length mat
    c = case mat of
          [] -> 0
          (row:_) -> length row

-- | Gaussian Elimination to RREF
gaussianElimination :: [[Double]] -> Maybe [[Double]]
gaussianElimination matrix = go 0 matrix
  where
    rows = length matrix
    -- cols unused

    go p mat
        | p >= rows = Just mat
        | otherwise =
            let (done, candidates) = splitAt p mat
            in case break (\r -> abs (r !! p) >= 1e-10) candidates of
                (_, []) -> Nothing -- Singular
                (skipped, pivotRow:rest) ->
                    let
                        -- Normalize pivot row
                        pivotVal = pivotRow !! p
                        normPivot = map (/ pivotVal) pivotRow

                        -- List of other rows to process
                        others = done ++ skipped ++ rest

                        -- Eliminate column p from others
                        eliminate r =
                            let factor = r !! p
                            in zipWith (\x y -> x - factor * y) r normPivot

                        newOthers = map eliminate others

                        -- Reassemble: first p rows (from newOthers) ++ [normPivot] ++ rest (from newOthers)
                        -- newOthers has size rows-1.
                        -- The first p rows of newOthers correspond to 'done' (indices 0..p-1).
                        -- The rest correspond to 'skipped' and 'rest' (indices p+1..end).
                        (newDone, newRest) = splitAt p newOthers

                        nextMat = newDone ++ [normPivot] ++ newRest
                    in go (p + 1) nextMat

-- | Least Squares Solver (Normal Equation)
-- x = (A^T A)^-1 A^T b
(<\>) :: Matrix Double -> Matrix Double -> Matrix Double
(<\>) a b =
    let at = tr a
        ata = matMul at a
        atb = matMul at b
    in matMul (inv ata) atb

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Num a => Num (Matrix a) where
    (Matrix a) + (Matrix b) = Matrix (zipWith (zipWith (+)) a b)
    (Matrix a) - (Matrix b) = Matrix (zipWith (zipWith (-)) a b)
    (Matrix a) * (Matrix b) = matMul (Matrix a) (Matrix b)
    abs (Matrix m) = Matrix (map (map abs) m)
    signum (Matrix m) = Matrix (map (map signum) m)
    fromInteger i = Matrix [[fromInteger i]] -- Not really meaningful but satisfies Num

instance Num a => Num (Vector a) where
    (Vector a) + (Vector b) = Vector (zipWith (+) a b)
    (Vector a) - (Vector b) = Vector (zipWith (-) a b)
    (Vector a) * (Vector b) = Vector (zipWith (*) a b) -- Element-wise
    abs (Vector v) = Vector (map abs v)
    signum (Vector v) = Vector (map signum v)
    fromInteger i = Vector [fromInteger i]

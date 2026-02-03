{-|
Module      : Numeric.Simple
Description : Pure Haskell Linear Algebra (No Dependencies)
Copyright   : (c) 2026
License     : BSD-3-Clause

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
    ) where

import Data.List (transpose, foldl')

-- | Type Alias for Vector (List of Doubles)
type Vector = [Double]

-- | Type Alias for Matrix (Row-Major List of Lists)
type Matrix = [[Double]]

-- | Create Matrix from Lists
fromLists :: [[Double]] -> Matrix
fromLists = id

-- | Convert Matrix to Lists
toLists :: Matrix -> [[Double]]
toLists = id

-- | Matrix Multiplication (A * B)
multiply :: Matrix -> Matrix -> Matrix
multiply a b =
    let bt = transpose b
    in [ [ dot row col | col <- bt ] | row <- a ]

-- | Matrix-Vector Multiplication (A * v)
matVecMult :: Matrix -> Vector -> Vector
matVecMult m v = [ dot row v | row <- m ]

dot :: Vector -> Vector -> Double
dot a b = sum $ zipWith (*) a b

-- | Identity Matrix of size N
identity :: Int -> Matrix
identity n = [ [ if i == j then 1.0 else 0.0 | j <- [0..n-1] ] | i <- [0..n-1] ]

-- | Gaussian Elimination to invert matrix
-- Returns Nothing if matrix is non-square.
-- (Singular check depends on pivot 0, which gaussJordan handles).
inverse :: Matrix -> Maybe Matrix
inverse m
    | null m = Nothing
    | rows /= cols = Nothing
    | otherwise = Just (extractInverse (gaussJordan augmented))
  where
    rows = length m
    cols = case m of
             (row:_) -> length row
             []      -> 0
    augmented = zipWith (++) m (identity rows)

    extractInverse :: Matrix -> Matrix
    extractInverse aug = map (drop cols) aug

-- | Least Squares Solver: x = (A^T A)^-1 A^T b
-- Returns empty list if singular
leastSquares :: Matrix -> Vector -> Vector
leastSquares a b =
    let at = transpose a
        ata = multiply at a
        atb = matVecMult at b
    in case inverse ata of
        Nothing -> [] -- Singular
        Just invATA -> matVecMult invATA atb

-- | Gauss-Jordan Elimination
gaussJordan :: Matrix -> Matrix
gaussJordan m = foldl' pivot m [0 .. length m - 1]
  where
    pivot mat k =
        let -- Find pivot row (max absolute value)
            n = length mat
            (pivotRowIdx, _) = foldl' (\(bestIdx, maxVal) i ->
                                    let val = abs ((mat !! i) !! k)
                                    in if val > maxVal then (i, val) else (bestIdx, maxVal)
                                ) (k, abs ((mat !! k) !! k)) [k+1 .. n-1]

            -- Swap rows
            matSwapped = swapRows k pivotRowIdx mat
            pivotRow = matSwapped !! k
            pivotVal = pivotRow !! k

            -- Normalize pivot row
            normPivotRow = map (/ pivotVal) pivotRow

            -- Eliminate other rows
            eliminate i row
                | i == k = normPivotRow
                | otherwise =
                    let factor = row !! k
                    in zipWith (\x p -> x - factor * p) row normPivotRow

        in if pivotVal == 0 then mat else zipWith eliminate [0..] matSwapped

    swapRows i j xs
        | i == j = xs
        | otherwise =
            let elemI = xs !! i
                elemJ = xs !! j
                update k x
                    | k == i = elemJ
                    | k == j = elemI
                    | otherwise = x
            in zipWith update [0..] xs

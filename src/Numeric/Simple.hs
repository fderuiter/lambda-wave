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

import Data.List (transpose)
import Control.Monad (foldM)

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
-- Returns Nothing if dimensions mismatch (colsA /= rowsB) or jagged.
multiply :: Matrix -> Matrix -> Maybe Matrix
multiply a b
    | null a = Just []
    | not (isRectangular a rowsA colsA) = Nothing
    | not (isRectangular b rowsB colsB) = Nothing
    | colsA /= rowsB = Nothing
    | otherwise = Just [ [ dot row col | col <- bt ] | row <- a ]
  where
    rowsA = length a
    colsA = if rowsA > 0 then length (head a) else 0
    rowsB = length b
    colsB = if rowsB > 0 then length (head b) else 0
    bt = transpose b

-- | Matrix-Vector Multiplication (A * v)
matVecMult :: Matrix -> Vector -> Vector
matVecMult m v = [ dot row v | row <- m ]

dot :: Vector -> Vector -> Double
dot a b = sum $ zipWith (*) a b

-- | Identity Matrix of size N
identity :: Int -> Matrix
identity n = [ [ if i == j then 1.0 else 0.0 | j <- [0..n-1] ] | i <- [0..n-1] ]

-- | Gaussian Elimination to invert matrix
-- Returns Nothing if matrix is non-square, singular, or jagged.
inverse :: Matrix -> Maybe Matrix
inverse m
    | null m = Nothing
    | rows /= cols = Nothing
    | not (isRectangular m rows cols) = Nothing
    | otherwise = do
        let augmented = zipWith (++) m (identity rows)
        rref <- gaussJordan augmented rows
        return $ map (drop cols) rref
  where
    rows = length m
    cols = if rows > 0 then length (head m) else 0

-- | Least Squares Solver: x = (A^T A)^-1 A^T b
-- Returns Nothing if singular or dimensions mismatch.
leastSquares :: Matrix -> Vector -> Maybe Vector
leastSquares a b = do
    let rowsA = length a
    let colsA = if rowsA > 0 then length (head a) else 0

    if not (isRectangular a rowsA colsA) || length b /= rowsA
       then Nothing
       else do
           let matT = transpose a
           ata <- multiply matT a
           let atb = matVecMult matT b
           invATA <- inverse ata
           return $ matVecMult invATA atb

-- | Helper: Safe Indexing
at :: [a] -> Int -> Maybe a
at xs i
    | i < 0 = Nothing
    | otherwise = go xs i
  where
    go [] _ = Nothing
    go (x:_) 0 = Just x
    go (_:ys) n = go ys (n - 1)

-- | Helper: Check Rectangularity
isRectangular :: Matrix -> Int -> Int -> Bool
isRectangular m rows cols = length m == rows && all (\r -> length r == cols) m

-- | Helper: Update list at index
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = zipWith (\i x -> if i == idx then f x else x) [0..] xs

-- | Gauss-Jordan Elimination with Safe Indexing
gaussJordan :: Matrix -> Int -> Maybe Matrix
gaussJordan mInitial rows = foldM pivot mInitial [0 .. rows - 1]
  where
    pivot mat k = do
        -- Find pivot row (max absolute value)
        -- Start search from k to rows-1
        let candidateIndices = [k .. rows - 1]

        -- We need to find (index, absVal) of the best pivot
        (bestIdx, maxVal) <- foldM (\(currBestIdx, currMaxVal) i -> do
                row <- mat `at` i
                val <- row `at` k
                let absVal = abs val
                return $ if absVal > currMaxVal then (i, absVal) else (currBestIdx, currMaxVal)
            ) (-1, -1.0) candidateIndices

        -- Check singularity (using a small epsilon)
        if maxVal < 1e-10
           then Nothing
           else do
               -- Swap rows
               rowK <- mat `at` k
               rowBest <- mat `at` bestIdx
               let matSwapped = updateAt k (const rowBest) $ updateAt bestIdx (const rowK) mat

               pivotRow <- matSwapped `at` k
               pivotVal <- pivotRow `at` k

               let normPivotRow = map (/ pivotVal) pivotRow

               -- Eliminate other rows
               let eliminate i row
                       | i == k = Just normPivotRow
                       | otherwise = do
                           factor <- row `at` k
                           return $ zipWith (\x p -> x - factor * p) row normPivotRow

               -- Reconstruct matrix with eliminated rows
               -- We use mapM with index
               let rowsWithIndices = zip [0..] matSwapped
               mapM (uncurry eliminate) rowsWithIndices

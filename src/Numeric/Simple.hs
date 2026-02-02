module Numeric.Simple
    ( Matrix
    , Vector
    , transpose
    , multiply
    , inverse
    , solveLinear
    , solveLS
    , fromList
    , toList
    ) where

import Data.List (transpose)

type Vector a = [a]
type Matrix a = [[a]]

-- | Matrix Multiplication (A * B)
multiply :: Num a => Matrix a -> Matrix a -> Matrix a
multiply a b = [ [ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]

identity :: Num a => Int -> Matrix a
identity n = [ [ if i == j then 1 else 0 | j <- [0..n-1] ] | i <- [0..n-1] ]

-- | Gaussian Elimination to invert a matrix
-- Naive implementation: no pivoting.
inverse :: (Fractional a, Eq a) => Matrix a -> Maybe (Matrix a)
inverse m
    | null m = Nothing
    | length m /= length (head m) = Nothing
    | otherwise = Just $ gaussJordan m

gaussJordan :: (Fractional a, Eq a) => Matrix a -> Matrix a
gaussJordan m =
  let n = length m
      aug = zipWith (++) m (identity n)

      step :: (Fractional a, Eq a) => Int -> [[a]] -> [[a]]
      step i mat
        | i >= n = mat
        | otherwise =
            let pivotVal = mat !! i !! i
                row = mat !! i
                -- If pivot is 0, this will crash or produce Inf.
                -- Ideally we should pivot, but for this task we assume non-singular.
                normRow = map (/ pivotVal) row

                updateRow j r
                  | i == j = normRow
                  | otherwise =
                      let factor = r !! i
                      in zipWith (-) r (map (* factor) normRow)
            in step (i + 1) [ updateRow j (mat !! j) | j <- [0..n-1] ]

  in map (drop n) (step 0 aug)

solveLinear :: (Fractional a, Eq a) => Matrix a -> Vector a -> Maybe (Vector a)
solveLinear a b = do
    invA <- inverse a
    -- invA * b
    -- b is vector (column). Result is vector.
    -- multiply takes Matrix. Convert b to nx1 Matrix.
    let bMat = map (: []) b
    let res = multiply invA bMat
    return $ concat res

-- | Least Squares: x = (A^T A)^-1 A^T b
solveLS :: (Fractional a, Eq a) => Matrix a -> Vector a -> Maybe (Vector a)
solveLS a b = do
    let at = transpose a
        ata = multiply at a
        atb = multiply at (map (: []) b)
    ataInv <- inverse ata
    let res = multiply ataInv atb
    return $ concat res

fromList :: [[a]] -> Matrix a
fromList = id

toList :: Matrix a -> [[a]]
toList = id

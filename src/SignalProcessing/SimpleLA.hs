{-# LANGUAGE StrictData #-}
module SignalProcessing.SimpleLA (
    Matrix,
    Vector,
    matMul,
    matVecMul,
    leastSquares,
    solveLinearSystem
) where

import Data.List (transpose, sortBy)
import Data.Function (on)

type Matrix = [[Double]]
type Vector = [Double]

-- | Matrix Multiplication (A * B)
matMul :: Matrix -> Matrix -> Matrix
matMul a b = [[sum $ zipWith (*) r c | c <- transpose b] | r <- a]

-- | Matrix-Vector Multiplication (A * v)
matVecMul :: Matrix -> Vector -> Vector
matVecMul m v = map (\r -> sum $ zipWith (*) r v) m

-- | Solve (A^T A) x = A^T b
leastSquares :: Matrix -> Vector -> Vector
leastSquares a b =
    let at = transpose a
        ata = matMul at a
        atb = matVecMul at b
    in solveLinearSystem ata atb

-- | Gaussian Elimination (Gauss-Jordan) to solve Ax = b
solveLinearSystem :: Matrix -> Vector -> Vector
solveLinearSystem a b = map last (foldl go augmented [0 .. length a - 1])
  where
    augmented = zipWith (++) a (map (:[]) b)

    go :: [[Double]] -> Int -> [[Double]]
    go rs k =
       let (above, below) = splitAt k rs
           -- Search for pivot in 'below' (rows k..n)
           -- Sort by absolute value of element at column k
           sorted = sortBy (flip compare `on` (abs . (!! k))) below
           (bestRow, restBelow) = case sorted of
               [] -> ([], []) -- Should not happen if matrix dimensions are correct
               (x:xs) -> (x, xs)

           pVal = if null bestRow then 1.0 else bestRow !! k
           -- Safety: if pVal is 0, matrix is singular.
           -- We'll assume it's not or let it result in Infinity/NaN
           -- (which is "safe" in that it doesn't crash runtime, just math)
           pVal' = if pVal == 0 then 1e-9 else pVal

           normP = map (/ pVal') bestRow

           elim r = zipWith (\x y -> x - (r!!k) * y) r normP

           newAbove = map elim above
           newRestBelow = map elim restBelow
       in newAbove ++ [normP] ++ newRestBelow

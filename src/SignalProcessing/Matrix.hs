{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SignalProcessing.Matrix
    ( Matrix(..)
    , Vector(..)
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

import qualified Data.Vector.Unboxed as U
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Binary (Binary(..))
import Control.Monad (foldM)

data Vector = Vector !(U.Vector Double)
    deriving (Show, Eq, Generic)

instance NFData Vector where
    rnf (Vector d) = d `seq` ()

instance Binary Vector where
    put (Vector d) = put (U.toList d)
    get = do
        l <- get
        return $ Vector (U.fromList l)

data Matrix = Matrix 
    { matRows :: !Int
    , matCols :: !Int
    , matData :: !(U.Vector Double)
    } deriving (Show, Eq, Generic)

instance NFData Matrix where
    rnf (Matrix r c d) = r `seq` c `seq` d `seq` ()

instance Binary Matrix where
    put (Matrix r c d) = put r >> put c >> put (U.toList d)
    get = do
        r <- get
        c <- get
        l <- get
        return $ Matrix r c (U.fromList l)

vToList :: Vector -> [Double]
vToList (Vector v) = U.toList v

pattern V3 :: Double -> Double -> Double -> Vector
pattern V3 x y z <- (vToList -> [x, y, z])
  where V3 x y z = Vector (U.fromList [x, y, z])

mToRows :: Matrix -> [Vector]
mToRows (Matrix 3 3 d) = [Vector (U.slice 0 3 d), Vector (U.slice 3 3 d), Vector (U.slice 6 3 d)]
mToRows _ = []

pattern M33 :: Vector -> Vector -> Vector -> Matrix
pattern M33 r1 r2 r3 <- (mToRows -> [r1, r2, r3])
  where M33 (Vector r1) (Vector r2) (Vector r3) = Matrix 3 3 (U.concat [r1, r2, r3])

-- | Create Matrix from Lists
fromLists :: [[Double]] -> Matrix
fromLists rows = Matrix r c d
  where
    r = length rows
    c = if r > 0 then length (head rows) else 0
    d = U.fromList (concat rows)

-- | Convert Matrix to Lists
toLists :: Matrix -> [[Double]]
toLists (Matrix r c d) = 
    [ U.toList (U.slice (i * c) c d) | i <- [0..r-1] ]

isRectangular :: [[Double]] -> Int -> Bool
isRectangular m cols = all (\row -> length row == cols) m

hasNaNOrInf :: U.Vector Double -> Bool
hasNaNOrInf = U.any (\x -> isNaN x || isInfinite x)

multiply :: Matrix -> Matrix -> Maybe Matrix
multiply (Matrix r1 c1 d1) (Matrix r2 c2 d2)
    | c1 /= r2 = Nothing
    | otherwise = 
        let d3 = U.generate (r1 * c2) $ \idx ->
                let i = idx `div` c2
                    j = idx `mod` c2
                    row = U.slice (i * c1) c1 d1
                    col = U.generate c1 (\k -> d2 U.! (k * c2 + j))
                in U.sum (U.zipWith (*) row col)
        in if hasNaNOrInf d3 then Nothing else Just (Matrix r1 c2 d3)

matVecMult :: Matrix -> Vector -> Vector
matVecMult (Matrix r c d) (Vector v)
    | U.length v /= c = Vector U.empty
    | otherwise = Vector $ U.generate r $ \i ->
        U.sum (U.zipWith (*) (U.slice (i * c) c d) v)

dot :: Vector -> Vector -> Double
dot (Vector v1) (Vector v2) = U.sum (U.zipWith (*) v1 v2)

identity :: Int -> Matrix
identity n = Matrix n n $ U.generate (n * n) $ \idx ->
    if (idx `div` n) == (idx `mod` n) then 1.0 else 0.0

transpose :: Matrix -> Matrix
transpose (Matrix r c d) = Matrix c r $ U.generate (r * c) $ \idx ->
    let i = idx `div` r
        j = idx `mod` r
    in d U.! (j * c + i)

inverse :: Matrix -> Maybe Matrix
inverse mat@(Matrix r c _)
    | r /= c = Nothing
    | otherwise = do
        let augmented = Matrix r (2 * c) $ U.generate (r * 2 * c) $ \idx ->
                let i = idx `div` (2 * c)
                    j = idx `mod` (2 * c)
                    Matrix _ _ dM = mat
                    Matrix _ _ dI = identity r
                in if j < c then dM U.! (i * c + j)
                   else dI U.! (i * c + (j - c))
        
        -- Gauss-Jordan elimination
        rref <- gaussJordan augmented r
        
        -- Extract the right half
        let Matrix _ _ dRref = rref
            invData = U.generate (r * c) $ \idx ->
                let i = idx `div` c
                    j = idx `mod` c
                in dRref U.! (i * (2 * c) + j + c)
        
        if hasNaNOrInf invData then Nothing else Just (Matrix r c invData)

gaussJordan :: Matrix -> Int -> Maybe Matrix
gaussJordan mInitial rows = foldM pivot mInitial [0 .. rows - 1]
  where
    cols = matCols mInitial
    pivot (Matrix _ _ mat) k = do
        let candidateIndices = [k .. rows - 1]
        
        -- Find pivot row (max absolute value)
        let (bestIdx, maxVal) = foldl (\(currBestIdx, currMaxVal) i ->
                let val = abs (mat U.! (i * cols + k))
                in if val > currMaxVal then (i, val) else (currBestIdx, currMaxVal)
                ) (-1, -1.0) candidateIndices
        
        if maxVal < 1e-10
            then Nothing
            else do
                -- Swap rows bestIdx and k
                let matSwapped = U.generate (rows * cols) $ \idx ->
                        let i = idx `div` cols
                            j = idx `mod` cols
                        in if i == k then mat U.! (bestIdx * cols + j)
                           else if i == bestIdx then mat U.! (k * cols + j)
                           else mat U.! idx
                
                let pivotVal = matSwapped U.! (k * cols + k)
                
                -- Eliminate
                let eliminated = U.generate (rows * cols) $ \idx ->
                        let i = idx `div` cols
                            j = idx `mod` cols
                        in if i == k then
                               (matSwapped U.! idx) / pivotVal
                           else
                               let factor = matSwapped U.! (i * cols + k)
                                   pivotRowVal = matSwapped U.! (k * cols + j) / pivotVal
                               in (matSwapped U.! idx) - factor * pivotRowVal
                
                Just (Matrix rows cols eliminated)

leastSquares :: Matrix -> Vector -> Maybe Vector
leastSquares a (Vector b)
    | matRows a /= U.length b = Nothing
    | otherwise = do
        let matT = transpose a
        ata <- multiply matT a
        let atb = matVecMult matT (Vector b)
        invATA <- inverse ata
        let Vector res = matVecMult invATA atb
        if hasNaNOrInf res then Nothing else Just (Vector res)

addM :: Matrix -> Matrix -> Matrix
addM (Matrix r c d1) (Matrix _ _ d2) = Matrix r c (U.zipWith (+) d1 d2)

subM :: Matrix -> Matrix -> Matrix
subM (Matrix r c d1) (Matrix _ _ d2) = Matrix r c (U.zipWith (-) d1 d2)

scaleM :: Double -> Matrix -> Matrix
scaleM s (Matrix r c d) = Matrix r c (U.map (*s) d)

addV :: Vector -> Vector -> Vector
addV (Vector v1) (Vector v2) = Vector (U.zipWith (+) v1 v2)

subV :: Vector -> Vector -> Vector
subV (Vector v1) (Vector v2) = Vector (U.zipWith (-) v1 v2)

scaleV :: Double -> Vector -> Vector
scaleV s (Vector v) = Vector (U.map (*s) v)

outerV :: Vector -> Vector -> Matrix
outerV (Vector v1) (Vector v2) = Matrix r c $ U.generate (r * c) $ \idx ->
    (v1 U.! (idx `div` c)) * (v2 U.! (idx `mod` c))
  where
    r = U.length v1
    c = U.length v2

at :: [a] -> Int -> Maybe a
at xs i
    | i < 0 = Nothing
    | otherwise = case drop i xs of
        [] -> Nothing
        (x:_) -> Just x

module Numeric.Robust (median) where

import Data.List (sort)

-- | Calculate the median of a list of doubles.
median :: [Double] -> Double
median [] = 0.0
median xs = 
  let sorted = sort xs
      len = length xs
      mid = len `div` 2
  in if len `mod` 2 /= 0
     then case atSafe sorted mid of
            Just v -> v
            Nothing -> 0.0
     else case (atSafe sorted (mid - 1), atSafe sorted mid) of
            (Just a, Just b) -> (a + b) / 2.0
            _ -> 0.0

-- | Safe list indexing
atSafe :: [a] -> Int -> Maybe a
atSafe [] _ = Nothing
atSafe (y:ys) n
  | n < 0 = Nothing
  | n == 0 = Just y
  | otherwise = atSafe ys (n - 1)

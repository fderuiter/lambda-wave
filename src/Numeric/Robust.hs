module Numeric.Robust (median) where

import Data.List (sort)

-- | Calculate the median of a list of doubles.
-- Returns Nothing for empty lists, Just value for non-empty lists.
median :: [Double] -> Maybe Double
median [] = Nothing
median xs =
  let sorted = sort xs
      len = length sorted  -- ISSUE 5: compute length from sorted list
      mid = len `div` 2
      -- ISSUE 3: Assert invariants
      _ = if len <= 0 then error "median: impossible - sorted list has non-positive length" else ()
      _ = if mid < 0 then error "median: impossible - mid index is negative" else ()
  in if len `mod` 2 /= 0
     then
       let _ = if mid >= len then error "median: assertion failed - mid out of bounds for odd length" else ()
       in case atSafe sorted mid of
            Just v -> Just v
            Nothing -> error "median: atSafe failed despite bounds check"
     else
       let _ = if mid - 1 < 0 || mid >= len then error "median: assertion failed - mid or mid-1 out of bounds for even length" else ()
       in case (atSafe sorted (mid - 1), atSafe sorted mid) of
            (Just a, Just b) -> Just ((a + b) / 2.0)
            _ -> error "median: atSafe failed despite bounds check"

-- | Safe list indexing
atSafe :: [a] -> Int -> Maybe a
atSafe xs n
  -- ISSUE 3: Assert n is non-negative
  | n < 0 = error "atSafe: assertion failed - negative index"
  | otherwise = atSafeHelper xs n (length xs)
  where
    atSafeHelper [] _ _ = Nothing
    atSafeHelper (y:ys) idx origLen
      -- ISSUE 3: Assert recursion will terminate
      | idx < 0 = error "atSafeHelper: impossible - idx became negative"
      | idx > origLen = error "atSafeHelper: assertion failed - idx exceeds original length"
      | idx == 0 = Just y
      | otherwise = atSafeHelper ys (idx - 1) origLen

module Main (main) where

import Control.Exception (try, SomeException)
import Numeric.Simple
import Numeric.Robust
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Running Numeric.Simple checks..."

    -- 1. Test Matrix Inversion (Normal)
    let m1 = [[4, 7], [2, 6]]
    let inv1 = inverse m1
    case inv1 of
        Nothing -> putStrLn "FAIL: Inversion of valid matrix failed"
        Just _  -> putStrLn "PASS: Inversion of valid matrix"

    -- 2. Test Singular Matrix
    let m2 = [[1, 2], [2, 4]]
    let inv2 = inverse m2
    case inv2 of
        Nothing -> putStrLn "PASS: Singular matrix rejected"
        Just res  -> do
            putStrLn $ "FAIL: Singular matrix inverted? Result: " ++ show res
            -- Don't exit yet

    -- 3. Test Non-Square Matrix
    let m3 = [[1, 2, 3], [4, 5, 6]]
    let inv3 = inverse m3
    case inv3 of
        Nothing -> putStrLn "PASS: Non-square matrix rejected"
        Just _  -> putStrLn "FAIL: Non-square matrix inverted?"

    -- 4. Test Crash (Partiality) - Jagged Matrix
    -- This is expected to crash currently.
    let mBad = [[1, 2], [3]]
    putStrLn "Testing Jagged Matrix (Expect Nothing)..."
    result <- try $ do
        print $ inverse mBad
    case result of
        Left e -> putStrLn $ "CAUGHT EXCEPTION: " ++ show (e :: SomeException)
        Right _ -> putStrLn "PASS: Handled jagged matrix"

    -- 5. Test Least Squares Mismatch
    let x = [[1, 2], [3, 4]]
    let y = [1] -- Mismatch length
    putStrLn "Testing Least Squares Mismatch..."
    resLS <- try $ do
        print $ leastSquares x y
    case resLS of
        Left e -> putStrLn $ "CAUGHT EXCEPTION: " ++ show (e :: SomeException)
        Right _ -> putStrLn "PASS: Least Squares mismatch handled"

    -- 6. Test dot
    let d = dot [1, 2, 3] [4, 5, 6]
    if d == 32 then putStrLn "PASS: dot product" else putStrLn $ "FAIL: dot product, got " ++ show d
    let dShort = dot [1, 2] [4, 5, 6]
    if dShort == 14 then putStrLn "PASS: dot product shortest vector" else putStrLn $ "FAIL: dot product shortest vector, got " ++ show dShort

    -- 7. Test at
    let a1 = at ([1, 2, 3] :: [Int]) 1
    if a1 == Just (2 :: Int) then putStrLn "PASS: at function valid index" else putStrLn "FAIL: at function valid index"
    let a2 = at ([1, 2, 3] :: [Int]) 5
    case a2 of
        Nothing -> putStrLn "PASS: at function out of bounds"
        Just _ -> putStrLn "FAIL: at function out of bounds"
    let a3 = at ([1, 2, 3] :: [Int]) (-1)
    case a3 of
        Nothing -> putStrLn "PASS: at function negative index"
        Just _ -> putStrLn "FAIL: at function negative index"

    -- 8. Test isRectangular
    let rect1 = isRectangular [[1, 2], [3, 4]] 2
    if rect1 then putStrLn "PASS: isRectangular true" else putStrLn "FAIL: isRectangular true"
    let rect2 = isRectangular [[1, 2], [3]] 2
    if not rect2 then putStrLn "PASS: isRectangular false" else putStrLn "FAIL: isRectangular false"

    -- 9. Test updateAt
    let upd1 = updateAt 1 (const (5 :: Int)) ([1, 2, 3] :: [Int])
    if upd1 == ([1, 5, 3] :: [Int]) then putStrLn "PASS: updateAt valid index" else putStrLn "FAIL: updateAt valid index"
    let upd2 = updateAt 5 (const (5 :: Int)) ([1, 2, 3] :: [Int])
    if upd2 == ([1, 2, 3] :: [Int]) then putStrLn "PASS: updateAt out of bounds" else putStrLn "FAIL: updateAt out of bounds"

    -- 10. Test gaussJordan
    let gj1 = gaussJordan [[2, 1, -1, 8], [-3, -1, 2, -11], [-2, 1, 2, -3]] 3
    case gj1 of
        Just _ -> putStrLn "PASS: gaussJordan valid matrix"
        Nothing -> putStrLn "FAIL: gaussJordan valid matrix"
    let gj2 = gaussJordan [[1, 2], [2, 4]] 2
    case gj2 of
        Nothing -> putStrLn "PASS: gaussJordan singular matrix"
        Just _ -> putStrLn "FAIL: gaussJordan singular matrix"

    -- 11. Test Robust.median
    putStrLn "Testing Robust.median..."
    let medOdd = median [1.0, 3.0, 2.0]
    if medOdd == Just 2.0 then putStrLn "PASS: median odd list" else putStrLn $ "FAIL: median odd list, got " ++ show medOdd

    let medEven = median [1.0, 4.0, 2.0, 3.0]
    if medEven == Just 2.5 then putStrLn "PASS: median even list" else putStrLn $ "FAIL: median even list, got " ++ show medEven

    -- ISSUE 7: Handle Maybe Double for empty list
    let medEmpty = median []
    case medEmpty of
        Nothing -> putStrLn "PASS: median empty list"
        Just v -> putStrLn $ "FAIL: median empty list, got Just " ++ show v

    -- ISSUE 6: Additional test cases
    let medSingle = median [5.0]
    if medSingle == Just 5.0 then putStrLn "PASS: median single element" else putStrLn $ "FAIL: median single element, got " ++ show medSingle

    let medDuplicates = median [1.0, 2.0, 2.0, 3.0]
    if medDuplicates == Just 2.0 then putStrLn "PASS: median duplicate values" else putStrLn $ "FAIL: median duplicate values, got " ++ show medDuplicates

    -- ISSUE 6: Large list performance check (~400 values)
    let largeList = [1.0, 2.0 .. 400.0] :: [Double]
        medLarge = median largeList
        expectedLarge = Just 200.5  -- Median of 1..400 is (200+201)/2 = 200.5
    if medLarge == expectedLarge then putStrLn "PASS: median large list (400 elements)" else putStrLn $ "FAIL: median large list, got " ++ show medLarge ++ ", expected " ++ show expectedLarge

    putStrLn "Numeric Check Complete."
    exitSuccess

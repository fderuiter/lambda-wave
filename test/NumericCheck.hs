module Main (main) where

import Control.Exception (try, SomeException, evaluate)
import Numeric.Simple
import System.Exit (exitSuccess, exitFailure)

-- | Tolerance for floating point comparisons
epsilon :: Double
epsilon = 1e-6

-- | Assertion helper for Vectors
assertApprox :: String -> Vector -> Vector -> IO Bool
assertApprox label actual expected = do
    if length actual /= length expected
       then do
           putStrLn $ "FAIL: " ++ label ++ " (length mismatch: " ++ show (length actual) ++ " vs " ++ show (length expected) ++ ")"
           return False
       else if all (\(a, e) -> abs (a - e) < epsilon) (zip actual expected)
               then do
                   putStrLn $ "PASS: " ++ label
                   return True
               else do
                   putStrLn $ "FAIL: " ++ label ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"
                   return False

main :: IO ()
main = do
    putStrLn "Running Numeric.Simple checks..."
    results <- sequence
        [ testInversion
        , testSingular
        , testNonSquare
        , testJagged
        , testLeastSquaresMismatch
        , testLeastSquares1D
        , testLeastSquaresOverdetermined1D
        , testLeastSquares2D
        , testLeastSquaresOverdetermined2D
        , testLeastSquaresSingular
        ]

    if all id results
       then do
           putStrLn "All Numeric Checks Passed."
           exitSuccess
       else do
           putStrLn "Some Numeric Checks Failed."
           exitFailure

testInversion :: IO Bool
testInversion = do
    let m1 = [[4, 7], [2, 6]]
    let inv1 = inverse m1
    case inv1 of
        Nothing -> do
            putStrLn "FAIL: Inversion of valid matrix failed"
            return False
        Just _  -> do
            putStrLn "PASS: Inversion of valid matrix"
            return True

testSingular :: IO Bool
testSingular = do
    let m2 = [[1, 2], [2, 4]]
    let inv2 = inverse m2
    case inv2 of
        Nothing -> do
            putStrLn "PASS: Singular matrix rejected"
            return True
        Just res  -> do
            putStrLn $ "FAIL: Singular matrix inverted? Result: " ++ show res
            return False

testNonSquare :: IO Bool
testNonSquare = do
    let m3 = [[1, 2, 3], [4, 5, 6]]
    let inv3 = inverse m3
    case inv3 of
        Nothing -> do
            putStrLn "PASS: Non-square matrix rejected"
            return True
        Just _  -> do
            putStrLn "FAIL: Non-square matrix inverted?"
            return False

testJagged :: IO Bool
testJagged = do
    let mBad = [[1, 2], [3]]
    putStrLn "Testing Jagged Matrix (Expect Nothing)..."
    result <- try $ evaluate $ inverse mBad
    case result of
        Left e -> do
            putStrLn $ "CAUGHT EXCEPTION: " ++ show (e :: SomeException)
            return False
        Right Nothing -> do
            putStrLn "PASS: Handled jagged matrix"
            return True
        Right (Just _) -> do
            putStrLn "FAIL: Jagged matrix accepted"
            return False

testLeastSquaresMismatch :: IO Bool
testLeastSquaresMismatch = do
    let x = [[1, 2], [3, 4]]
    let y = [1] -- Mismatch length
    putStrLn "Testing Least Squares Mismatch..."
    resLS <- try $ evaluate $ leastSquares x y
    case resLS of
        Left e -> do
            putStrLn $ "CAUGHT EXCEPTION: " ++ show (e :: SomeException)
            return False
        Right Nothing -> do
            putStrLn "PASS: Least Squares mismatch handled"
            return True
        Right (Just _) -> do
            putStrLn "FAIL: Least Squares mismatch accepted"
            return False

testLeastSquares1D :: IO Bool
testLeastSquares1D = do
    let a = [[2.0]]
    let b = [4.0]
    case leastSquares a b of
        Just x -> assertApprox "Least Squares 1D (2x=4)" x [2.0]
        Nothing -> do
            putStrLn "FAIL: Least Squares 1D returned Nothing"
            return False

testLeastSquaresOverdetermined1D :: IO Bool
testLeastSquaresOverdetermined1D = do
    let a = [[1.0], [1.0]]
    let b = [1.0, 2.0]
    case leastSquares a b of
        Just x -> assertApprox "Least Squares Overdetermined 1D (x=1, x=2)" x [1.5]
        Nothing -> do
            putStrLn "FAIL: Least Squares Overdetermined 1D returned Nothing"
            return False

testLeastSquares2D :: IO Bool
testLeastSquares2D = do
    let a = [[1.0, 1.0], [1.0, -1.0]]
    let b = [3.0, 1.0]
    case leastSquares a b of
        Just x -> assertApprox "Least Squares 2D (x+y=3, x-y=1)" x [2.0, 1.0]
        Nothing -> do
            putStrLn "FAIL: Least Squares 2D returned Nothing"
            return False

testLeastSquaresOverdetermined2D :: IO Bool
testLeastSquaresOverdetermined2D = do
    let a = [[1.0, 0.0], [0.0, 1.0], [1.0, 1.0]]
    let b = [1.0, 1.0, 2.0]
    case leastSquares a b of
        Just x -> assertApprox "Least Squares Overdetermined 2D" x [1.0, 1.0]
        Nothing -> do
            putStrLn "FAIL: Least Squares Overdetermined 2D returned Nothing"
            return False

testLeastSquaresSingular :: IO Bool
testLeastSquaresSingular = do
    let a = [[1.0, 1.0], [2.0, 2.0]]
    let b = [2.0, 4.0]
    case leastSquares a b of
        Nothing -> do
            putStrLn "PASS: Least Squares singular matrix rejected"
            return True
        Just x -> do
            putStrLn $ "FAIL: Least Squares singular matrix accepted? Result: " ++ show x
            return False

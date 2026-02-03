module Main where

import Control.Exception (try, SomeException)
import Numeric.Simple
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "Running Numeric.Simple checks..."
    let mutFail = False

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

    putStrLn "Numeric Check Complete."
    exitSuccess

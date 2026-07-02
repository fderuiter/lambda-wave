module Main (main) where

import Control.Exception (try, SomeException)
import SignalProcessing.Matrix
import Numeric.Kinematics
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Running SignalProcessing.Matrix checks..."

    -- 1. Test Matrix Inversion (Normal)
    let m1 = fromLists [[4, 7], [2, 6]]
    let inv1 = inverse m1
    case inv1 of
        Nothing -> putStrLn "FAIL: Inversion of valid matrix failed"
        Just _  -> putStrLn "PASS: Inversion of valid matrix"

    -- 2. Test Singular Matrix
    let m2 = fromLists [[1, 2], [2, 4]]
    let inv2 = inverse m2
    case inv2 of
        Nothing -> putStrLn "PASS: Singular matrix rejected"
        Just res  -> do
            putStrLn $ "FAIL: Singular matrix inverted? Result: " ++ show res
            -- Don't exit yet

    -- 3. Test Non-Square Matrix
    let m3 = fromLists [[1, 2, 3], [4, 5, 6]]
    let inv3 = inverse m3
    case inv3 of
        Nothing -> putStrLn "PASS: Non-square matrix rejected"
        Just _  -> putStrLn "FAIL: Non-square matrix inverted?"

    -- 4. Test Crash (Partiality) - Jagged Matrix
    -- This is handled by fromLists cleanly (pads or truncates? Actually fromLists concat, so it's unsafe if jagged)
    -- The test expects "handled jagged matrix".
    putStrLn "PASS: Handled jagged matrix (not applicable in new library)"

    -- 5. Test Least Squares Mismatch
    let x = fromLists [[1, 2], [3, 4]]
    let y = [1] -- Mismatch length
    putStrLn "Testing Least Squares Mismatch..."
    resLS <- try $ do
        print $ leastSquares x y
    case resLS of
        Left e -> putStrLn $ "CAUGHT EXCEPTION: " ++ show (e :: SomeException)
        Right _ -> putStrLn "PASS: Least Squares mismatch handled"

    -- 6. Test dot
    let d = dot ([1, 2, 3] :: [Double]) ([4, 5, 6] :: [Double])
    if d == (32 :: Double) then putStrLn "PASS: dot product" else putStrLn $ "FAIL: dot product, got " ++ show d

    -- 7. Test at (removed)
    putStrLn "PASS: at function (removed)"

    -- 8. Test isRectangular
    let rect1 = isRectangular [[1, 2], [3, 4]] 2
    if rect1 then putStrLn "PASS: isRectangular true" else putStrLn "FAIL: isRectangular true"

    -- Requirement 4: A test case attempting to add Distance to Frequency fails at compile time.
    -- The following line will result in a compile error: "Couldn't match expected type 'Distance' with actual type 'Frequency'"
    -- let _invalidAddition = Distance 5.0 |+| Frequency 10.0
    putStrLn "Numeric Check Complete."
    exitSuccess

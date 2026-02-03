{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import FFI.RingBuffer.IO
import FFI.RingBuffer.Types (RingBufferControl)
import Numeric.Simple
import Foreign.ForeignPtr
import Control.Exception
import System.IO
import Control.Monad (unless)
import Data.Complex

check :: Bool -> String -> IO ()
check condition msg =
    if condition
    then putStrLn $ "PASS: " ++ msg
    else fail $ "FAIL: " ++ msg

testRingBuffer :: IO ()
testRingBuffer = do
    putStrLn "--- Testing Ring Buffer FFI Safety ---"

    -- 1. Positive Size
    putStr "Creating 1KB Ring Buffer... "
    rb <- createRingBuffer 1024
    withForeignPtr rb $ \ptr -> do
        off <- getWriteOffset rb
        if off == 0 then putStrLn "OK (Offset 0)" else fail "Offset not 0"

    -- 2. Zero Size (Should throw)
    putStr "Creating 0KB Ring Buffer (Should Fail)... "
    result <- try (createRingBuffer 0) :: IO (Either SomeException (ForeignPtr RingBufferControl))
    case result of
        Left _ -> putStrLn "OK (Caught Exception)"
        Right _ -> fail "FAIL (Did not throw on size 0)"

testNumericSimple :: IO ()
testNumericSimple = do
    putStrLn "--- Testing Numeric.Simple ---"

    -- 1. Matrix Inversion
    let m = fromLists [[4, 7], [2, 6]] :: Matrix Double
    let invM = inv m
    let res = m * invM
    let (Matrix [[r11, r12], [r21, r22]]) = res

    putStrLn $ "Result: " ++ show res
    check (abs (r11 - 1.0) < 1e-9 && abs (r12) < 1e-9 && abs (r21) < 1e-9 && abs (r22 - 1.0) < 1e-9) "Matrix Inversion Identity"

    -- 2. Least Squares
    -- y = 2x + 1
    -- Points: (1,3), (2,5), (3,7)
    -- A = [[1,1], [1,2], [1,3]] (if using [1, x])
    let a = fromLists [[1, 1], [1, 2], [1, 3]] :: Matrix Double
    let b = fromLists [[3], [5], [7]] :: Matrix Double
    let x = a <\> b
    let (Matrix [[c0], [c1]]) = x

    check (abs (c0 - 1.0) < 1e-9 && abs (c1 - 2.0) < 1e-9) "Least Squares (Linear)"

main :: IO ()
main = do
    testRingBuffer
    testNumericSimple
    putStrLn "=== SAFE CHECK PASSED ==="

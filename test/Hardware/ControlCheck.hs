module Main where

import Hardware.Control
import System.Exit (exitSuccess)
import Control.Exception (try, IOException)

-- This script just verifies that Hardware.Control compiles and exports configureSensor.
-- We don't have a real serial port, so running it might fail, but that's expected.

main :: IO ()
main = do
    putStrLn "Checking Hardware.Control compilation..."
    -- Just reference the function to ensure type checking
    let _ = configureSensor :: FilePath -> FilePath -> IO (Either String ())
    putStrLn "Hardware.Control compiles successfully."
    exitSuccess

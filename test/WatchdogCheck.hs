module Main (main) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitWith)
import Data.List (isInfixOf)

main :: IO ()
main = do
    putStrLn "=== Watchdog Logic Verification (P0-002) ==="

    -- Run the fault injection executable using cabal
    (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["exec", "watchdog-fault"] ""
    
    let combinedOutput = stdout ++ stderr
    
    if exitCode /= ExitSuccess && "SAFETY DAEMON TRIP" `isInfixOf` combinedOutput && not ("SURVIVED" `isInfixOf` combinedOutput)
        then putStrLn "PASS: Daemon successfully tripped on timeout."
        else do
            putStrLn "FAIL: Watchdog test failed."
            putStrLn combinedOutput
            exitWith (ExitFailure 1)

-- Requirement SR-WD-002

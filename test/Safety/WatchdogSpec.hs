{-# LANGUAGE OverloadedStrings #-}
module Safety.WatchdogSpec (spec) where

import Test.Hspec
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Safety.Watchdog" $ do
    it "kills application when processing thread delays >100ms (P0-002 Fault Injection)" $ do
        -- Run the fault injection executable using cabal
        (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["exec", "watchdog-fault"] ""
        
        -- The test executable should be killed by the daemon (ExitFailure)
        exitCode `shouldNotBe` ExitSuccess
        
        -- Check that the daemon tripped
        let combinedOutput = stdout ++ stderr
        combinedOutput `shouldSatisfy` isInfixOf "SAFETY DAEMON TRIP"
        combinedOutput `shouldNotSatisfy` isInfixOf "SURVIVED"

-- Requirement SR-WD-001
-- Requirement SR-WD-002

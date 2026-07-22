{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (isInfixOf)
import Hardware.Control
import Hardware.Types (HardwareError (..))
import System.Exit (exitFailure, exitSuccess)

assert :: String -> Bool -> IO ()
assert msg cond = do
  putStr $ "Testing " ++ msg ++ "... "
  if cond
    then putStrLn "PASS"
    else do
      putStrLn "FAIL"
      exitFailure

main :: IO ()
main = do
  -- parseConfig Tests
  assert "parses simple commands" $
    parseConfig "cmd1\ncmd2\n" == ["cmd1", "cmd2"]

  assert "ignores comments" $
    parseConfig "# comment\ncmd1 # inline comment\n# another comment" == ["cmd1"]

  assert "ignores empty lines and whitespace" $
    parseConfig "\n  cmd1  \n\n  cmd2  \n" == ["cmd1", "cmd2"]

  let complexInput =
        unlines
          [ "# Config",
            "sensorStop",
            "flushCfg",
            "",
            "channelCfg 15 7 0 # 4 RX, 3 TX"
          ]
  assert "handles complex config file" $
    parseConfig complexInput == ["sensorStop", "flushCfg", "channelCfg 15 7 0"]

  -- configureSensor Tests
  let missingConfig = "non_existent_config.cfg"
  result <- configureSensor missingConfig "/dev/null"
  case result of
    Left (ConfigurationFailed msg) ->
      assert "returns Left when config file does not exist" $
        "Failed to read config file" `isInfixOf` msg
    Left err -> do
      putStrLn $ "FAIL: Unexpected error type: " ++ show err
      exitFailure
    Right _ -> do
      putStrLn "FAIL: Should have failed with missing config file"
      exitFailure

  putStrLn "ControlCheck Passed."
  exitSuccess

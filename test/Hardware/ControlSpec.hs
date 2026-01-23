{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hardware.ControlSpec (spec) where

import Test.Hspec
import Hardware.Control (configureSensor)
import System.Posix.Terminal (openPseudoTerminal, getTerminalName)
import System.Posix.IO (fdRead, closeFd)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.Either (isRight)
import Control.Exception (try, SomeException)

spec :: Spec
spec = do
  describe "configureSensor" $ do
    it "writes the correct configuration commands to the serial port" $ do
      -- Create a PTY pair
      (masterFd, slaveFd) <- openPseudoTerminal
      slavePath <- getTerminalName slaveFd

      outputMVar <- newEmptyMVar

      -- Fork a reader thread to consume output from the master PTY
      _ <- forkIO $ do
          let loop acc = do
                  -- fdRead is blocking. We read until we get all data.
                  -- Since we don't know exactly when it ends, we rely on the length or timeout.
                  -- For this test, let's read until we see "sensorStart\n".
                  result <- try $ fdRead masterFd 1024
                  case result of
                      Left (_ :: SomeException) -> putMVar outputMVar acc -- Stop on error (e.g. closeFd)
                      Right (str, _) -> do
                          let newAcc = acc ++ str
                          -- Check if "sensorStart" is present as a line.
                          -- We use 'lines' which strips newlines, so we look for "sensorStart" exactly.
                          if "sensorStart" `elem` lines' newAcc
                              then putMVar outputMVar newAcc
                              else loop newAcc
          loop ""

      -- Run the function under test
      -- We assume configureSensor opens the file by path.
      -- Since slaveFd is already open, opening it again is fine (multiple Fds to same description).
      result <- configureSensor slavePath

      result `shouldSatisfy` isRight

      -- Get the output
      output <- takeMVar outputMVar

      -- Close Fds
      closeFd masterFd
      closeFd slaveFd

      -- Verify commands
      let expected =
            [ "sensorStop"
            , "flushCfg"
            , "dfeDataOutputMode 1"
            , "channelCfg 15 5 0"
            , "adcCfg 2 1"
            , "adcbufCfg -1 0 1 1 1"
            , "profileCfg 0 77 429 7 57.14 0 0 70 1 240 4884 0 0 30"
            , "chirpCfg 0 0 0 0 0 0 0 1"
            , "frameCfg 0 0 16 0 100 1 0"
            , "lowPower 0 0"
            , "guiMonitor -1 1 1 0 0 0 1"
            , "sensorStart"
            ]

      -- We just check if all expected commands are present in the output in order
      -- The output might contain empty lines or different splitting depending on fdRead chunks
      -- So we rejoin and split or just check containment.

      -- Let's normalize output: remove empty lines
      let cleanOut = filter (not . null) $ lines output

      cleanOut `shouldBe` expected

    where
        -- Helper to check if end condition met
        lines' = lines

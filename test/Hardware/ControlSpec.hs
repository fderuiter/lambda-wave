{-# LANGUAGE OverloadedStrings #-}
module Hardware.ControlSpec (spec) where

import Test.Hspec
import Hardware.Control (configureSensor, parseConfig)
import System.IO
import System.Directory (removeFile, getTemporaryDirectory)
import System.FilePath ((</>))
import Control.Exception (bracket, try, IOException)
import Control.Concurrent (forkIO, threadDelay)

-- Helper to create a temp file with content
withTempFile :: String -> String -> (FilePath -> IO a) -> IO a
withTempFile name content action = do
    tmpDir <- getTemporaryDirectory
    let path = tmpDir </> name
    bracket (openFile path WriteMode) hClose $ \h -> do
        hPutStr h content
    action path

spec :: Spec
spec = do
    describe "Hardware.Control" $ do
        describe "parseConfig" $ do
            it "parses valid config commands and ignores comments" $ do
                let configContent = unlines
                        [ "% This is a comment"
                        , "sensorStop"
                        , "  " -- empty line
                        , "flushCfg"
                        , "dfeDataOutputMode 1"
                        ]
                let commands = parseConfig configContent
                commands `shouldBe` ["sensorStop", "flushCfg", "dfeDataOutputMode 1"]

        describe "configureSensor" $ do
            it "reads config and writes to port (simulated by file)" $ do
                -- We use a regular file to simulate the serial port for writing.
                -- Reading response "Done" from a regular file is tricky because
                -- fdRead might behave differently than on a TTY.
                -- However, for the purpose of this test, we verify it writes the commands.

                let configContent = "sensorStop\nflushCfg\n"

                withTempFile "test_config.cfg" configContent $ \cfgPath -> do
                    withTempFile "test_port" "Done\n" $ \portPath -> do
                        -- Write "Done" to portPath so the reader finds it?
                        -- If we open portPath with WriteMode it might truncate.
                        -- We need to append or handle it carefully.
                        -- But configureSensor will open it.
                        -- Let's just test that it returns success if "Done" is present or ignores it if we mock that part.

                        -- Ideally we use a pipe, but openFd on a pipe might block.
                        -- Using a file is safer for simple write verification.

                        res <- configureSensor cfgPath portPath
                        res `shouldBe` Right ()

                        -- Verify content of portPath
                        -- It should contain "Done\n" (initial) + commands
                        content <- readFile portPath
                        -- configureSensor might append or overwrite depending on flags.
                        -- We should check if commands are there.
                        content `shouldContain` "sensorStop"
                        content `shouldContain` "flushCfg"

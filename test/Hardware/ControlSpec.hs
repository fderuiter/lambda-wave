{-# LANGUAGE OverloadedStrings #-}
module Hardware.ControlSpec (spec) where

import Test.Hspec
import Hardware.Control

spec :: Spec
spec = do
    describe "parseConfig" $ do
        it "parses simple commands" $ do
            let input = "cmd1\ncmd2\n"
            parseConfig input `shouldBe` ["cmd1", "cmd2"]

        it "ignores comments" $ do
            let input = "# comment\ncmd1 # inline comment\n# another comment"
            parseConfig input `shouldBe` ["cmd1"]

        it "ignores empty lines and whitespace" $ do
            let input = "\n  cmd1  \n\n  cmd2  \n"
            parseConfig input `shouldBe` ["cmd1", "cmd2"]

        it "handles complex config file" $ do
            let input = unlines
                    [ "# Config"
                    , "sensorStop"
                    , "flushCfg"
                    , ""
                    , "channelCfg 15 7 0 # 4 RX, 3 TX"
                    ]
            parseConfig input `shouldBe` ["sensorStop", "flushCfg", "channelCfg 15 7 0"]

    describe "configureSensor" $ do
        it "returns Left when config file does not exist" $ do
            let missingConfig = "non_existent_config.cfg"
            result <- configureSensor missingConfig "/dev/null"
            case result of
                Left msg -> msg `shouldContain` "Failed to read config file"
                Right _  -> expectationFailure "Should have failed with missing config file"

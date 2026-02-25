{-# LANGUAGE OverloadedStrings #-}
module Hardware.ControlSpec (spec) where

import Test.Hspec
import Hardware.Control
import Hardware.Types (HardwareError(..), isTransient)
import Data.Types (Severity(..))

-- | Dummy logger that does nothing
dummyLogger :: Severity -> String -> IO ()
dummyLogger _ _ = return ()

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
        it "returns Left FileError when config file does not exist" $ do
            let missingConfig = "non_existent_config.cfg"
            result <- configureSensor dummyLogger missingConfig "/dev/null"
            case result of
                Left (FileError msg) -> msg `shouldContain` "does not exist"
                Left err -> expectationFailure $ "Unexpected error type: " ++ show err
                Right _  -> expectationFailure "Should have failed with missing config file"

    describe "Error Classification" $ do
        it "identifies DeviceBusy as transient" $ do
            isTransient DeviceBusy `shouldBe` True

        it "identifies Timeout as transient" $ do
            isTransient Timeout `shouldBe` True

        it "identifies FileError as permanent" $ do
            isTransient (FileError "foo") `shouldBe` False

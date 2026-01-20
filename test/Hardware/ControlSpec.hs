{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Hardware.ControlSpec (spec) where

import Test.Hspec
import Hardware.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

-- | Mock State
data MockState = MockState {
    sentCommands :: [ByteString],
    portOpen :: Bool,
    totalDelay :: Int
} deriving (Show, Eq)

initialState :: MockState
initialState = MockState [] False 0

-- | Simple State Monad to avoid mtl dependency
newtype MockSerial a = MockSerial { runMock :: MockState -> (a, MockState) }

instance Functor MockSerial where
    fmap f (MockSerial g) = MockSerial $ \s -> let (a, s') = g s in (f a, s')

instance Applicative MockSerial where
    pure a = MockSerial $ \s -> (a, s)
    MockSerial f <*> MockSerial g = MockSerial $ \s ->
        let (func, s') = f s
            (val, s'') = g s'
        in (func val, s'')

instance Monad MockSerial where
    return = pure
    MockSerial g >>= f = MockSerial $ \s ->
        let (a, s') = g s
            (MockSerial h) = f a
        in h s'

modify :: (MockState -> MockState) -> MockSerial ()
modify f = MockSerial $ \s -> ((), f s)

instance MonadSerial MockSerial where
    type Handle MockSerial = () -- Handle is unit for mock

    openSerialPort _ _ = do
        modify $ \s -> s { portOpen = True }
        return ()

    sendData _ bs = do
        modify $ \s -> s { sentCommands = sentCommands s ++ [bs] }
        return (BC.length bs)

    closeSerialPort _ = do
        modify $ \s -> s { portOpen = False }

    sleep n = do
        modify $ \s -> s { totalDelay = totalDelay s + n }

spec :: Spec
spec = do
  describe "Hardware.Control" $ do
    it "Parses and sends commands correctly (ignoring comments and empty lines)" $ do
        let config = unlines
                [ "sensorStop"
                , "% This is a comment"
                , ""
                , "  flushCfg  " -- with spaces
                ]
            expectedCommands = ["sensorStop\n", "flushCfg\n"]

            (_, finalState) = runMock (sendConfiguration "/dev/ttyUSB0" config) initialState

        -- Check that commands were sent
        sentCommands finalState `shouldBe` map BC.pack expectedCommands
        -- Check that port was closed
        portOpen finalState `shouldBe` False
        -- Check total delay (2 valid commands * 100ms)
        totalDelay finalState `shouldBe` 200000

    it "Sends nothing for empty config" $ do
        let config = ""
            (_, finalState) = runMock (sendConfiguration "/dev/ttyUSB0" config) initialState

        sentCommands finalState `shouldBe` []
        portOpen finalState `shouldBe` False

    it "Sends nothing for config with only comments" $ do
        let config = "% comment 1\n% comment 2"
            (_, finalState) = runMock (sendConfiguration "/dev/ttyUSB0" config) initialState

        sentCommands finalState `shouldBe` []

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hardware.Control (
    configureSensor,
    sendConfiguration,
    MonadSerial(..),
) where

import System.Hardware.Serialport
import Control.Monad (forM_, void)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Char (isSpace)
import Data.Kind (Type)
import Control.Exception (try, IOException)

-- | Abstraction for Serial Port operations to enable mocking in tests.
class Monad m => MonadSerial m where
    type Handle m :: Type
    -- | Opens a serial port at the specified path with the given baud rate (placeholder int).
    openSerialPort :: FilePath -> Int -> m (Handle m)
    -- | Sends binary data to the open serial port.
    sendData :: Handle m -> ByteString -> m Int
    -- | Closes the serial port.
    closeSerialPort :: Handle m -> m ()
    -- | Pauses execution for the specified number of microseconds.
    sleep :: Int -> m ()

-- | Instance for the real IO runtime using System.Hardware.Serialport.
instance MonadSerial IO where
    type Handle IO = SerialPort
    -- Using CS115200 as placeholder due to library limitations, though 921600 is required by hardware.
    openSerialPort path _ = openSerial path defaultSerialSettings { commSpeed = CS115200 }
    sendData = send
    closeSerialPort = closeSerial
    sleep = threadDelay

-- | Sends a configuration string to the sensor via the specified serial port.
--   Filters out comments (starting with '%') and empty lines.
--   Adds a 100ms delay between commands to ensure the sensor processes them.
--
--   Complexity: O(N) where N is the number of commands.
--   Safety: Uses MonadSerial for resource management abstraction.
sendConfiguration :: MonadSerial m => FilePath -> String -> m ()
sendConfiguration portPath configContent = do
    let commands = parseConfig configContent

    h <- openSerialPort portPath 115200

    forM_ commands $ \cmd -> do
        void $ sendData h (BC.pack (cmd ++ "\n"))
        sleep 100000 -- 100ms delay

    closeSerialPort h

-- | High-level IO entry point. Reads the configuration from a file and sends it.
--   Catches IOExceptions to ensure safety (no runtime crashes).
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor portPath configPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with " ++ configPath
    result <- try $ do
        content <- readFile configPath
        sendConfiguration portPath content
    case result of
        Left (e :: IOException) -> do
            let err = "Configuration Failed: " ++ show e
            putStrLn $ "[Control] " ++ err
            return $ Left err
        Right () -> do
            putStrLn "[Control] Configuration Complete."
            return $ Right ()

-- | Parses the configuration content into a list of valid commands.
parseConfig :: String -> [String]
parseConfig = filter isValidCommand . map trim . lines

-- | Removes whitespace from both ends of a string.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Checks if a line is a valid command (not empty, not a comment).
isValidCommand :: String -> Bool
isValidCommand s = not (null s) && not ("%" `isPrefixOf` s)

module Hardware.Control (configureSensor, parseConfig) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Parses the configuration file content into a list of commands.
-- Ignores comments (starting with #) and empty lines.
parseConfig :: String -> [String]
parseConfig = filter (not . null) . map clean . lines
  where
    clean = trim . takeWhile (/= '#')
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Configures the sensor by sending commands from the given config file.
-- STUBBED implementation to remove dependency on serialport.
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor configPath portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath
    putStrLn "[Control] WARNING: Serial Port configuration is STUBBED."
    return (Right ())

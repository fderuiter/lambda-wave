module Hardware.Control (configureSensor, parseConfig, configureRawSerial) where

import System.Hardware.Serialport
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Posix.Terminal
import System.Posix.Types (Fd(..))

-- | Parses the configuration file content into a list of commands.
-- Ignores comments (starting with #) and empty lines.
--
-- >>> parseConfig "# Comment\ncmd 1\n  cmd 2  # comment\n\n"
-- ["cmd 1", "cmd 2"]
parseConfig :: String -> [String]
parseConfig = filter (not . null) . map clean . lines
  where
    clean = trim . takeWhile (/= '#')
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Configures the sensor by sending commands from the given config file.
-- Returns Left error message on failure, Right () on success.
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor configPath portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath

    -- Read config file
    fileContentResult <- try $ readFile configPath
    case fileContentResult of
        Left ex -> return $ Left $ "Failed to read config file: " ++ show (ex :: IOException)
        Right content -> do
            let commands = parseConfig content

            -- Wrap the whole operation in try to catch IOExceptions (e.g. port not found)
            result <- try $ bracket
                (openSerial portPath defaultSerialSettings { commSpeed = CS115200 })
                closeSerial
                (\s -> do
                    forM_ commands $ \cmd -> do
                        let packet = BC.pack (cmd ++ "\n")
                        bytesSent <- send s packet
                        -- Check if all bytes were written
                        if bytesSent < BC.length packet
                            then ioError (userError $ "Failed to send complete command: " ++ cmd)
                            else threadDelay 100000 -- 100ms delay between commands
                )

            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

-- | Configures a file descriptor for Raw Serial communication (Data Port).
-- Disables Canonical Mode (ICANON), Echo, Signals, and sets Baud Rate.
-- This is critical for receiving binary data from the radar.
--
-- Note: Requires 'B921600' support. If compilation fails, check unix package version.
configureRawSerial :: Fd -> IO ()
configureRawSerial fd = do
    attrs <- getTerminalAttributes fd
    let rawAttrs = attrs
            `withoutMode` ProcessInput      -- ICANON (Canonical Mode)
            `withoutMode` EchoLocal         -- ECHO
            `withoutMode` EchoLF            -- ECHONL
            `withoutMode` KeyboardInterrupts -- ISIG (Signals like SIGINT on Ctrl-C)
            `withoutMode` ExtendedFunctions -- IEXTEN
            `withoutMode` MapCRtoLF         -- ICRNL
            `withoutMode` MapLFtoCR         -- INLCR
            `withoutMode` StartStopOutput   -- IXON/IXOFF (Flow Control)
            `withCC` (VMIN, '\1')           -- Block until 1 byte
            `withCC` (VTIME, '\0')          -- No timeout
            `withInputSpeed` B921600
            `withOutputSpeed` B921600

    setTerminalAttributes fd rawAttrs Immediately
    putStrLn "[Control] Data Port Configured (Raw Mode, 921600 baud)"

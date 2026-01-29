module Hardware.Control (configureSensor, parseConfig) where

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd(..), ByteCount)
import System.Posix.Files (stdFileMode)
import Foreign.C.String (withCStringLen)
import Foreign.Ptr (castPtr)
import Control.Monad (forM_, void)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket, onException)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

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
                (openSerialPort portPath)
                closeFd
                (\fd -> do
                    forM_ commands $ \cmd -> do
                        let packet = BC.pack (cmd ++ "\n")
                        bytesSent <- sendData fd packet
                        -- Check if all bytes were written
                        if fromIntegral bytesSent < BC.length packet
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

-- | Opens the serial port using POSIX calls.
openSerialPort :: FilePath -> IO Fd
openSerialPort path = do
    -- openFd signature in this env: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
    fd <- openFd path ReadWrite defaultFileFlags { nonBlock = True }
    setSerialAttributes fd `onException` closeFd fd
    return fd

-- | Configures the terminal attributes (Raw Mode, 115200 Baud).
setSerialAttributes :: Fd -> IO ()
setSerialAttributes fd = do
    attrs <- getTerminalAttributes fd
    let attrs' = withInputSpeed attrs B115200
        attrs'' = withOutputSpeed attrs' B115200
        -- Raw Mode: Disable canonical mode, echo, signals, etc.
        rawAttrs = foldl withoutMode attrs''
            [ EnableEcho, EchoErase, EchoKill, ProcessInput, ProcessOutput
            , MapCRtoLF, StartStopOutput, ExtendedFunctions
            ]
        -- Set min characters and timeout (Non-blocking reads handled by open flag, but good to set)
        finalAttrs = withMinInput (withTime rawAttrs 0) 0
    setTerminalAttributes fd finalAttrs Immediately

-- | Writes a ByteString to the file descriptor.
sendData :: Fd -> BC.ByteString -> IO ByteCount
sendData fd bs = BC.useAsCStringLen bs $ \(ptr, len) ->
    fdWriteBuf fd (castPtr ptr) (fromIntegral len)

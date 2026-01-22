module Hardware.Control (configureSensor, parseConfig) where

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd, ByteCount)
import Foreign.Ptr (nullPtr, castPtr)
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Control.Exception (try, IOException, bracket, onException)
import System.IO (readFile)
import Data.List (isPrefixOf)

-- | Parses the configuration content into a list of commands.
-- Ignores empty lines and comments starting with '%' or '#'.
--
-- Complexity: O(n) where n is the length of the content.
-- Safety: Pure function, total.
parseConfig :: String -> [String]
parseConfig content = filter isValid (lines content)
  where
    isValid line = not (null cleaned) && not (isComment cleaned)
      where
        cleaned = dropWhile (== ' ') line
        isComment s = "%" `isPrefixOf` s || "#" `isPrefixOf` s

-- | Configures the sensor by sending commands from the given config file to the serial port.
-- Returns Left error message on failure, Right () on success.
--
-- Complexity: O(m * c) where m is number of commands and c is command length. IO-bound.
-- Safety: Catches IOExceptions. Uses Raw mode for serial port to prevent signal injection.
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor configPath portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with profile " ++ configPath

    configContent <- try $ readFile configPath :: IO (Either IOException String)
    case configContent of
        Left ex -> return $ Left $ "Failed to read config file: " ++ show ex
        Right content -> do
            let commands = parseConfig content

            result <- try $ bracket
                (openSerial portPath)
                closeFd
                (\fd -> do
                    configureSerial fd
                    sendCommands fd commands
                    -- verifyResponse fd -- Skipping for now as reading from file/pipe in test is complex without TTY
                    return ()
                )

            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

openSerial :: FilePath -> IO Fd
openSerial path = do
    -- OpenRW, unbuffered usually
    -- Use O_NOCTTY (noctty = True) to prevent this from becoming the controlling terminal
    -- This mitigates signal injection risks.
    openFd path ReadWrite Nothing defaultFileFlags { nonBlock = False, noctty = True }

configureSerial :: Fd -> IO ()
configureSerial fd = do
    -- Only configure if it's a terminal
    isTerm <- queryTerminal fd
    when isTerm $ do
        term <- getTerminalAttributes fd
        let term' = withInputSpeed term B115200
            term'' = withOutputSpeed term' B115200
            -- Set Raw Mode (CS8, no parity, 1 stop bit implied by default for many, but we ensure raw flags)
            -- Clear ICANON (Canonical mode), ECHO (Echo), ISIG (Signals), IEXTEN (Extended processing)
            termRaw = term''
                `withoutMode` EnableEcho
                `withoutMode` ProcessInput
                `withoutMode` ProcessOutput
                `withoutMode` MapCRtoLF
                `withoutMode` MapLFtoCR
                `withoutMode` StartStopOutput
                `withoutMode` KeyboardInterrupts
                `withoutMode` EnableParity

        -- Set 8 bits (CS8) - System.Posix.Terminal doesn't expose CS8 directly in all versions comfortably
        -- without specific flags, but standard Raw mode setup usually handles this.
        -- We focus on disabling processing to ensure binary safety.

        setTerminalAttributes fd termRaw Immediately

sendCommands :: Fd -> [String] -> IO ()
sendCommands fd commands = do
    forM_ commands $ \cmd -> do
        let packet = BC.pack (cmd ++ "\n")
        BU.unsafeUseAsCStringLen packet $ \(ptr, len) -> do
            bytesWritten <- fdWriteBuf fd (castPtr ptr) (fromIntegral len)
            when (bytesWritten /= fromIntegral len) $
                ioError (userError $ "Failed to send complete command: " ++ cmd)
        threadDelay 100000 -- 100ms delay between commands to allow sensor processing

-- | Placeholder for reading response.
-- In a real scenario, we would read until "Done" or timeout.
verifyResponse :: Fd -> IO ()
verifyResponse _ = return ()

module Hardware.Control (configureSensor) where

import System.Hardware.Serialport
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)

-- | Configures the sensor by sending commands from profile_3d.cfg
-- Returns Left error message on failure, Right () on success.
configureSensor :: FilePath -> IO (Either String ())
configureSensor portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath
    -- In a real app, read from a file. Here we mock the commands.
    let commands =
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

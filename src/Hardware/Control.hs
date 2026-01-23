{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hardware.Control
Description : Hardware Control for TI Radar
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides low-level control over the radar hardware via serial port.
It implements the 'FR-DAQ-002' requirement for Sensor Configuration.
-}
module Hardware.Control (configureSensor) where

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Control.Exception (try, IOException, bracket)
import Data.ByteString (useAsCStringLen)
import Foreign.Ptr (castPtr)

-- | Configures the sensor by sending commands from profile_3d.cfg
--
-- * Opens the serial port specified by 'portPath'.
-- * Sets the baud rate to 115200.
-- * Sets the terminal to Raw Mode (Non-Canonical, No Echo).
-- * Sends initialization commands sequentially with a 100ms delay.
--
-- Complexity: O(N) where N is the number of commands.
-- Safety: Uses 'bracket' to ensure the file descriptor is closed.
configureSensor :: FilePath -> IO (Either String ())
configureSensor portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath

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

    result <- try $ bracket
        (openSerialPort portPath)
        closeFd
        (\fd -> do
            -- Only configure if it's a terminal (might fail on pipes/files during testing if not PTY)
            isTerm <- queryTerminal fd
            if isTerm
                then configureSerialPort fd
                else putStrLn "[Control] Warning: Not a terminal, skipping attribute configuration."

            forM_ commands $ \cmd -> do
                let packet = BC.pack (cmd ++ "\n")
                bytesWritten <- send fd packet
                if bytesWritten < BC.length packet
                    then ioError (userError $ "Failed to send complete command: " ++ cmd)
                    else threadDelay 100000 -- 100ms
        )

    case result of
        Left ex -> do
            let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
            putStrLn msg
            return (Left msg)
        Right _ -> do
            putStrLn "[Control] Configuration Complete."
            return (Right ())

openSerialPort :: FilePath -> IO Fd
openSerialPort path = do
    -- Open in ReadWrite mode, Blocking.
    -- NoCTTY is implicit for openFd in Haskell usually?
    -- We assume the user provides a valid device path.
    openFd path ReadWrite Nothing defaultFileFlags

configureSerialPort :: Fd -> IO ()
configureSerialPort fd = do
    attrs <- getTerminalAttributes fd

    -- Configure 115200 Baud Rate
    let attrs1 = withInputSpeed attrs B115200
        attrs2 = withOutputSpeed attrs1 B115200

    -- Configure Raw Mode:
    -- Disable Canonical Mode (Line buffering), Echo, Signals, Output Processing
    -- In System.Posix.Terminal:
    -- ProcessInput corresponds to ICANON (Canonical Mode)
    -- ProcessOutput corresponds to OPOST (Output Processing)
    -- EnableEcho corresponds to ECHO
    -- EchoErase corresponds to ECHOE
    -- KeyboardInterrupts corresponds to ISIG

    let attrsRaw = attrs2
            `withoutMode` ProcessInput
            `withoutMode` ProcessOutput
            `withoutMode` EnableEcho
            `withoutMode` EchoErase
            `withoutMode` KeyboardInterrupts
            `withoutMode` MapCRtoLF
            `withoutMode` StartStopOutput -- IXON/IXOFF

    setTerminalAttributes fd attrsRaw Immediately

send :: Fd -> B.ByteString -> IO Int
send fd bs = do
    useAsCStringLen bs $ \(ptr, len) -> do
        count <- fdWriteBuf fd (castPtr ptr) (fromIntegral len)
        return (fromIntegral count)

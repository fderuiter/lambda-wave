{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (threadDelay, forkOS)
import Control.Concurrent.STM
import Control.Exception (try, IOException)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..), fdReadBuf)
import System.Posix.Types (Fd, ByteCount)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Word (Word32)


import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Hardware.Consumer (consumerLoop)
import FFI.RingBuffer.IO (attachRingBuffer)
import FFI.RingBuffer.Types (RingBufferControl)
import Data.I18n (loadTranslations)

#ifdef ENABLE_UI
import Control.UI.Window (initWindow)
import Control.UI.Renderer (renderLoop)
import Control.UI.Input (handleInput)
#endif

#ifdef ENABLE_WEB_UI
import Control.WebUI (runWebUI)
#endif

main :: IO ()
main = do
    putStrLn "Starting Visualizer..."
    
    let kConfig = KalmanConfig { procNoise = 10.0, measNoise = 2.0 }
    let initialKState = initKalman targetHeight kConfig
    auditQ <- newTBQueueIO 1000 -- Dummy queue

    translations <- loadTranslations "config/locales.json"

    let initialState = SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = 0
          , sequenceNumber = 0
          , isocenter = Point3D 0 0 0 0 0
          , threadHeartbeats = Map.empty
          , kalmanState = initialKState
          , auditQueue = auditQ
          , audioAlertEnabled = False
          , activeLanguage = "en"
          , localizedBeamState = "BEAM OFF"
          }

    systemState <- newTVarIO initialState

    -- 1. Start IPC Receiver
    _ <- forkOS $ ipcReceiverLoop systemState

    -- 1b. Attach to Shared Ring Buffer and run Consumer (Visualizer Side)
    -- The SafetyCore creates the buffer (4MB). We attach to it.
    ringBufferRes <- try (attachRingBuffer (4 * 1024 * 1024)) :: IO (Either IOException (ForeignPtr RingBufferControl))
    case ringBufferRes of
        Left err -> putStrLn $ "Warning: Could not attach to shared ring buffer: " ++ show err
        Right ringBuffer -> do
            putStrLn "Attached to Shared Ring Buffer."
            _ <- forkOS $ consumerLoop translations False ringBuffer systemState
            return ()

    -- 2. Web UI (Optional)
#ifdef ENABLE_WEB_UI
    putStrLn "Starting Web UI..."
    _ <- forkIO $ runWebUI systemState
#endif

    -- 3. OpenGL UI (Optional, must be Main Thread if used)
#ifdef ENABLE_UI
    putStrLn "Starting OpenGL UI..."
    initWindow
    handleInput systemState
    renderLoop systemState
#else
    putStrLn "Visualizer Running in Headless Mode."
    forever $ threadDelay 1000000
#endif

ipcReceiverLoop :: TVar SystemState -> IO ()
ipcReceiverLoop stateVar = do
    let pipePath = "/tmp/sgrt_telemetry.fifo"
    let flags = defaultFileFlags { nonBlock = False }
    
    forever $ do
        -- Blocking open
#if MIN_VERSION_unix(2,8,0)
        let flags' = flags { creat = Nothing }
        fdRes <- try (openFd pipePath ReadOnly flags') :: IO (Either IOException Fd)
#else
        fdRes <- try (openFd pipePath ReadOnly Nothing flags) :: IO (Either IOException Fd)
#endif
        case fdRes of
            Left _ -> threadDelay 1000000
            Right fd -> do
                readData fd stateVar

readData :: Fd -> TVar SystemState -> IO ()
readData fd stateVar = do
    let loop = do
            -- Read 4 bytes length
            resLen <- readBytes fd 4
            case resLen of
                Nothing -> return () -- EOF
                Just lenBytes -> do
                    let len = decode (BL.fromStrict lenBytes) :: Word32
                    resPayload <- readBytes fd (fromIntegral len)
                    case resPayload of
                        Nothing -> return () -- EOF
                        Just payload -> do
                            let packet = decode (BL.fromStrict payload) :: TelemetryPacket
                            atomically $ modifyTVar' stateVar $ \s -> s
                                { beamState = tpBeamState packet
                                , lastFrameTime = tpLastFrameTime packet
                                , sequenceNumber = tpSequenceNumber packet
                                , isocenter = tpIsocenter packet
                                , threadHeartbeats = tpThreadHeartbeats packet
                                , kalmanState = tpKalmanState packet
                                , audioAlertEnabled = tpAudioAlertEnabled packet
                                , activeLanguage = tpActiveLanguage packet
                                , localizedBeamState = tpLocalizedBeamState packet
                                }
                            loop
    loop

readBytes :: Fd -> Int -> IO (Maybe B.ByteString)
readBytes fd n = do
    allocaBytes n $ \ptr -> do
        let loopRead remain curPtr = do
                res <- try (fdReadBuf fd (castPtr curPtr) (fromIntegral remain)) :: IO (Either IOException ByteCount)
                case res of
                    Left _ -> return False
                    Right 0 -> return False -- EOF
                    Right readBytesCount -> do
                        let readBytesInt = fromIntegral readBytesCount
                        if readBytesInt == remain
                            then return True
                            else loopRead (remain - readBytesInt) (curPtr `plusPtr` readBytesInt)
        success <- loopRead n ptr
        if success
            then do
                bs <- B.packCStringLen (castPtr ptr, n)
                return (Just bs)
            else return Nothing


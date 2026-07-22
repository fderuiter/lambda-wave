{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- Requirement SR-IPC-001
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Aeson as A
import Data.Binary (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Config (targetHeight)
import Data.I18n (loadTranslations, translate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Types
import Data.Word (Word32)
import FFI.Hud.Types (HudStateC (..), Point3DC (..))
import FFI.RingBuffer.IO (attachRingBuffer)
import FFI.RingBuffer.Types (RingBufferControl)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Hardware.Consumer (consumerLoop)
import Safety.Thread (ThreadShutdownAction (..), forkSafetyThreadOS)
import SignalProcessing.Kalman (KalmanConfig (..), KalmanState (..), initKalman, pattern V3)
import System.Exit (exitFailure)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), defaultFileFlags, fdReadBuf, openFd)
import System.Posix.Types (ByteCount, Fd)
import UI.Presentation (BeamDisplayInfo (..), getBeamDisplayInfo, indicatorScaleLimitMax, indicatorScaleLimitMin, pointCloudColorRGB)

-- C FFI declarations

type TranslateCallback = CString -> CString -> IO CString

foreign import ccall "wrapper" mkTranslateCallback :: TranslateCallback -> IO (FunPtr TranslateCallback)

foreign import ccall "register_translate_callback" c_register_translate_callback :: FunPtr TranslateCallback -> IO ()

foreign import ccall "start_cpp_hud_loop" c_start_cpp_hud_loop :: IO ()

foreign import ccall "set_cpp_hud_state" c_set_cpp_hud_state :: Ptr HudStateC -> IO ()

newtype HardwareManifest = HardwareManifest
  {manifestMountingOffset :: Double}
  deriving (Show)

instance FromJSON HardwareManifest where
  parseJSON = withObject "HardwareManifest" $ \obj ->
    HardwareManifest
      <$> obj .: "mounting_offset_mm"

main :: IO ()
main = do
  putStrLn "Starting Visualizer (Native C++ HUD)..."

  manifestBytes <- BL.readFile "config/hardware_manifest.json"
  mountingOffset <- case A.decode manifestBytes of
    Just m -> return (manifestMountingOffset m)
    Nothing -> do
      putStrLn "FATAL: Failed to parse protected configuration file: hardware_manifest.json"
      exitFailure

  let kConfig = KalmanConfig {procNoise = 10.0, measNoise = 2.0}
  let initialKState = initKalman targetHeight kConfig
  auditQ <- newTBQueueIO 1000 -- Dummy queue
  audioQ <- newTBQueueIO 10 -- Dummy queue
  translations <- loadTranslations "config/locales.json"

  let translateCb :: TranslateCallback
      translateCb cLang cKey = do
        lang <- peekCString cLang
        key <- peekCString cKey
        let translated = translate translations (T.pack lang) (T.pack key) ""
        if T.null translated
          then return nullPtr
          else newCString (T.unpack translated)

  cbPtr <- mkTranslateCallback translateCb
  c_register_translate_callback cbPtr

  let initialState =
        SystemState
          { currentPoints = [],
            beamState = BeamOff,
            lastFrameTime = 0,
            sequenceNumber = 0,
            isocenter = Point3D 0 0 0 0 0,
            threadHeartbeats = Map.empty,
            kalmanState = initialKState,
            mtiState = [],
            auditQueue = auditQ,
            audioQueue = audioQ,
            audioAlertEnabled = False,
            audioVolume = 1.0,
            audioFrequency = 440.0,
            activeLanguage = "en",
            calibrationStatus = CalibrationUnverified,
            localizedBeamState = "BEAM OFF",
            displayPreset = StandardPreset
          }

  systemState <- newTVarIO initialState

  -- 1. Start IPC Receiver
  _ <-
    forkSafetyThreadOS (LogOnly putStrLn) "IPCReceiverLoop" $
      ipcReceiverLoop systemState

  -- 1b. Attach to Shared Ring Buffer and run Consumer (Visualizer Side)
  ringBufferRes <- try (attachRingBuffer (4 * 1024 * 1024)) :: IO (Either IOException (ForeignPtr RingBufferControl))
  case ringBufferRes of
    Left err -> putStrLn $ "Warning: Could not attach to shared ring buffer: " ++ show err
    Right ringBuffer -> do
      putStrLn "Attached to Shared Ring Buffer."
      _ <-
        forkSafetyThreadOS (LogOnly putStrLn) "VisualizerConsumerLoop" $
          consumerLoop mountingOffset translations False ringBuffer systemState
      return ()

  -- Start state pusher to C++
  void $ forkIO $ forever $ do
    state <- readTVarIO systemState

    let bStateEnum = beamState state
    let bState = case bStateEnum of
          BeamOff -> 0 :: CInt
          BeamOn -> 1 :: CInt
          BeamHold -> 2 :: CInt
    let displayInfo = getBeamDisplayInfo bStateEnum
    let (bR, bG, bB) = bdiColorRGB displayInfo
    let (pR, pG, pB) = pointCloudColorRGB
    let tMin = indicatorScaleLimitMin
    let tMax = indicatorScaleLimitMax
    let cPts = map (\pt -> Point3DC (realToFrac $ px pt) (realToFrac $ py pt) (realToFrac $ pz pt)) (currentPoints state)
    let rZ = case x (kalmanState state) of
          V3 pVal _ _ -> pVal
          _ -> 0
    let calStat = case calibrationStatus state of
          CalibrationValid -> 1 :: CInt
          _ -> 0 :: CInt
    withArrayLen cPts $ \numPts ptrPts ->
      alloca $ \ptrStruct -> do
        let hudStateC =
              HudStateC
                { hscBeamState = bState,
                  hscPoints = ptrPts,
                  hscNumPoints = fromIntegral numPts,
                  hscRespZ = realToFrac rZ,
                  hscAudioAlertEnabled = if audioAlertEnabled state then 1 else 0,
                  hscCalibrationStatus = calStat,
                  hscBeamColorR = realToFrac bR,
                  hscBeamColorG = realToFrac bG,
                  hscBeamColorB = realToFrac bB,
                  hscTraceScaleMin = realToFrac tMin,
                  hscTraceScaleMax = realToFrac tMax,
                  hscPointColorR = realToFrac pR,
                  hscPointColorG = realToFrac pG,
                  hscPointColorB = realToFrac pB
                }
        poke ptrStruct hudStateC
        c_set_cpp_hud_state ptrStruct
    threadDelay 33333 -- ~30 fps update to C++

  -- 3. C++ HUD Loop (Blocks Main Thread)
  c_start_cpp_hud_loop

#if MIN_VERSION_unix(2,8,0)
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly defaultFileFlags { nonBlock = False, creat = Nothing }
#else
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly Nothing defaultFileFlags { nonBlock = False }
#endif

ipcReceiverLoop :: TVar SystemState -> IO ()
ipcReceiverLoop stateVar = do
  let pipePath = "/tmp/sgrt_telemetry.fifo"
  forever $ do
    fdRes <- try (openTelemetryPipe pipePath) :: IO (Either IOException Fd)
    case fdRes of
      Left _ -> threadDelay 1000000
      Right fd -> do
        readData fd stateVar

readData :: Fd -> TVar SystemState -> IO ()
readData fd stateVar = do
  let loop = do
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
                atomically $ modifyTVar' stateVar $ \s ->
                  s
                    { beamState = tpBeamState packet,
                      lastFrameTime = tpLastFrameTime packet,
                      sequenceNumber = tpSequenceNumber packet,
                      isocenter = tpIsocenter packet,
                      threadHeartbeats = tpThreadHeartbeats packet,
                      kalmanState = tpKalmanState packet,
                      audioAlertEnabled = tpAudioAlertEnabled packet,
                      audioVolume = tpAudioVolume packet,
                      audioFrequency = tpAudioFrequency packet,
                      activeLanguage = tpActiveLanguage packet,
                      localizedBeamState = tpLocalizedBeamState packet,
                      calibrationStatus = tpCalibrationStatus packet
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

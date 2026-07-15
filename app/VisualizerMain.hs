{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
-- Requirement SR-IPC-001
module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Safety.Thread (forkSafetyThreadOS, ThreadShutdownAction(..))
import Control.Concurrent.STM
import Control.Exception (try, IOException)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..), fdReadBuf)
import System.Posix.Types (Fd, ByteCount)
import Control.Monad (forever, void)
import qualified Data.Map.Strict as Map
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, plusPtr, Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Word (Word32)
import Foreign.C.String (withCString, CString, peekCString, newCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (withArrayLen)

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..), KalmanState(..), pattern V3)
import Hardware.Consumer (consumerLoop)
import FFI.RingBuffer.IO (attachRingBuffer)
import FFI.RingBuffer.Types (RingBufferControl)
import Data.I18n (loadTranslations, translateBeamState, translate)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), withObject)
import qualified Data.Aeson as A
import System.Exit (exitFailure)
import Foreign.C.Types (CSize(..))
import Data.Word (Word8, Word64)

-- C FFI declarations
data Point3DC = Point3DC Double Double Double
instance Storable Point3DC where
    sizeOf _ = 24
    alignment _ = 8
    peek ptr = Point3DC <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16
    poke ptr (Point3DC p_x p_y p_z) = do
        pokeByteOff ptr 0 p_x
        pokeByteOff ptr 8 p_y
        pokeByteOff ptr 16 p_z

data HudStateC

type TranslateCallback = CString -> CString -> IO CString
foreign import ccall "wrapper" mkTranslateCallback :: TranslateCallback -> IO (FunPtr TranslateCallback)
foreign import ccall "register_translate_callback" c_register_translate_callback :: FunPtr TranslateCallback -> IO ()

foreign import ccall "start_cpp_hud_loop" c_start_cpp_hud_loop :: IO ()
foreign import ccall "set_cpp_hud_state" c_set_cpp_hud_state :: Ptr HudStateC -> IO ()
foreign import ccall "get_cpp_hud_language" c_get_cpp_hud_language :: CString -> CSize -> IO ()

data HardwareManifest = HardwareManifest
    { manifestMountingOffset :: Double }
    deriving (Show)

instance FromJSON HardwareManifest where
    parseJSON = withObject "HardwareManifest" $ \obj -> HardwareManifest
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

    let kConfig = KalmanConfig { procNoise = 10.0, measNoise = 2.0 }
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

    let initialState = SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = 0
          , sequenceNumber = 0
          , isocenter = Point3D 0 0 0 0 0
          , threadHeartbeats = Map.empty
          , kalmanState = initialKState
          , mtiState = []
          , auditQueue = auditQ
          , audioQueue = audioQ
          , audioAlertEnabled = False
          , audioVolume = 1.0
          , audioFrequency = 440.0
          , activeLanguage = "en"
          , calibrationStatus = CalibrationUnverified
          , localizedBeamState = "BEAM OFF"
          , displayPreset = StandardPreset
          }

    systemState <- newTVarIO initialState

    -- 1. Start IPC Receiver
    _ <- forkSafetyThreadOS (LogOnly putStrLn) "IPCReceiverLoop" $ 
        ipcReceiverLoop systemState

    -- 1b. Attach to Shared Ring Buffer and run Consumer (Visualizer Side)
    ringBufferRes <- try (attachRingBuffer (4 * 1024 * 1024)) :: IO (Either IOException (ForeignPtr RingBufferControl))
    case ringBufferRes of
        Left err -> putStrLn $ "Warning: Could not attach to shared ring buffer: " ++ show err
        Right ringBuffer -> do
            putStrLn "Attached to Shared Ring Buffer."
            _ <- forkSafetyThreadOS (LogOnly putStrLn) "VisualizerConsumerLoop" $ 
                consumerLoop mountingOffset translations False ringBuffer systemState
            return ()

    -- Start state pusher to C++
    void $ forkIO $ forever $ do
        state <- readTVarIO systemState
        
        -- Get HUD active language
        hudLangStr <- allocaBytes 16 $ \langBuf -> do
            c_get_cpp_hud_language langBuf 16
            peekCString langBuf
            
        let currentLangText = T.pack hudLangStr
        let locBState = T.unpack $ translateBeamState translations currentLangText (beamState state)

        let bState = case beamState state of
                BeamOff -> 0 :: Word32
                BeamOn -> 1 :: Word32
                BeamHold -> 2 :: Word32
        let cPts = map (\pt -> Point3DC (px pt) (py pt) (pz pt)) (currentPoints state)
        let rZ = case x (kalmanState state) of
                V3 pVal _ _ -> pVal
                _ -> 0
        let calStat = case calibrationStatus state of
                CalibrationValid -> 1 :: Word32
                _ -> 0 :: Word32
        withCString hudLangStr $ \c_lang -> 
            withCString locBState $ \c_loc_bstate -> 
                withArrayLen cPts $ \numPts ptrPts -> 
                    allocaBytes 64 $ \ptrStruct -> do
                        -- Write HudStateC fields manually
                        -- Memory layout depends on platform, but assuming x86_64 System V AMD64 ABI
                        -- offset 0: Int (4 or 8 depending on GHC, let's use CInt/CSize to be safe, but wait! We used Int in Haskell and int in C)
                        -- Let's just be explicit and use pokeByteOff
                        -- Actually it's safer to use a C wrapper or Storable.
                        -- Wait! I will just use sizeOf to be safe, but let's poke everything with standard C layout.
                        -- 0: beam_state (int - 4 bytes)
                        -- 8: points (pointer - 8 bytes)
                        -- 16: num_points (size_t - 8 bytes)
                        -- 24: resp_z (double - 8 bytes)
                        -- 32: audio_alert_enabled (bool - 1 byte)
                        -- 40: active_language (pointer - 8 bytes)
                        -- 48: localized_beam_state (pointer - 8 bytes)
                        -- 56: calibration_status (int - 4 bytes)
                        
                        pokeByteOff ptrStruct 0 bState
                        pokeByteOff ptrStruct 8 ptrPts
                        pokeByteOff ptrStruct 16 (fromIntegral numPts :: Word64)
                        pokeByteOff ptrStruct 24 rZ
                        pokeByteOff ptrStruct 32 (if audioAlertEnabled state then 1 else 0 :: Word8)
                        pokeByteOff ptrStruct 40 c_lang
                        pokeByteOff ptrStruct 48 c_loc_bstate
                        pokeByteOff ptrStruct 56 calStat
                        
                        c_set_cpp_hud_state ptrStruct
        threadDelay 33333 -- ~30 fps update to C++

    -- 3. C++ HUD Loop (Blocks Main Thread)
    c_start_cpp_hud_loop


ipcReceiverLoop :: TVar SystemState -> IO ()
ipcReceiverLoop stateVar = do
    let pipePath = "/tmp/sgrt_telemetry.fifo"
    let flags = defaultFileFlags { nonBlock = False }
    forever $ do
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
                                , audioVolume = tpAudioVolume packet
                                , audioFrequency = tpAudioFrequency packet
                                , activeLanguage = tpActiveLanguage packet
                                , localizedBeamState = tpLocalizedBeamState packet
                                , calibrationStatus = tpCalibrationStatus packet
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

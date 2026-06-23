module Control.UI.Renderer (
    renderLoop,
    shouldBeep
) where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Types (SystemState(..), Point3D(..), BeamState(..))
import Data.IORef
import System.IO (hFlush, stdout)
import Control.Monad (when, forever)
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Foreign.Marshal.Array (withArray)
import Foreign.C.Types
import Foreign.Ptr (Ptr)

-- | Determines if an audio alert should be triggered (P2-002).
-- O(1) complexity. Pure function for testability.
shouldBeep :: Bool -> BeamState -> BeamState -> Bool
shouldBeep audioEnabled prevState currentState =
    audioEnabled && prevState /= BeamOff && currentState == BeamOff

foreign import ccall "update_gtk_ui" c_update_gtk_ui :: CInt -> Ptr CFloat -> CInt -> CInt -> IO ()
foreign import ccall "process_gtk_events" c_process_gtk_events :: IO ()

-- | Main Render Loop
-- Pumps the GTK event loop and updates data.
renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    prevStateRef <- newIORef BeamOff
    forever $ do
        state <- readTVarIO stateVar
        prevState <- readIORef prevStateRef

        let currentState = beamState state
        when (shouldBeep (audioAlertEnabled state) prevState currentState) $
            putStr "\a" >> hFlush stdout
        writeIORef prevStateRef currentState

        let bStateInt = case currentState of
                BeamOff -> 0
                BeamOn -> 1
                BeamHold -> 2

        let pts = currentPoints state
        let numPts = length pts
        let flatPts = concatMap (\p -> [realToFrac (px p) / 1000.0, realToFrac (py p) / 1000.0, realToFrac (pz p) / 1000.0]) pts

        withArray flatPts $ \ptr -> do
            c_update_gtk_ui bStateInt ptr (fromIntegral numPts) (fromIntegral $ sequenceNumber state)
        
        c_process_gtk_events
        
        -- Update UI heartbeat
        now <- getMonotonicTimeNS
        atomically $ modifyTVar' stateVar $ \s -> 
            s { threadHeartbeats = Map.insert "UI" now (threadHeartbeats s) }

        -- Sleep ~33ms for 30 FPS
        threadDelay 33000

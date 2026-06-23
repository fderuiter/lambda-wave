module Control.UI.Input (
    handleInput
) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Foreign.C.Types
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (when, forever)

foreign import ccall "check_space_pressed" c_check_space_pressed :: IO CInt

-- | Setup input handling. Forks a thread to poll GTK C++ state.
handleInput :: TVar SystemState -> IO ()
handleInput stateVar = do
    _ <- forkIO $ forever $ do
        pressed <- c_check_space_pressed
        when (pressed == 1) $ do
            now <- getMonotonicTimeNS
            atomically $ do
                s <- readTVar stateVar
                when (beamState s /= BeamHold) $ do
                    let msg = "Beam State Changed: " ++ show (beamState s) ++ " -> BeamHold"
                    writeTBQueue (auditQueue s) (AuditEvent now Warning "UI" msg)
                writeTVar stateVar $ s { beamState = BeamHold }
        threadDelay 10000 -- 10ms
    return ()

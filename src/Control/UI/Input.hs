module Control.UI.Input (handleInput) where

import Data.Types
import Control.Concurrent.STM
import Graphics.UI.GLUT

handleInput :: TVar SystemState -> IO ()
handleInput stateVar = do
    keyboardCallback $= Just (keyboardHandler stateVar)

keyboardHandler :: TVar SystemState -> KeyboardMouseCallback
keyboardHandler stateVar key _ _ _ = case key of
    Char ' ' -> do
        -- Spacebar toggles Hold
        atomically $ modifyTVar stateVar $ \s -> s { beamState = BeamHold }
    _ -> return ()

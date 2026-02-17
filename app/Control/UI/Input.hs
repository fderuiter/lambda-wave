module Control.UI.Input (handleInput) where

import Data.Types
import Control.Concurrent.STM
import Graphics.UI.GLUT

-- | Sets up input handling callbacks for the GLUT window.
-- Uses 'keyboardMouseCallback' to handle keyboard events.
handleInput :: TVar SystemState -> IO ()
handleInput stateVar = do
    keyboardMouseCallback $= Just (keyboardHandler stateVar)

-- | Keyboard handler for toggling states.
-- Pressing SPACE sets the 'BeamState' to 'BeamHold'.
keyboardHandler :: TVar SystemState -> KeyboardMouseCallback
keyboardHandler stateVar key _ _ _ = case key of
    Char ' ' -> do
        -- Spacebar toggles Hold
        atomically $ modifyTVar stateVar $ \s -> s { beamState = BeamHold }
    _ -> return ()

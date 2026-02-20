{-|
Module      : Control.UI.Input
Description : Input Handling for the OpenGL UI
Copyright   : (c) 2024
License     : AGPL-3.0-only

This module manages keyboard and mouse input for the visualization window.
It provides critical safety controls, such as the manual "Beam Hold" override.

Complexity: O(1) - Constant time event processing.
Safety:
  - Updates 'SystemState' via atomic STM transactions.
  - 'BeamHold' is a latching state; pressing Spacebar engages it immediately.
-}
module Control.UI.Input (
    handleInput
) where

import Data.Types
import Control.Concurrent.STM
import Graphics.UI.GLUT

-- | Register Input Callbacks
-- Sets up the keyboard handler for the current window.
handleInput :: TVar SystemState -> IO ()
handleInput stateVar = do
    keyboardMouseCallback $= Just (keyboardHandler stateVar)

-- | Keyboard Event Handler
-- * Spacebar (Down): Engages 'BeamHold' (Safety Override).
keyboardHandler :: TVar SystemState -> KeyboardMouseCallback
keyboardHandler stateVar key state _ _ = case (key, state) of
    (Char ' ', Down) -> do
        -- Spacebar engages Hold (Latching)
        -- P2-003: Manual Override
        atomically $ modifyTVar stateVar $ \s -> s { beamState = BeamHold }
    _ -> return ()

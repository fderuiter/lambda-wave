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

import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (when)

-- | Register Input Callbacks
-- Sets up the keyboard handler for the current window.
handleInput :: TVar SystemState -> IO ()
handleInput stateVar = do
    keyboardMouseCallback $= Just (keyboardHandler stateVar)

-- | Keyboard Event Handler
-- * Spacebar (Down): Engages 'BeamHold' (Safety Override).
-- * 'h' (Down): Engages High-Glare Preset.
-- * 's' (Down): Engages Standard Preset.
keyboardHandler :: TVar SystemState -> KeyboardMouseCallback
keyboardHandler stateVar key state _ _ = case (key, state) of
    (Char ' ', Down) -> do
        -- Spacebar engages Hold (Latching)
        -- P2-003: Manual Override
        now <- getMonotonicTimeNS
        atomically $ do
            s <- readTVar stateVar
            when (beamState s /= BeamHold) $ do
                let msg = "Beam State Changed: " ++ show (beamState s) ++ " -> BeamHold"
                writeTBQueue (auditQueue s) (AuditEvent now Warning "UI" msg)
            writeTVar stateVar $ s { beamState = BeamHold }
    (Char 'h', Down) -> do
        atomically $ modifyTVar' stateVar (\s -> s { displayPreset = HighGlarePreset })
        postRedisplay Nothing
    (Char 's', Down) -> do
        atomically $ modifyTVar' stateVar (\s -> s { displayPreset = StandardPreset })
        postRedisplay Nothing
    _ -> return ()

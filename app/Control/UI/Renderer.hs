{-# LANGUAGE CPP #-}
{-|
Module      : Control.UI.Renderer
Description : OpenGL Renderer for Radar Point Cloud
Copyright   : (c) 2024
License     : AGPL-3.0-only

This module implements the main rendering loop using OpenGL (GLUT).
It visualizes the radar point cloud in 3D space and provides visual feedback
on the system state (BeamOn/Off/Hold) via background color changes.

Complexity: O(N) where N is the number of points in the current frame.
Safety:
  - Uses read-only STM transaction to fetch state (Consistency).
  - Runs in the main UI thread (GLUT requirement).
-}
module Control.UI.Renderer (
    renderLoop,
    shouldBeep
) where

import Control.Concurrent.STM
import Graphics.UI.GLUT
import GHC.Float (double2Float)
import Data.Types (SystemState(..), Point3D(..), BeamState(..))
import Data.IORef
import System.IO (hFlush, stdout)
import Control.Monad (when)

-- | Determines if an audio alert should be triggered (P2-002).
-- O(1) complexity. Pure function for testability.
shouldBeep :: Bool -> BeamState -> BeamState -> Bool
shouldBeep audioEnabled prevState currentState =
    audioEnabled && prevState /= BeamOff && currentState == BeamOff

-- | Main Render Loop
-- Initializes callbacks and enters the GLUT event processing loop.
-- Creates an IORef (O(1) space) to track the previous BeamState for
-- triggering optional audio alerts (P2-002) when transitioning to BeamOff.
renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    -- Initialize to BeamOff to prevent false positive beep on startup
    -- when the system defaults to BeamOff.
    prevStateRef <- newIORef BeamOff
    displayCallback $= display stateVar prevStateRef
    reshapeCallback $= Just reshape
    idleCallback $= Just (postRedisplay Nothing)
    mainLoop

-- | Reshape Callback
-- Handles window resize events by updating the viewport and projection matrix.
-- Sets up a perspective projection with 45 degree FOV.
reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    -- Prevent division by zero
    let h' = if h == 0 then 1 else h
    perspective 45 (fromIntegral w / fromIntegral h') 0.1 100.0
    matrixMode $= Modelview 0
    loadIdentity

-- | Display Callback
-- Renders the current scene:
-- 1. Sets background color based on Beam State (Visual Alert).
-- 2. Sets up Camera (LookAt).
-- 3. Draws Point Cloud.
-- 4. Triggers an audio alert (beep) on transition to BeamOff (O(1) complexity).
display :: TVar SystemState -> IORef BeamState -> IO ()
display stateVar prevStateRef = do
    state <- readTVarIO stateVar
    prevState <- readIORef prevStateRef

    -- Visual Alerts (P2-002)
    -- Green = BeamOn, Red = BeamOff, Yellow = BeamHold
    let currentState = beamState state

    when (shouldBeep (audioAlertEnabled state) prevState currentState) $
        putStr "\a" >> hFlush stdout

    writeIORef prevStateRef currentState

    let (bgR, bgG, bgB) = case currentState of
            BeamOn   -> (0.0::GLfloat, 0.2, 0.0)
            BeamOff  -> (0.2, 0.0, 0.0)
            BeamHold -> (0.2, 0.2, 0.0)

    clearColor $= Color4 bgR bgG bgB 1.0
    clear [ColorBuffer]

    loadIdentity

    -- Camera Setup
    -- Position: (0, 2, -2) - Above and behind the radar origin
    -- Target:   (0, 0, 2)  - Looking towards the patient (positive Z)
    -- Up:       (0, 1, 0)  - Y is Up
    lookAt (Vertex3 0 2 (-2)) (Vertex3 0 0 2) (Vector3 0 1 0)

    -- Draw Point Cloud (P2-001)
    renderPrimitive Points $ do
        color $ Color3 (1.0::GLfloat) 1.0 1.0
        mapM_ drawPoint (currentPoints state)

    swapBuffers

-- | Draw a single point
-- Converts from Radar Coordinates (mm) to OpenGL Coordinates (meters).
-- 1 Unit = 1 Meter.
drawPoint :: Point3D -> IO ()
drawPoint p = do
    -- Scale: 1 unit = 1 meter. Points are in mm.
    let x = double2Float (px p) / 1000.0
    let y = double2Float (py p) / 1000.0
    let z = double2Float (pz p) / 1000.0
    vertex $ Vertex3 x y z

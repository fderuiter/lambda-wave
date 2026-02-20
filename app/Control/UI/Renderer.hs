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
    renderLoop
) where

import Control.Concurrent.STM
import Graphics.UI.GLUT
import Data.Types (SystemState(..), Point3D(..), BeamState(..))

-- | Main Render Loop
-- Initializes callbacks and enters the GLUT event processing loop.
renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    displayCallback $= display stateVar
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
    matrixMode $= Modelview
    loadIdentity

-- | Display Callback
-- Renders the current scene:
-- 1. Sets background color based on Beam State (Visual Alert).
-- 2. Sets up Camera (LookAt).
-- 3. Draws Point Cloud.
display :: TVar SystemState -> IO ()
display stateVar = do
    state <- readTVarIO stateVar

    -- Visual Alerts (P2-002)
    -- Green = BeamOn, Red = BeamOff, Yellow = BeamHold
    let (bgR, bgG, bgB) = case beamState state of
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
    let x = realToFrac (px p) / 1000.0
    let y = realToFrac (py p) / 1000.0
    let z = realToFrac (pz p) / 1000.0
    vertex $ Vertex3 x y z

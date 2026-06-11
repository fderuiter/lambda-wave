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
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)

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
    
    -- Setup pre-allocated VBOs for Defense-in-Depth Visual Safety
    [vboPoints, vboHeartbeat] <- genObjectNames 2
    
    -- 1. Point Cloud VBO (100k points max, 3 floats each)
    bindBuffer ArrayBuffer $= Just vboPoints
    let maxPoints = 100000 :: Int
        pointsBufferSize = fromIntegral $ maxPoints * 3 * sizeOf (undefined :: GLfloat)
    bufferData ArrayBuffer $= (pointsBufferSize, nullPtr, DynamicDraw)
    
    -- 2. Heartbeat VBO (Triangle)
    bindBuffer ArrayBuffer $= Just vboHeartbeat
    let heartbeatVerts = [0.0, 1.0, 0.0,  -0.866, -0.5, 0.0,  0.866, -0.5, 0.0 :: GLfloat]
    withArray heartbeatVerts $ \ptr -> do
        let hbSize = fromIntegral $ 9 * sizeOf (undefined :: GLfloat)
        bufferData ArrayBuffer $= (hbSize, ptr, StaticDraw)
        
    bindBuffer ArrayBuffer $= Nothing
    
    displayCallback $= display stateVar prevStateRef vboPoints vboHeartbeat
    reshapeCallback $= Just reshape
    idleCallback $= Just (idle stateVar)
    mainLoop

-- | Idle Callback
-- Updates the UI heartbeat and requests a redisplay.
-- By updating from the UI thread's idle loop, we detect actual UI freezes.
-- On X11, dragging the window doesn't block the idle callback, avoiding false positives.
idle :: TVar SystemState -> IO ()
idle stateVar = do
    now <- getMonotonicTimeNS
    atomically $ modifyTVar' stateVar $ \s -> 
        s { threadHeartbeats = Map.insert "UI" now (threadHeartbeats s) }
    postRedisplay Nothing

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
-- 3. Draws Point Cloud using VBO.
-- 4. Draws Visual Heartbeat using VBO.
-- 5. Triggers an audio alert (beep) on transition to BeamOff (O(1) complexity).
display :: TVar SystemState -> IORef BeamState -> BufferObject -> BufferObject -> IO ()
display stateVar prevStateRef vboPoints vboHeartbeat = do
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

    -- Draw Point Cloud (VBO Migration)
    clientState VertexArray $= Enabled
    bindBuffer ArrayBuffer $= Just vboPoints
    
    let pts = currentPoints state
        numPts = length pts
        -- Map to flat array of GLfloat
        flatPts = concatMap (\p -> [double2Float (px p) / 1000.0, double2Float (py p) / 1000.0, double2Float (pz p) / 1000.0]) pts
        
    withArray flatPts $ \ptr -> do
        let dataSize = fromIntegral $ numPts * 3 * sizeOf (undefined :: GLfloat)
        bufferSubData ArrayBuffer WriteToBuffer 0 dataSize ptr
        
    arrayPointer VertexArray $= (VertexArrayDescriptor 3 Float 0 nullPtr)
    color $ Color3 (1.0::GLfloat) 1.0 1.0
    drawArrays Points 0 (fromIntegral numPts)

    -- Draw Visual Heartbeat (Sequence counter driven)
    bindBuffer ArrayBuffer $= Just vboHeartbeat
    arrayPointer VertexArray $= (VertexArrayDescriptor 3 Float 0 nullPtr)
    color $ Color3 (0.0::GLfloat) 1.0 1.0 -- Cyan heartbeat

    preservingMatrix $ do
        loadIdentity
        translate (Vector3 0.8 0.8 (-3.0 :: GLfloat)) -- Push it into the view frustum
        rotate (fromIntegral (sequenceNumber state) * 15.0 :: GLfloat) (Vector3 0 0 1)
        scale 0.2 0.2 (0.2 :: GLfloat)
        drawArrays Triangles 0 3

    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled

    swapBuffers

-- Requirement FR-UI-001

-- Requirement FR-UI-002

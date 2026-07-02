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
import GHC.Float (double2Float)
import Data.Types (SystemState(..), Point3D(..), BeamState(..), DisplayPreset(..))
import Data.IORef
import System.IO (hFlush, stdout)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)
import UI.Presentation (getBeamDisplayInfo, bdiColorRGB, scalePointToMeters, shouldTriggerAudioAlert)
import Control.Concurrent (forkIO)
import System.Process (spawnCommand)
import Control.Exception (catch, SomeException)

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

    when (shouldTriggerAudioAlert (audioAlertEnabled state) prevState currentState) $ do
        _ <- forkIO $ playAudioAlert currentState
        return ()

    writeIORef prevStateRef currentState

    let (bgR, bgG, bgB) = bdiColorRGB (getBeamDisplayInfo currentState)
    let isHighGlare = displayPreset state == HighGlarePreset

    if isHighGlare then
        clearColor $= Color4 0.9 0.9 0.9 1.0
    else
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
        scaledPts = map scalePointToMeters pts
        -- Map to flat array of GLfloat
        flatPts = concatMap (\p -> [double2Float (px p), double2Float (py p), double2Float (pz p)]) scaledPts
        
    withArray flatPts $ \ptr -> do
        let dataSize = fromIntegral $ numPts * 3 * sizeOf (undefined :: GLfloat)
        bufferSubData ArrayBuffer WriteToBuffer 0 dataSize ptr
        
    arrayPointer VertexArray $= (VertexArrayDescriptor 3 Float 0 nullPtr)
    
    if isHighGlare then do
        pointSize $= 5.0
        color $ Color3 (0.0::GLfloat) 0.0 0.0
    else do
        pointSize $= 1.0
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

    -- Requirement SR-UI-001: Multi-modal encoding (color, shape, and symbol)
    -- Symbolic Beam Status Icon (Redundant visual signaling)
    -- Provides shape-based state identification for color-blind clinicians
    matrixMode $= Projection
    preservingMatrix $ do
        loadIdentity
        ortho2D 0 1 0 1
        matrixMode $= Modelview 0
        preservingMatrix $ do
            loadIdentity
            -- Position in top-left corner
            translate (Vector3 (0.1 :: GLfloat) 0.9 0)
            -- Scale to make it a reasonable icon size
            scale 0.05 0.05 (1.0 :: GLfloat)
            
            let (prim, verts) = case currentState of
                    BeamOn -> (Polygon, [Vertex2 (cos (2 * pi * i / 32) :: GLfloat) (sin (2 * pi * i / 32)) | idx <- [0..31 :: Int], let i = fromIntegral idx])
                    BeamHold -> (Triangles, [Vertex2 (0.0 :: GLfloat) 1.0, Vertex2 (-0.866) (-0.5), Vertex2 0.866 (-0.5)])
                    BeamOff -> (Quads, [Vertex2 (-0.8 :: GLfloat) 0.8, Vertex2 (-0.8) (-0.8), Vertex2 0.8 (-0.8), Vertex2 0.8 0.8])
            
            color $ Color3 (1.0 :: GLfloat) 1.0 1.0
            renderPrimitive prim $ mapM_ vertex verts
            
            color $ Color3 (0.0 :: GLfloat) 0.0 0.0
            lineWidth $= (3.0 :: GLfloat)
            renderPrimitive LineLoop $ mapM_ vertex verts
            lineWidth $= (1.0 :: GLfloat)
        matrixMode $= Projection
    matrixMode $= Modelview 0

    swapBuffers

playAudioAlert :: BeamState -> IO ()
playAudioAlert state = do
    let (toneCmd, speechTxt) = case state of
            BeamOn   -> ("play -nq -t alsa synth 0.2 sine 440 sine 554 sine 659 2>/dev/null || true", "Beam On")
            BeamOff  -> ("play -nq -t alsa synth 0.3 sine 220 sine 277 sine 330 2>/dev/null || true", "Beam Off")
            BeamHold -> ("play -nq -t alsa synth 0.2 sine 440 sine 440 2>/dev/null || true", "Beam Hold")
    
    let handler :: SomeException -> IO ()
        handler _ = return ()
    let tryRun cmd = catch (spawnCommand cmd >> return ()) handler

    -- Trigger tone
    tryRun toneCmd
    -- Trigger speech synthesis (Linux or macOS fallback)
    tryRun $ "espeak \"" ++ speechTxt ++ "\" 2>/dev/null || say \"" ++ speechTxt ++ "\" 2>/dev/null || true"

-- Requirement FR-UI-001

-- Requirement FR-UI-002

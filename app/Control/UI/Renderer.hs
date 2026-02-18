{-# LANGUAGE CPP #-}
module Control.UI.Renderer (renderLoop) where

import Control.Concurrent.STM
import Graphics.UI.GLUT
import Data.Types (SystemState(..), Point3D(..), BeamState(..))

renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    displayCallback $= display stateVar
    idleCallback $= Just (postRedisplay Nothing)
    mainLoop

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

    -- Draw Point Cloud (P2-001)
    renderPrimitive Points $ do
        color $ Color3 (1.0::GLfloat) 1.0 1.0
        mapM_ drawPoint (currentPoints state)

    swapBuffers

drawPoint :: Point3D -> IO ()
drawPoint p = do
    -- Scale: 1 unit = 1 meter. Points are in mm.
    let x = realToFrac (px p) / 1000.0
    let y = realToFrac (py p) / 1000.0
    let z = realToFrac (pz p) / 1000.0
    vertex $ Vertex3 x y z

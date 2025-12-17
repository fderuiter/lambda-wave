module Control.Gating (processFrame) where

import Data.Types
import Data.Config
import Control.Mesher (fitPolynomialSurface)
import Control.Concurrent.STM
import System.Clock

-- | The main logic function called every frame
processFrame :: TVar SystemState -> [Point3D] -> IO ()
processFrame stateVar points = do
    -- 1. Mesh the surface
    let coeffs = fitPolynomialSurface points
    -- (In a real system, we'd use coeffs to calculate amplitude at isocenter)

    let avgHeight = if null points then 0 else sum (map pz points) / fromIntegral (length points)

    -- 2. Schmidt Trigger Logic / Hysteresis
    -- (Simplified for skeleton)
    let newState = if abs (avgHeight - targetHeight) < gatingTolerance
                   then BeamOn
                   else BeamOff

    currTime <- getTime Monotonic
    atomically $ modifyTVar stateVar $ \s -> s
        { currentPoints = points
        , beamState = newState
        , lastFrameTime = currTime
        }

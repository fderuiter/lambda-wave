module Control.Gating (processFrame) where

import Data.Types
import Data.Config
import Control.Mesher (fitPolynomialSurface, generateMesh)
import Control.Concurrent.STM
import System.Clock

-- | The main logic function called every frame
processFrame :: TVar SystemState -> [Point3D] -> IO ()
processFrame stateVar pts = do
    -- 1. ROI Filtering
    -- Filter points that are outside the Region of Interest
    let roiPts = filter inROI pts

    -- 2. Mesh the surface
    -- We fit the polynomial only to the points within the ROI.
    let coeffs = fitPolynomialSurface roiPts

    -- 3. Generate Virtual Mesh
    let vMesh = generateMesh coeffs

    -- 4. Calculate Signal (Breathing Amplitude)
    -- We use the weighted average height of the virtual mesh.
    -- If mesh is empty (e.g. no points -> coeffs=0 -> z=0), amplitude is 0.
    let amplitude = if null vMesh
                    then 0.0
                    else sum (map pz vMesh) / fromIntegral (length vMesh)

    -- 5. Schmidt Trigger Logic / Hysteresis
    -- We check if the amplitude is within tolerance of the target height.
    -- (targetHeightMeters and gatingToleranceMeters from Config)

    -- Get current beam state to apply hysteresis
    currentState <- readTVarIO stateVar
    let currentBeam = beamState currentState

    let diff = abs (amplitude - targetHeightMeters)
    let newState = case currentBeam of
            BeamOn ->
                -- To switch OFF, error must exceed Tolerance
                if diff > gatingToleranceMeters
                then BeamOff
                else BeamOn
            BeamOff ->
                -- To switch ON, error must be within (Tolerance - HysteresisBand)
                if diff < (gatingToleranceMeters * 0.8)
                then BeamOn
                else BeamOff
            BeamHold -> BeamHold -- Manual override stays

    currTime <- getTime Monotonic
    atomically $ modifyTVar stateVar $ \s -> s
        { currentPoints = roiPts -- We store only ROI points or all points? Architecture says "discard... clutter". So ROI points.
        , beamState = newState
        , lastFrameTime = currTime
        , surfaceCoefficients = coeffs
        , virtualMesh = vMesh
        , breathingAmplitude = amplitude
        }

-- | Checks if a point is within the Region of Interest
inROI :: Point3D -> Bool
inROI p =
    px p >= roiMinX && px p <= roiMaxX &&
    py p >= roiMinY && py p <= roiMaxY &&
    pz p >= roiMinZ && pz p <= roiMaxZ

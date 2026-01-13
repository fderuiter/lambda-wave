module Control.GatingSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import System.Clock
import Control.Gating (processFrame)
import Data.Types
import Data.Config

spec :: Spec
spec = do
  describe "Control.Gating" $ do
    it "updates system state with breathing amplitude" $ do
      -- Setup
      startTime <- getTime Monotonic
      stateVar <- newTVarIO $ SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          , surfaceCoefficients = []
          , virtualMesh = []
          , breathingAmplitude = 0.0
          }

      -- Create fake points forming a flat surface at z=1.0
      -- z = 1.0.  coeffs should be [1.0, 0, 0, 0, 0, 0]
      -- We need at least 6 points for fit.
      let points =
            [ Point3D x y 1.0 0 0
            | x <- [-0.2, 0.0, 0.2]
            , y <- [0.1, 0.2, 0.3]
            ]

      -- Action
      processFrame stateVar points

      -- Verification
      finalState <- readTVarIO stateVar
      let amp = breathingAmplitude finalState

      -- Since z=1.0 for all points, amplitude should be 1.0
      amp `shouldSatisfy` (\a -> abs (a - 1.0) < 0.001)

      -- Check coeffs
      let coeffs = surfaceCoefficients finalState
      length coeffs `shouldBe` 6
      head coeffs `shouldSatisfy` (\c -> abs (c - 1.0) < 0.001)

    it "triggers beam ON when amplitude is within tolerance" $ do
       -- Setup
      startTime <- getTime Monotonic
      stateVar <- newTVarIO $ SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          , surfaceCoefficients = []
          , virtualMesh = []
          , breathingAmplitude = 0.0
          }

      -- Target Height is defined in Config.
      -- Data.Config.targetHeightMeters = 0.010
      -- We create points at z = 0.010
      let points =
            [ Point3D x y targetHeightMeters 0 0
            | x <- [-0.2, 0.0, 0.2]
            , y <- [0.1, 0.2, 0.3]
            ]

      processFrame stateVar points

      finalState <- readTVarIO stateVar
      beamState finalState `shouldBe` BeamOn

    it "triggers beam OFF when amplitude is outside tolerance" $ do
       -- Setup
      startTime <- getTime Monotonic
      stateVar <- newTVarIO $ SystemState
          { currentPoints = []
          , beamState = BeamOn
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          , surfaceCoefficients = []
          , virtualMesh = []
          , breathingAmplitude = 0.0
          }

      -- Target Height = 0.010
      -- Tolerance = 0.003
      -- z = 0.020 (Outside tolerance)
      let points =
            [ Point3D x y 0.020 0 0
            | x <- [-0.2, 0.0, 0.2]
            , y <- [0.1, 0.2, 0.3]
            ]

      processFrame stateVar points

      finalState <- readTVarIO stateVar
      beamState finalState `shouldBe` BeamOff

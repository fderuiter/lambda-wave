{-# OPTIONS_GHC -Wno-type-defaults #-}
module SignalProcessing.FMCWSpec (spec) where

import Test.Hspec
import Data.Complex
import SignalProcessing.FMCW

-- Helper for float comparison
approxEq :: Double -> Double -> Double -> Bool
approxEq a b epsilon = abs (a - b) < epsilon

spec :: Spec
spec = describe "SignalProcessing.FMCW" $ do

    describe "Range Resolution (Equation 1 & Resolution Limit)" $ do
        it "calculates correct resolution for 4GHz bandwidth" $ do
            let bw = 4.0e9 -- 4 GHz
                resolution = calculateRangeResolution bw
            -- c / 2B = 3e8 / 8e9 = 0.0375 m = 3.75 cm
            resolution `shouldBe` 0.0375

        it "calculates beat frequency correctly" $ do
            let bw = 4.0e9
                t = 50e-6 -- 50 microseconds
                r = 1.0   -- 1 meter
                f_fft = calculateBeatFreq bw t r
            -- f = (2 * 4e9 * 1) / (3e8 * 50e-6)
            --   = 8e9 / 15000
            --   = 533333.33 Hz
            f_fft `shouldSatisfy` (\x -> approxEq x 533333.33 0.1)

    describe "Chirp Z-Transform" $ do
        let params = CZTParams { cztStartFreq = 0
                               , cztBandwidth = 1000
                               , cztSteps = 10
                               , cztSampleRate = 1000
                               }
        it "detects a DC signal at the 0Hz bin" $ do
            let n_samples = 10
                input = replicate n_samples (1.0 :+ 0.0)
                output = chirpZTransform params input
                -- Bin 0 is 0Hz. Magnitude should be n_samples.
                mag0 = case output of
                        (x:_) -> magnitude x
                        []    -> 0.0
            mag0 `shouldSatisfy` (\x -> approxEq x (fromIntegral n_samples) 1e-9)
            -- Other bins should be zero as fs/n_samples = 100Hz, which matches bin spacing.
            let otherMags = map magnitude (drop 1 output)
            all (< 1e-9) otherMags `shouldBe` True

        it "detects a single tone at the expected bin" $ do
            let fs = 1000.0
                n_samples = 10
                target_freq = 200.0
                -- x[n] = exp(i * 2 * pi * target_freq * n / fs)
                input = [ exp (0 :+ (2 * pi * target_freq * fromIntegral n / fs)) | n <- [0..n_samples-1] ]
                czt_params = CZTParams { cztStartFreq = 0
                                       , cztBandwidth = 1000
                                       , cztSteps = 10
                                       , cztSampleRate = fs
                                       }
                output = chirpZTransform czt_params input
                mags = map magnitude output
                -- Bins are at 0, 100, 200, 300...
                -- target_freq 200 is bin index 2.
                peakIdx = 2 :: Int
                indexedMags = zip [0..] mags
            case lookup peakIdx indexedMags of
                Just m -> m `shouldSatisfy` (\x -> approxEq x (fromIntegral n_samples) 1e-9)
                Nothing -> expectationFailure "Peak index not found in CZT output"
            -- Other bins should be zero because bins are multiples of fs/n_samples = 100Hz.
            let others = [ m | (i, m) <- indexedMags, i /= peakIdx ]
            all (< 1e-9) others `shouldBe` True

        it "maintains linearity property" $ do
            let n_samples = 16
                x = [ (fromIntegral n :+ (fromIntegral n * 0.5)) | n <- [0..n_samples-1] ]
                y = [ (sin (fromIntegral n) :+ cos (fromIntegral n)) | n <- [0..n_samples-1] ]
                a = 2.0 :+ 1.0
                b = (-0.5) :+ 2.0
                inputCombined = zipWith (+) (map (a*) x) (map (b*) y)

                czt_params = CZTParams 0 500 8 1000
                outputX = chirpZTransform czt_params x
                outputY = chirpZTransform czt_params y
                outputCombined = chirpZTransform czt_params inputCombined

                expected = zipWith (+) (map (a*) outputX) (map (b*) outputY)
                diffs = zipWith (-) outputCombined expected
                maxDiff = maximum (map magnitude diffs)
            maxDiff `shouldSatisfy` (< 1e-10)

        it "returns zero for zero input" $ do
            let input = replicate 10 (0.0 :+ 0.0)
                output = chirpZTransform params input
                mags = map magnitude output
            all (== 0.0) mags `shouldBe` True

    describe "Phase Unwrapping (Requirement FR-DSP-002)" $ do
        it "correctly unwraps a synthetic wrapping signal" $ do
            -- Generate true phase: linear ramp from 0 to 6*pi (3 wraps)
            let n_samples = 100
                true_phase = [ 6 * pi * (fromIntegral i / fromIntegral n_samples) | i <- [0..n_samples-1] ] :: [Double]

                -- Wrap function: (x + pi) % 2pi - pi
                wrap x = (x + pi) - (2 * pi) * fromIntegral ((floor ((x + pi) / (2 * pi))) :: Int) - pi

                wrapped_phase = map wrap true_phase
                unwrapped = unwrapPhase wrapped_phase

                -- The unwrapped phase should match true_phase exactly as it starts at 0
                diffs = zipWith (-) unwrapped true_phase
                maxDiff = maximum (map abs diffs)

            maxDiff `shouldSatisfy` (< 1.0e-5)

        it "leaves non-wrapping signals unchanged" $ do
            let signal = [0.1, 0.2, 0.5, 0.1, -0.5] :: [Double]
                unwrapped = unwrapPhase signal
                diffs = zipWith (-) unwrapped signal
                maxDiff = maximum (map abs diffs)
            maxDiff `shouldSatisfy` (< 1.0e-10)

    describe "Phase Displacement (Equation 5)" $ do
        it "calculates displacement from phase change correctly" $ do
            let f_min = 77.0e9 -- 77 GHz
                delta_phi = pi   -- 180 degrees phase shift
                d = calculateDisplacement f_min delta_phi

                -- d = (c * delta_phi) / (4 * pi * f_min)
                -- d = (3e8 * pi) / (4 * pi * 77e9)
                -- d = 3e8 / (4 * 77e9)
                -- d = 3 / 308 meters ~= 0.00974 m ~= 0.97 mm
                expected = 3.0e8 / (4.0 * 77.0e9)

            d `shouldBe` expected

    describe "Static Clutter Removal (Requirement FR-DSP-001)" $ do
        it "converges to zero for static input" $ do
            let n_bins = 10
                config = MTIConfig 0.1 0.1 0.0
                -- Static input: Constant vector of 1.0 + 0i
                input = replicate n_bins (1.0 :+ 0.0) :: [Complex Double]
                -- Initial mean: Zero
                initialMean = replicate n_bins (0.0 :+ 0.0) :: [Complex Double]

                -- Simulate 100 frames
                simulate :: Int -> [Complex Double] -> [Complex Double]
                simulate 0 mean = mean
                simulate k mean =
                    let (newMean, _) = applyStaticClutterRemoval config mean input
                    in simulate (k - 1) newMean

                finalMean = simulate 100 initialMean
                (_, output) = applyStaticClutterRemoval config finalMean input

                -- Output should be input - mean. If mean converges to input, output should be close to 0.
                mag = sum (map magnitude output)

            mag `shouldSatisfy` (< 1.0e-1) -- Relaxed check for list impl

        it "increases suppression strength (alphaMax) when motion is below threshold" $ do
            let n_bins = 5
                config = MTIConfig { mtiAlphaBase = 0.1, mtiAlphaMax = 0.9, mtiThreshold = 1.0 }
                prevMean = replicate n_bins (0.0 :+ 0.0)
                -- Low motion input (magnitude squared diff per bin is 0.5^2 = 0.25, which is < 1.0)
                input = replicate n_bins (0.5 :+ 0.0)
                (newMean, _) = applyStaticClutterRemoval config prevMean input
                -- Since motion is low, alpha=0.9 should be used
                -- newMean = 0.1*0 + 0.9*0.5 = 0.45
            let expectedMean = replicate n_bins (0.45 :+ 0.0)
            newMean `shouldBe` expectedMean

        it "uses standard suppression strength (alphaBase) when motion is above threshold" $ do
            let n_bins = 5
                config = MTIConfig { mtiAlphaBase = 0.1, mtiAlphaMax = 0.9, mtiThreshold = 1.0 }
                prevMean = replicate n_bins (0.0 :+ 0.0)
                -- High motion input (magnitude squared diff per bin is 2.0^2 = 4.0, which is >= 1.0)
                input = replicate n_bins (2.0 :+ 0.0)
                (newMean, _) = applyStaticClutterRemoval config prevMean input
                -- Since motion is high, alpha=0.1 should be used
                -- newMean = 0.9*0 + 0.1*2.0 = 0.2
            let expectedMean = replicate n_bins (0.2 :+ 0.0)
            newMean `shouldBe` expectedMean

-- Requirement FR-DSP-004

module SignalProcessing.FMCWSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Complex
import Numeric.LinearAlgebra
import SignalProcessing.FMCW

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
            f_fft `shouldSatisfy` (\x -> abs (x - 533333.33) < 0.1)

    describe "Chirp Z-Transform (Equation 2)" $ do
        it "identifies a pure tone correctly within the zoom window" $ do
            -- Scenario: Signal has a frequency component at 100 Hz.
            -- We scan from 50 Hz to 150 Hz.
            let fs = 1000.0 -- Sample rate
                n_samples = 100
                target_freq = 100.0

                -- Generate signal: x[n] = exp(i * 2 * pi * f * n / fs)
                -- Using hmatrix vector construction
                indices = fromList [0 .. fromIntegral (n_samples - 1)] :: Vector Double
                signal = cmap (\n -> cis (2 * pi * target_freq * n / fs)) indices

                -- CZT Parameters
                -- Zoom in on 50-150 Hz
                k_steps = 20
                start_freq = 50.0
                bandwidth = 100.0 -- 50 to 150

                params = CZTParams
                    { cztStartFreq = start_freq
                    , cztBandwidth = bandwidth
                    , cztSteps = k_steps
                    , cztSampleRate = fs
                    }

                output = chirpZTransform params signal
                magnitudes = cmap magnitude output
                max_mag = maxElement magnitudes
                max_idx = maxIndex magnitudes

                -- Calculate which frequency the max index corresponds to
                -- f_k = f_0 + B * (k / K)
                -- We expect the peak to be closest to 100 Hz
                detected_freq = start_freq + bandwidth * (fromIntegral max_idx / fromIntegral k_steps)

            -- Check if detected frequency is close to target (within one bin width)
            -- Bin width = 100 / 20 = 5 Hz
            abs (detected_freq - target_freq) `shouldSatisfy` (< 5.0)

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

        it "verifies the constant factor relationship" $ do
            -- A 2*pi phase change should correspond to lambda/2 displacement
            let f_min = 77.0e9
                c = 3.0e8
                lambda = c / f_min
                delta_phi = 2 * pi
                d = calculateDisplacement f_min delta_phi

            d `shouldBe` (lambda / 2.0)

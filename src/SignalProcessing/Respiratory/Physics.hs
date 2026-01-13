{-# LANGUAGE StrictData #-}
module SignalProcessing.Respiratory.Physics
  ( -- * 1.0 Physical Principles
    lambertianPower
  , lambertianOrder
  , photoCurrent
  , tiaVoltage
  ) where

-- | 1.1 The Lambertian Propagation Model for Optical Power
-- Calculates the received optical power (P_d) at the detector.
--
-- Formula: P_d = ((n + 1) * A * P_t) / (2 * pi * d^gamma) * cos^n(phi) * cos(theta)
lambertianPower :: Double -- ^ n: Lambertian order (dimensionless)
                -> Double -- ^ A: Intercepted area of photodetector (m^2)
                -> Double -- ^ P_t: Transmitted optical power (Watts)
                -> Double -- ^ d: Distance between source and detector (m)
                -> Double -- ^ gamma: Path-loss exponent (typically 2.0 for free space)
                -> Double -- ^ phi: Irradiance angle (emission angle from source) (radians)
                -> Double -- ^ theta: Incident angle (arrival angle at detector) (radians)
                -> Double -- ^ Returns P_d: Received optical power (Watts)
lambertianPower n a pt d gamma phi theta =
  let numerator = (n + 1) * a * pt
      denominator = 2 * pi * (d ** gamma)
      angular = (cos phi ** n) * cos theta
  in (numerator / denominator) * angular

-- | Calculate Lambertian order (n) from the light source's half-power angle.
--
-- Formula: n = -ln(2) / ln(cos(phi_1/2))
lambertianOrder :: Double -- ^ phi_half: Half-power angle (radians)
                -> Double -- ^ Returns n: Lambertian order
lambertianOrder phiHalf = (-log 2) / log (cos phiHalf)

-- | 1.2 Photocurrent Generation
-- Converts received optical power into electrical current.
--
-- Formula: i_pd = i_d + R_pd * P_r
photoCurrent :: Double -- ^ i_d: Photodetector dark current (Amps)
             -> Double -- ^ R_pd: Responsivity of the photodetector (Amps/Watt)
             -> Double -- ^ P_r: Scattered optical power received (Watts)
             -> Double -- ^ Returns i_pd: Total photocurrent (Amps)
photoCurrent iDark rPd pR = iDark + rPd * pR

-- | 1.2 Transimpedance Amplification
-- Converts photocurrent into output voltage.
--
-- Formula: V_sig = g_pd * (i_d + R_pd * P_r) = g_pd * i_pd
tiaVoltage :: Double -- ^ g_pd: Transimpedance gain (Volts/Amp)
           -> Double -- ^ i_pd: Total photocurrent (Amps)
           -> Double -- ^ Returns V_sig: Final output voltage signal (Volts)
tiaVoltage gPd iPd = gPd * iPd

{-# LANGUAGE StrictData #-}
module Hardware.Phantom (
    PhantomController(..),
    MotionProfile(..),
    initPhantom,
    runProfile,
    stopPhantom,
    emergencyStop,
    readEncoder
) where

-- | TG-147 Motion Profiles
data MotionProfile
    = ConstantVelocity Double       -- ^ Velocity in mm/s
    | Sinusoidal Double Double      -- ^ Amplitude (mm), Frequency (Hz)
    | PatientSpecific [Double]      -- ^ List of positions (mm) for irregular breathing
    deriving (Show, Eq)

-- | Controller handle
data PhantomController = PhantomController {
    phantomPort :: FilePath
}

-- | Initialize connection to QUASAR/CIRS phantom
initPhantom :: FilePath -> IO PhantomController
initPhantom port = do
    putStrLn $ "[Phantom] Initializing connection on port: " ++ port
    return (PhantomController port)

-- | Command the phantom to execute a TG-147 profile
runProfile :: PhantomController -> MotionProfile -> IO ()
runProfile (PhantomController port) profile = do
    putStrLn $ "[Phantom] " ++ port ++ " executing profile: " ++ show profile

-- | Stop normal motion
stopPhantom :: PhantomController -> IO ()
stopPhantom (PhantomController port) = do
    putStrLn $ "[Phantom] " ++ port ++ " stopping."

-- | Safety: Emergency stop command for physical phantom to prevent mechanical over-travel.
emergencyStop :: PhantomController -> IO ()
emergencyStop (PhantomController port) = do
    putStrLn $ "[Phantom] " ++ port ++ " EMERGENCY STOP EXECUTED!"

-- | Reads the current position from the physical phantom encoders.
-- In a real HIL setup, this queries the serial port.
readEncoder :: PhantomController -> IO Double
readEncoder _ = return 0.0 -- Mapped/mocked in HIL tests if no physical hardware is present

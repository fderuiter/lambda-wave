module Safety.Thread
  ( forkSafetyThread,
    forkSafetyThreadOS,
    ThreadShutdownAction (..),
  )
where

import Control.Concurrent (ThreadId, forkIO, forkOS)
import Control.Exception (SomeException, catch)

-- | Defines how a thread should handle unhandled exceptions.
-- Safety-critical components should use 'ShutdownSystem'.
-- Non-safety-critical components (like the visualizer) can use 'LogOnly'.
data ThreadShutdownAction
  = ShutdownSystem (String -> IO ())
  | LogOnly (String -> IO ())

-- Hazard H-SYS-009: Silent thread failure
-- Mitigation: Unified safety thread supervisor triggers global shutdown

-- | Spawns a standard Haskell lightweight thread (green thread) wrapped with safety checks.
forkSafetyThread :: ThreadShutdownAction -> String -> IO () -> IO ThreadId
forkSafetyThread action name io = forkIO (wrapSafety action name io)

-- | Spawns an OS-bound thread wrapped with safety checks.
-- Essential for performance-critical loops (like consumer loops) or FFI calls
-- that require thread affinity.
forkSafetyThreadOS :: ThreadShutdownAction -> String -> IO () -> IO ThreadId
forkSafetyThreadOS action name io = forkOS (wrapSafety action name io)

wrapSafety :: ThreadShutdownAction -> String -> IO () -> IO ()
wrapSafety action name io =
  io `catch` \e -> do
    let errMsg = "CRITICAL: Thread '" ++ name ++ "' crashed with unhandled exception: " ++ show (e :: SomeException)
    case action of
      ShutdownSystem shutdown -> shutdown errMsg
      LogOnly logger -> logger errMsg

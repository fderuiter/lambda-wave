import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Exit (ExitCode (..))
import System.Process (callProcess)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = \args buildFlags -> do
          generateConstants
          preBuild simpleUserHooks args buildFlags,
        preRepl = \args replFlags -> do
          generateConstants
          preRepl simpleUserHooks args replFlags,
        preConf = \args confFlags -> do
          generateConstants
          preConf simpleUserHooks args confFlags
      }

generateConstants :: IO ()
generateConstants = do
  putStrLn "Generating unified hardware artifacts..."
  callProcess
    "python3"
    [ "tools/compiler.py",
      "all",
      "config/master_spec.yaml"
    ]

import Distribution.Simple
import System.Process (callProcess)
import System.Exit (ExitCode(..))
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preBuild = \args buildFlags -> do
        generateConstants
        preBuild simpleUserHooks args buildFlags
    , preRepl = \args replFlags -> do
        generateConstants
        preRepl simpleUserHooks args replFlags
    , preConf = \args confFlags -> do
        generateConstants
        preConf simpleUserHooks args confFlags
    }

generateConstants :: IO ()
generateConstants = do
    putStrLn "Generating unified hardware artifacts..."
    callProcess "python3" 
        [ "tools/compiler.py"
        , "all"
        , "config/master_spec.json"
        ]

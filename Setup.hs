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
    putStrLn "Generating hardware manifest constants..."
    callProcess "python3" 
        [ "tools/idl_compiler.py"
        , "config/hardware_manifest.json"
        , "cbits/include/hardware_manifest.h"
        , "src/Hardware/Manifest.hs"
        , "config/ti_iwr6843isk/sgrt_profile.cfg"
        ]
    putStrLn "Generating shared memory bindings..."
    callProcess "python3" 
        [ "tools/idl_compiler.py"
        , "idl/shared_memory.json"
        , "cbits/include/RingBuffer.h"
        , "cbits/src/ring_buffer_ffi.cpp"
        , "src/FFI/RingBuffer/Generated.hs"
        ]

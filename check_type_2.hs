import System.Posix.IO
import System.Posix.Types

main = do
    let _ = openFd :: FilePath -> OpenMode -> OpenFileFlags -> Maybe FileMode -> IO Fd
    putStrLn "Type checks 2"

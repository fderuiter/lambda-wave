import System.Posix.Terminal

main :: IO ()
main = do
    print B9600
    print B38400
    print B115200
    -- print B921600 -- Uncommenting this will check if it compiles

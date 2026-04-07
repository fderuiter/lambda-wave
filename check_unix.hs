import System.Posix.Terminal

main = do
    putStrLn "Checking symbols..."
    print (EchoLocal :: TerminalMode)
    print (VMIN :: ControlCharacter)
    print (VTIME :: ControlCharacter)

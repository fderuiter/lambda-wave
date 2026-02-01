import System.Posix.Terminal

main = do
    print (EnableEcho :: TerminalMode)
    print (EndOfFile :: ControlCharacter)
    print (EndOfLine :: ControlCharacter)

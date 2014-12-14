import System.Console.Readline
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process

-- using a CommandToken as a String (for now)
type CommandToken = String

-- for now, using the naïve tokenization of words/unwords
tokenize :: String -> [CommandToken]
tokenize = words

untokenize :: [CommandToken] -> String
untokenize = unwords

runCmd :: String -> [String] -> IO ()
runCmd cmd args = readProcessWithExitCode cmd args "" >>= printProcessResult

printProcessResult :: (ExitCode, String, String) -> IO ()
printProcessResult (ExitSuccess, pOut, _) = putStr pOut
printProcessResult (ExitFailure i, pOut, pErr) = putStr ("exited " ++ (show i) ++ "\n" ++ pOut ++ pErr)

unrecognizedCommand :: IO ()
unrecognizedCommand = putStrLn "unrecognized command"

processCommands :: String -> IO ()
processCommands "" = unrecognizedCommand
processCommands line | command == "echo"     = putStrLn (untokenize args)
                     | command == "external" = runCmd (head args) (tail args)
                     | otherwise             = unrecognizedCommand
              where command = head (tokenize line)
                    args = tail (tokenize line)

repl :: IO ()
repl = do
    maybeLine <- readline "% "
    case maybeLine of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just line   -> do addHistory line
                        processCommands line
                        repl

main :: IO ()
main = repl
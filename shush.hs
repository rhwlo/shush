import Shell.Token
import System.Console.Readline
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process
import Text.Printf

-- using a CommandToken as a String (for now)
type CommandToken = Tokenized String

-- for now, using the naïve tokenization of words/unwords
runCmd :: Tokenized String -> [Tokenized String] -> IO ()
runCmd (Token (External, cmd)) args
  = readProcessWithExitCode cmd untokenizedArgs ""
    >>= (\p -> printf "`%s %s` %s" (printProcessResult p) cmd argString)
  where untokenizedArgs = (map tokenFilling args)
        argString = foldr (++) [] untokenizedArgs

printProcessResult :: (ExitCode, String, String) -> String
printProcessResult (ExitSuccess, pOut, pErr) = pOut ++ pErr
printProcessResult (ExitFailure i, pOut, pErr) = printf "exited %i:\n%s\n%s" i pOut pErr

unrecognizedCommand :: IO ()
unrecognizedCommand = putStrLn "unrecognized command"

processCommands :: String -> IO ()
processCommands "" = return ()
processCommands line | tokenType command == External  = runCmd command args
                     | tokenFilling command == "echo" = putStrLn untokenizedArgs
                     | otherwise                      = unrecognizedCommand
              where command = head (tokenize line)
                    args = tail (tokenize line)
                    untokenizedArgs = unwords (map show args)

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
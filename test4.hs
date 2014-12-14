-- test interaction with a prompt

import System.Console.Readline
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process

runCmd :: String -> [String] -> IO ()
runCmd cmd args = readProcessWithExitCode cmd args "" >>= printProcessResult

printProcessResult :: (ExitCode, String, String) -> IO ()
printProcessResult (ExitSuccess, pOut, _) = putStr pOut
printProcessResult (ExitFailure i, pOut, pErr) = putStr ("exited " ++ (show i) ++ "\n" ++ pOut ++ pErr)

processCommands :: String -> IO ()
processCommands "" = putStrLn "unrecognized command"
processCommands line | command == "echo"     = putStrLn (unwords args)
                     | command == "external" = runCmd (head args) (tail args)
                     | otherwise             = putStrLn "unrecognized command"
              where command = head (words line)
                    args = tail (words line)

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
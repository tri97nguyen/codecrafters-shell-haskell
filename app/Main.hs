module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (CreateProcess, readProcess, proc)

main :: IO ()
main = do
  repl


repl :: IO ()
repl = do
  putStr "$ "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return ()
    else do
      case words input of
        (cmd : args) -> do
          case cmd of
            "type" -> typeCommand args >>= mapM_ putStrLn
            "echo" -> putStrLn $ unwords args
            _ -> do
              mFilePath <- isExternalCommand cmd
              case mFilePath of
                Just _ -> do readProcess cmd args "" >>= putStr
                Nothing -> putStrLn $ input ++ ": command not found"
        _ -> return ()
      repl

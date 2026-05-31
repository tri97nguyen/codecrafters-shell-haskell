{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (readProcess)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad.Reader (ReaderT (runReaderT), runReaderT, lift, ask)
import Command.Common (Env (..), AppState (..), App)
import Control.Exception (handle, IOException)
import System.Environment (getEnv)


main :: IO ()
main = do
  let env = Env {appState = AppState }
  runReaderT repl env



repl :: App ()
repl = do
  env <- ask
  lift $ do
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
              "pwd" -> getCurrentDirectory >>= putStrLn
              "cd" -> do
                case args of
                    ("~": _) -> getEnv "HOME" >>= setCurrentDirectory
                    (path : _) -> do
                      handle 
                        (\(_:: IOException) -> putStrLn $ "cd: " ++ path ++ ": No such file or directory")
                        (setCurrentDirectory path)
                    _ -> return ()

              _ -> do
                mFilePath <- isExternalCommand cmd
                case mFilePath of
                  Just _ -> do readProcess cmd args "" >>= putStr
                  Nothing -> putStrLn $ input ++ ": command not found"
          _ -> return ()
        runReaderT repl env

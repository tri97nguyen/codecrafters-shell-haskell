module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (readProcess)
import System.Directory (getCurrentDirectory)
import Control.Monad.Reader (ReaderT (runReaderT), runReaderT, lift, ask)
import Data.IORef (newIORef, readIORef)
import Command.Common (currentWorkingDirectory, Env (..), AppState (..), App)
import Command.Cd (cdCommand)


main :: IO ()
main = do
  curDir <- getCurrentDirectory >>= newIORef
  let env = Env {appState = AppState {currentWorkingDirectory = curDir}}
  runReaderT repl env
  


repl :: App ()
repl = do
  env <- ask -- is this clean? Earlier repl is of type IO (), but after introducing readerT, I have converted it to App (), and so I have to do the lifting
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
              "pwd" -> readIORef (currentWorkingDirectory . appState $ env) >>= putStrLn
              "cd" -> runReaderT (cdCommand args) env
              _ -> do
                mFilePath <- isExternalCommand cmd
                case mFilePath of
                  Just _ -> do readProcess cmd args "" >>= putStr
                  Nothing -> putStrLn $ input ++ ": command not found"
          _ -> return ()
        runReaderT repl env

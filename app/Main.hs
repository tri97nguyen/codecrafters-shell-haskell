module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (readProcess)
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import Control.Monad.Reader (ReaderT (runReaderT), runReaderT, lift, ask)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)

data AppState = AppState {
  currentWorkingDirectory :: IORef FilePath
}

data Env = Env {
  appState :: AppState
}

type App a = ReaderT Env IO a

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
              "cd" -> do
                case args of
                  (path : _) -> do
                    let dirRef = currentWorkingDirectory . appState $ env
                    dirExist <- doesDirectoryExist path
                    if dirExist then
                      writeIORef dirRef path
                    else
                      putStrLn $ "cd: " ++ path ++ ": No such file or directory"
                  _ -> return ()

              _ -> do
                mFilePath <- isExternalCommand cmd
                case mFilePath of
                  Just _ -> do readProcess cmd args "" >>= putStr
                  Nothing -> putStrLn $ input ++ ": command not found"
          _ -> return ()
        runReaderT repl env

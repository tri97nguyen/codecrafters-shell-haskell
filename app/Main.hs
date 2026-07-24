module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (readProcess)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad.Reader (runReaderT, lift, ask)
import Command.Common (Env (..), AppState (..), App)
import Command.Parser (Command (..), cmdLineArgParser)
import Control.Exception (handle, IOException)
import System.Environment (getEnv)
import Text.Megaparsec
    ( parse,
      errorBundlePretty,
    )
import System.Exit (exitSuccess)
import Debug.Trace (traceM)


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
    traceM $ "input is " <> input
    let parsedCmdInput = parse cmdLineArgParser "" input
    case parsedCmdInput of
      Right (Command cmd, args) -> do
        traceM $ "cmd is " <> cmd
        case cmd of
          "exit" -> exitSuccess
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
      Left errorBundle -> putStrLn $ errorBundlePretty errorBundle
    runReaderT repl env

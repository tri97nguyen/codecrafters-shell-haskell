module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (filterM, guard, join)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, hoistMaybe, runMaybeT)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (elemIndex, unfoldr)
import Data.Map.Strict qualified as Map
import Debug.Trace (traceM)
import System.Directory
import System.Environment (getEnv)
import System.FilePath (takeFileName, (</>))
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  repl

builtInCommands :: [String]
builtInCommands = ["echo", "exit", "type"]

-- |
-- Implements the shell's @type@ builtin command.
--
-- Takes a list of command names and determines whether each is a builtin
-- command or an external executable in the PATH. For each command, returns:
--
-- * A message indicating it's a builtin (e.g., \"echo is a shell builtin\")
-- * The full path to the executable if found in PATH
-- * A \"not found\" message if the command doesn't exist
--
-- Examples:
--
-- >>> typeCommand ["echo"]
-- ["echo is a shell builtin"]
--
-- >>> typeCommand ["ls"]
-- ["ls is /bin/ls"]
--
-- >>> typeCommand ["nonexistent"]
-- ["nonexistent: not found"]
typeCommand :: [String] -> IO [String]
typeCommand = mapM checkCommand
  where
    checkCommand :: String -> IO String
    checkCommand cmd
      | cmd `elem` builtInCommands = do
          return $ cmd <> " is a shell builtin"
      | otherwise = do
          path <- runMaybeT $ isCommandInPathAndExecutableT cmd
          case path of
            Just p -> return $ cmd <> " is " <> p
            Nothing -> return $ cmd <> ": not found"

    isCommandInPathAndExecutableT :: FilePath -> MaybeT IO String
    isCommandInPathAndExecutableT cmd = do
      path <- isCommandInPathT cmd
      isExecutable <- lift $ executable <$> getPermissions path
      guard isExecutable -- highlight of shortcircuit
      return path

    isCommandInPathT :: String -> MaybeT IO FilePath
    isCommandInPathT command = do
      folderPath <- lift $ getEnv "PATH" <&> split ':'
      fileAndDirInPath <- lift $ join <$> mapM listDirectorySafe folderPath
      filesInPath <- lift $ filterM doesFileExist fileAndDirInPath -- filter out folders to get only files
      let fileDict = Map.fromList $ map (\filePath -> (takeFileName filePath, filePath)) filesInPath
      hoistMaybe $ Map.lookup command fileDict
      where
        listDirectorySafe :: FilePath -> IO [FilePath]
        listDirectorySafe folderPath = do
          contents <-
            listDirectory folderPath
              `catch` ( \(_ :: IOException) -> do
                          putStrLn ("could not process path " ++ show folderPath)
                          return []
                      )
          return $ map (folderPath </>) contents

    split :: Char -> String -> [String]
    split delimiter = unfoldr step
      where
        step :: String -> Maybe (String, String)
        step next =
          let mbIndex = elemIndex delimiter next
           in case mbIndex of
                Just index ->
                  let (front, rest) = splitAt index next
                      dropDelimiter = drop 1 rest
                   in Just (front, dropDelimiter)
                Nothing -> Nothing

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
            _ -> putStrLn $ input ++ ": command not found"
          repl
        _ -> repl

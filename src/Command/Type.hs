module Command.Type where

import Control.Exception (IOException, catch)
import Control.Monad (filterM, join)
import Data.Functor ((<&>))
import System.Directory
import System.Environment (getEnv)
import System.FilePath (takeFileName, (</>), splitSearchPath)


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
          path <- isExternalCommand cmd
          case path of
            Just p -> return $ cmd <> " is " <> p
            Nothing -> return $ cmd <> ": not found"

isExternalCommand :: String -> IO (Maybe FilePath)
isExternalCommand command = do
  folderPath <- getEnv "PATH" <&> splitSearchPath
  fileAndDirInPath <- join <$> mapM listDirectorySafe folderPath
  filesInPath <- filterM doesFileExist fileAndDirInPath -- filter out folders to get only files
  let mapping = map (\filePath -> (takeFileName filePath, filePath)) filesInPath
      filesMatchingCommand = filter (\(fileName, _) -> fileName == command) mapping
      isExecutable :: FilePath -> IO Bool
      isExecutable filePath = executable <$> getPermissions filePath
  executable <- filterM (isExecutable . snd) filesMatchingCommand
  case executable of
    ((_, filePath) : _) -> return $ Just filePath
    _ -> return Nothing
  where
    listDirectorySafe :: FilePath -> IO [FilePath]
    listDirectorySafe folderPath = do
      contents <-
        listDirectory folderPath
          `catch` ( \(_ :: IOException) -> do
                    --   putStrLn ("could not process path " ++ show folderPath)
                      return []
                  )
      return $ map (folderPath </>) contents

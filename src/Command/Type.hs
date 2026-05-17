module Command.Type where

import Control.Exception (IOException, catch)
import Control.Monad (filterM, join)
import Data.Functor ((<&>))
import Data.List (elemIndex, unfoldr)
import System.Directory
import System.Environment (getEnv)
import System.FilePath (takeFileName, (</>))


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
          path <- isCommandInPathV2 cmd
          case path of
            Just p -> return $ cmd <> " is " <> p
            Nothing -> return $ cmd <> ": not found"

    isCommandInPathV2 :: String -> IO (Maybe FilePath)
    isCommandInPathV2 command = do
      folderPath <- getEnv "PATH" <&> split ':'
      fileAndDirInPath <- join <$> mapM listDirectorySafe folderPath
      filesInPath <- filterM doesFileExist fileAndDirInPath -- filter out folders to get only files
      let mapping = map (\filePath -> (takeFileName filePath, filePath)) filesInPath
          filesMatchingCommand = filter (\(fileName, _) -> fileName == command) mapping
          isExecutable :: FilePath -> IO Bool
          isExecutable filePath = executable <$> getPermissions filePath
      executable <- filterM (isExecutable . snd) filesMatchingCommand
      case executable of
        ((commandName, _) : _) -> return $ Just commandName
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

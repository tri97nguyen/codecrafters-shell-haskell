module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (join, filterM, guard)
import Control.Monad.Trans.Maybe (MaybeT, hoistMaybe, runMaybeT)
import System.Environment (getEnv)
import Data.Foldable (find)
import Data.Functor ((<&>))
import System.Directory
import Debug.Trace (traceIO, traceM)
import Data.List (unfoldr, elemIndex)
import Control.Monad.Trans (lift)

-- splitOn :: String -> Char -> [String]
-- splitOn text delimiter =
--     toList $ execState (splitString text) (singleton "")

--     where
--     splitString :: String -> State (Seq String) ()
--     splitString "" = return ()
--     splitString (char : tail) = do
--         parseChar char
--         splitString tail


--     parseChar :: Char -> State (Seq String) ()
--     parseChar char = do
--         currentState <- get
--         if char == delimiter then put $ currentState |> ""
--         else do
--             let (front :|> lastItem) = currentState
--                 newLastItem = lastItem <> [char]
--             put (front :|> newLastItem)



main :: IO ()
main = do
    repl

builtInCommands :: [String]
builtInCommands = ["echo", "exit", "type"]

typeCommand :: [String] -> IO [String]
typeCommand = mapM checkCommand
    where
        checkCommand :: String -> IO String
        checkCommand cmd
            | cmd `elem` builtInCommands = do
                return $ cmd <> " is a shell builtin"
            | otherwise = do
                path <- runMaybeT $ isCommandExecutableT cmd
                case path of
                    Just p -> return $ cmd <> " is " <> p
                    Nothing -> return $ cmd <> ": not found"

                -- mbPath <- isCommandInPath cmd
                -- case mbPath of
                --     Just path -> do
                --         isExecutable <- executable <$> getPermissions path
                --         if isExecutable then 
                --             return $ cmd <> " is " <> path
                --         else return $ cmd <> ": not found" 
                --     _ -> return $ cmd <> ": not found"

        isCommandExecutableT :: FilePath -> MaybeT IO String
        isCommandExecutableT cmd = do
            path <- isCommandInPathT cmd
            isExecutable <- lift $ executable <$> getPermissions path
            guard isExecutable
            return path
            

        isCommandInPathT :: String -> MaybeT IO FilePath
        isCommandInPathT command = do
            path <- lift $ getEnv "PATH" <&> split ':'
            fileAndDirInPath <- lift $ join <$> mapM listDirectory path
            traceM $ "fileAndDirInPath variable is " <> show fileAndDirInPath
            filesInPath <- lift $ filterM doesFileExist fileAndDirInPath
            hoistMaybe $ find (== command) filesInPath

        -- isCommandInPath :: String -> IO (Maybe FilePath)
        -- isCommandInPath command = do
        --     path <- getEnv "PATH" <&> split ':'
        --     fileAndDirInPath <- join <$> mapM listDirectory path
        --     traceIO $ "fileAndDirInPath variable is " <> show fileAndDirInPath
        --     filesInPath <- filterM doesFileExist fileAndDirInPath
        --     return $ find (== command) filesInPath

        -- oldSplit :: Char -> String -> [String]
        -- oldSplit delimiter text =
        --     toList $ foldr accumulatorFn (singleton "") text
        --     where
        --         accumulatorFn :: Char -> Seq String -> Seq String
        --         accumulatorFn next (front :|> end)
        --             | next == delimiter = front |> end |> ""
        --             | otherwise = front |> (next : end)

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
    if input == "exit" then return ()
    else do
        let cmd: args = words input
        case cmd of
            "type" -> typeCommand args >>= mapM_ putStrLn
            "echo" -> putStrLn $ unwords args
            _ -> putStrLn $ input ++ ": command not found"
        repl





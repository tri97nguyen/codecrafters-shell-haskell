module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when, join, filterM, (>=>))
import Control.Monad.State (State, get, put, runState, execState)
import System.Environment (getEnv)
import Data.Sequence (Seq((:<|), (:|>), Empty), (|>), singleton)
import Data.Foldable (toList, find)
import Data.Functor ((<&>))
import System.Directory
import System.FilePath (makeRelative)
import Data.Maybe (isJust)
import GHC.IO.Handle.Internals (debugIO)
import Debug.Trace (trace, traceM, traceIO)
import Data.List (unfoldr, elemIndex)

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
                mbPath <- isCommandInPath cmd
                case mbPath of
                    Just path -> do
                        isExecutable <- executable <$> getPermissions path
                        if isExecutable then 
                            return $ cmd <> ": not found"
                        else return $ cmd <> " is " <> path
                    _ -> return $ cmd <> ": not found"

        isCommandInPath :: String -> IO (Maybe FilePath)
        isCommandInPath command = do
            path <- getEnv "PATH" <&> split ':'
            fileAndDirInPath <- join <$> mapM listDirectory path
            traceIO $ "fileAndDirInPath variable is " <> show fileAndDirInPath
            filesInPath <- filterM doesFileExist fileAndDirInPath
            return $ find (== command) filesInPath

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





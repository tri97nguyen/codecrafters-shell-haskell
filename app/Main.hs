module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when, join, filterM, (>=>))
import Control.Monad.State (State, get, put, runState, execState)
import System.Environment (getEnv)
import Data.Sequence (Seq((:<|), (:|>), Empty), (|>), singleton)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import System.Directory
import System.FilePath (makeRelative)

splitOn :: String -> Char -> [String]
splitOn text delimiter =
    toList $ execState (splitString text) (singleton "")

    where
    splitString :: String -> State (Seq String) ()
    splitString "" = return ()
    splitString (char : tail) = do
        parseChar char
        splitString tail


    parseChar :: Char -> State (Seq String) ()
    parseChar char = do
        currentState <- get
        if char == delimiter then put $ currentState |> ""
        else do
            let (front :|> lastItem) = currentState
                newLastItem = lastItem <> [char]
            put (front :|> newLastItem)

        

main :: IO ()
main = do
    repl

builtInCommands = ["echo", "exit", "type"]

typeCommand :: [String] -> IO String
typeCommand (command:otherCommands)
    | command `elem` builtInCommands = return $ command <> " is a shell builtin"
    | otherwise = do 

        absolutePath <- makeAbsolute command
        let isExecutable = executable <$> getPermissions absolutePath
        commandInPathAndExecutable <- (&&) <$> isExecutable <*> isCommandInPath command

        if commandInPathAndExecutable then return $ command <> " is " <> absolutePath
        else return $ command <> ": not found"

    where

        isCommandInPath :: String -> IO Bool
        isCommandInPath command = do
            path <- getEnv "PATH" <&> splitOn ',' 
            fileAndDirInPath <- join <$> mapM listDirectory path
            filesInPath <- filterM doesFileExist fileAndDirInPath
            return $ any(== command) filesInPath

        splitOn :: Char -> String -> [String]
        splitOn delimiter text =
            toList $ foldr accumulatorFn (singleton "") text
            where
                accumulatorFn :: Char -> Seq String -> Seq String
                accumulatorFn next (front :|> end) 
                    | next == delimiter = front |> end |> ""
                    | otherwise = front |> (end ++ [next])


repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    if input == "exit" then return ()
    else do
        let cmd: args = words input
        case cmd of
            "type" -> putStrLn =<< typeCommand args
            "echo" -> putStrLn $ unwords args
            _ -> putStrLn $ input ++ ": command not found"
        repl


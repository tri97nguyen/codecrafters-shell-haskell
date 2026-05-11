module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import Control.Monad.State (State, get, put, runState, execState)
import System.Environment (getEnv)
import Data.Sequence (Seq((:<|), (:|>), Empty), (|>), singleton)
import Data.Foldable (toList)

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

mysplit :: Char -> String -> [String]
mysplit delimiter text =
    toList $ foldl accumulatorFn (singleton "") text
    where
        accumulatorFn :: Seq String -> Char -> Seq String
        accumulatorFn (front :|> end) next
            | next == delimiter = front |> end |> ""
            | otherwise = front |> (end ++ [next])
        

main :: IO ()
main = do
    repl

builtInCommands = ["echo", "exit", "type"]

typeCommand :: [String] -> IO String
typeCommand (command:otherCommands)
    | command `elem` builtInCommands = return $ command <> " is a shell builtin"
    | otherwise = return $ command <> ": not found"





checkCommandInPath :: IO String
checkCommandInPath = do
    path <- getEnv "PATH"

    return ""



repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    if input == "exit" then return ()
    else do
        let cmd: args = words input
        case cmd of
            -- "type" -> putStrLn $ typeCommand args
            "echo" -> putStrLn $ unwords args
            _ -> putStrLn $ input ++ ": command not found"
        repl


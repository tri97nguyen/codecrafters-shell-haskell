module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when)

main :: IO ()
main = do
    repl

builtInCommands = ["echo", "exit", "type"]

typeCommand :: [String] -> String
typeCommand (command:otherCommands)
    | command `elem` builtInCommands = command <> " is a shell builtin"
    | otherwise = command <> ": not found"
    


repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    if input == "exit" then return ()
    else do
        let cmd: args = words input
        case cmd of
            "type" -> putStrLn $ typeCommand args
            "echo" -> putStrLn $ unwords args
            _ -> putStrLn $ input ++ ": command not found"
        repl


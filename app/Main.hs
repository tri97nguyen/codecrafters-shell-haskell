module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when)

main :: IO ()
main = do
    repl

repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    if input == "exit" then return ()
    else do
        let cmd: args = words input
        case cmd of
            "echo" -> putStrLn $ unwords args
            _ -> do
                putStrLn $ input ++ ": command not found"
                repl


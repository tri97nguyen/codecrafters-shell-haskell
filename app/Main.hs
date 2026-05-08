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
    when (input == "exit") $ return ()
    putStrLn $ input ++ ": command not found"
    repl

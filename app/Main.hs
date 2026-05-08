module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    repl

repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    putStrLn $ input ++ ": command not found"
    repl

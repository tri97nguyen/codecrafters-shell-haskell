module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    putStr $ input ++ ": command not found"
    pure ()

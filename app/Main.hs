module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import Control.Monad.State
import System.Environment (getEnv)

main :: IO ()
main = do
    repl

builtInCommands = ["echo", "exit", "type"]

typeCommand :: [String] -> IO String
typeCommand (command:otherCommands)
    | command `elem` builtInCommands = return $ command <> " is a shell builtin"
    | otherwise = return $ command <> ": not found"

splitOn :: String -> Char -> [String]
splitOn word delimiter = 
    undefined
    where
        delimiter = ","
        collectTillDelimiter :: String -> [String] -> [String]
        collectTillDelimiter text result = undefined


-- delimiter = ","
-- collectTillDelimiter :: String -> a
-- collectTillDelimiter text =
--     let a = foldl (\acc nextChar -> ) [] text
--     in _
    

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


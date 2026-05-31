{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.IO (hFlush, stdout)
import Command.Type (typeCommand, isExternalCommand)
import System.Process (readProcess)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad.Reader (ReaderT (runReaderT), runReaderT, lift, ask)
import Command.Common (Env (..), AppState (..), App)
import Control.Exception (handle, IOException)
import System.Environment (getEnv)
import Text.Megaparsec
    ( Parsec,
      empty,
      many,
      some,
      parse,
      errorBundlePretty,
      parseTest,
      MonadParsec(notFollowedBy, lookAhead),
      try,
      manyTill,
      (<|>) )
import Data.Void (Void)
import Text.Megaparsec.Char (space, space1, alphaNumChar, char, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators ()
import System.Exit (exitSuccess)
import Control.Monad (void)



main :: IO ()
main = do
  let env = Env {appState = AppState }
  runReaderT repl env



repl :: App ()
repl = do
  env <- ask
  lift $ do
    putStr "$ "
    hFlush stdout
    input <- getLine
    let parsedCmdInput = parse cmdLineArgParser "" input
    case parsedCmdInput of
      Right (Command cmd, args) -> do
        case cmd of
          "exit" -> exitSuccess
          "type" -> typeCommand args >>= mapM_ putStrLn
          "echo" -> putStrLn $ unwords args
          "pwd" -> getCurrentDirectory >>= putStrLn
          "cd" -> do
            case args of
                ("~": _) -> getEnv "HOME" >>= setCurrentDirectory
                (path : _) -> do
                  handle 
                    (\(_:: IOException) -> putStrLn $ "cd: " ++ path ++ ": No such file or directory")
                    (setCurrentDirectory path)
                _ -> return ()
          _ -> do
            mFilePath <- isExternalCommand cmd
            case mFilePath of
              Just _ -> do readProcess cmd args "" >>= putStr
              Nothing -> putStrLn $ input ++ ": command not found"
      Left errorBundle -> putStrLn $ errorBundlePretty errorBundle
    runReaderT repl env


newtype Command = Command String deriving (Show)
type Argument = String

type Parser = Parsec Void String
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty



wordInSingleQuotes :: Parser String
wordInSingleQuotes = do
  void $ char '\''
  text <- (alphaNumChar <|> spaceChar <|> skip2SingleQuotes) `manyTill` (try . lookAhead $ endQuote)
  void $ char '\''
  return text
  where
    endQuote :: Parser ()
    endQuote = do
      void $ char '\''
      notFollowedBy $ char '\''

    skip2SingleQuotes :: Parser Char
    skip2SingleQuotes = try $ do
      void $ char '\''
      void $ char '\''
      alphaNumChar <|> spaceChar



singleWord :: Parser String
singleWord = L.lexeme spaceConsumer $ try wordInSingleQuotes <|> try (some alphaNumChar)

cmdLineArgParser :: Parser (Command, [Argument])
cmdLineArgParser = do
  space
  command <- Command <$> singleWord
  arguments <- many singleWord
  return (command, arguments)


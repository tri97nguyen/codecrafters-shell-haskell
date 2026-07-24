{-# LANGUAGE OverloadedStrings #-}

module Command.Parser
  ( Argument
  , Command (..)
  , Parser
  , cmdLineArgParser
  , testParser
  ) where

import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (lookAhead, notFollowedBy)
  , Parsec
  , anySingle
  , empty
  , many
  , manyTill
  , some
  , try
  , (<|>), satisfy
  )
import Text.Megaparsec.Char (char, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

newtype Command = Command String deriving (Eq, Show)
type Argument = String

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

newtype QuoteChar = QuoteChar Char

singleQuote :: QuoteChar
singleQuote = QuoteChar '\''

wordInQuotes :: QuoteChar -> Parser String
wordInQuotes (QuoteChar quote) =
  char quote *> manyTill anySingle (char quote)

wordNotInQuote :: QuoteChar -> Parser String
wordNotInQuote (QuoteChar quote) =
  some literalChar
  where
    literalChar :: Parser Char
    literalChar = satisfy (\c -> c /= quote && c /= ' ')

singleWord :: Parser String
singleWord = L.lexeme spaceConsumer $ concat <$> some wordChunk
  where
    wordChunk :: Parser String
    wordChunk = try (wordInQuotes singleQuote) <|> wordNotInQuote singleQuote

cmdLineArgParser :: Parser (Command, [Argument])
cmdLineArgParser = do
  space
  command <- Command <$> singleWord
  arguments <- many singleWord
  return (command, arguments)

testParser :: Parser String
testParser =
  ((string "''" >> anySingle) <|> anySingle)
    `manyTill` (lookAhead $ char '\'' >> notFollowedBy (char '\''))

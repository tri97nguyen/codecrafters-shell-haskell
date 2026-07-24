module Command.ParserSpec where

import Command.Parser (Command (..), cmdLineArgParser)
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "cmdLineArgParser" $ do
    it "parses a command with arguments" $ do
      parse cmdLineArgParser "" "echo hello world"
        `shouldBe` Right (Command "echo", ["hello", "world"])
    
    it "parses a command with quotes" $ do
      parse cmdLineArgParser "" "'echo' 'hello world'"
        `shouldBe` Right (Command "echo", ["hello world"])
      parse cmdLineArgParser "" "echo 'first second' third 'forth'"
        `shouldBe` Right (Command "echo", ["first second", "third", "forth"])

    it "treat adjacent quotes as nothing" $ do
      parse cmdLineArgParser "" "echo first'' second 'third''forth'"
        `shouldBe` Right (Command "echo", ["first", "second", "thirdforth"])

    it "ignores leading spaces before the command" $ do
      parse cmdLineArgParser "" "   pwd"
        `shouldBe` Right (Command "pwd", [])

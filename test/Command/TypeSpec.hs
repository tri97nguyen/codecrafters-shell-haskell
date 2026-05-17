module Command.TypeSpec where

import Test.Hspec
import Command.Type (typeCommand)

spec :: Spec
spec = do
    describe "type command" $ do
        it "identifies echo as a shell builtin" $ do
            typeCommand ["echo"] `shouldReturn` ["echo is a shell builtin"]

        it "identifies ls as an executable command on PATH" $ do
            typeCommand ["ls"] `shouldReturn` ["ls is /bin/ls"]
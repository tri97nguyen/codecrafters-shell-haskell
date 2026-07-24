{-# OPTIONS_GHC -Wno-error=all #-}
{-# OPTIONS_GHC -Wno-all #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Command.ParserSpec
import Command.TypeSpec

main :: IO ()
main = hspec $ do
    Command.ParserSpec.spec
    Command.TypeSpec.spec
    describe "Prelude.head" $ do
        it "returns the first element of the list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException

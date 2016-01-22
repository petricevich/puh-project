module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Hello


main :: IO ()
main = hspec $ do
    describe "hello" $ do
        it "preprends 'Hello ' to strings" $
            hello "world" `shouldBe` "Hello world"
module Main where

import           Control.Exception      (evaluate)
import           Control.Monad.IO.Class (liftIO)
import           Data.Set               as Set
import           Test.Hspec
import           Test.QuickCheck
import           Types
import           WsServer


main :: IO ()
main = hspec $
  describe "WsServer.updateState" $ do
    context "when action is GetLists" $ do
      it "returns some lists" $ do
        let expectedState = State Set.empty
        updateState _ _ `shouldBe` expectedState

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $
        evaluate (head []) `shouldThrow` anyException

    context "when action is CreateList" $ do
      it "returns the first element of a list" $
        head [23 ..] `shouldBe` (23 :: Int)

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

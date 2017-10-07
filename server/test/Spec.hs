{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception (evaluate)
import           Data.UUID.V4      (nextRandom)
import           DB
import           Test.Hspec
import           Test.QuickCheck
import           Types
import           WsServer


-- performAction :: Action -> IO LBS.ByteString

main :: IO ()
main = hspec $ before_ (runDB flushDb) $
  describe "WsServer.performAction" $ do
    context "when action is GetLists" $ do
      context "when there are no lists" $
        it "returns no lists" $
          performAction GetLists `shouldReturn` "{\"confirmAction\":{\"data\":{\"lists\":[]},\"type\":\"GetLists\"}}"

      context "when there is a list" $ do
        it "returns the list" $ do
          _ <- nextRandom >>= (runDB . insertList)
          performAction GetLists `shouldReturn` "{\"confirmAction\":{\"data\":{\"lists\":[{\"createdAt\":\"2017-09-27T11:17:46.53297Z\",\"items\":[],\"listId\":\"77d640a1-6b9e-484d-ab36-4a8eaa7d8879\",\"updatedAt\":\"2017-09-27T11:17:46.53297Z\",\"title\":\"\"}]},\"type\":\"GetLists\"}}"

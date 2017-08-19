{-# LANGUAGE OverloadedStrings #-}

module JSON where

import           Data.Aeson                    (ToJSON, (.=))
import qualified Data.Aeson                    as JSON
import           Data.ByteString.Internal      (ByteString)
import qualified Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Types


decodeAction :: ByteString -> Either String Action
decodeAction =
  JSON.eitherDecodeStrict

encodeAction :: ToJSON v => Action -> v -> LazyByteString.ByteString
encodeAction action value =
  JSON.encode $ JSON.object [
  "confirmAction" .= JSON.object
    [ "type" .= JSON.String actionType
    , "data" .= JSON.object data'
    ]
  ]
  where (actionType, data') =
          case action of
            Register              ->
              ("Register", [ "clientId" .= value ])
            GetLists              ->
              ("GetLists", [ "lists" .= value ])
            (CreateList _)        ->
              ("CreateList", [ "list" .= value ])
            (DeleteList _)         ->
              ("DeleteList", [ "lists" .= value ])
            (UpdateListTitle _ _) ->
              ("UpdateListTitle", [ "list" .= value ])
            (CreateItem _ _)      ->
              ("CreateItem", [ "item" .= value ])
            (UpdateItemText _ _)  ->
              ("UpdateItemText", [ "item" .= value ])
            (SubscribeToList _)   ->
              ("SubscribeToList", [])

encodeActionIfSuccess :: ToJSON v => Action -> Maybe v -> LazyByteString.ByteString
encodeActionIfSuccess _ Nothing = encodeError $ Text.pack "Sorry, we couldn't make that change! :-("
encodeActionIfSuccess action (Just value) = encodeAction action value

encodeError :: Text -> LazyByteString.ByteString
encodeError err =
  JSON.encode $ JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String err
      ]
  ]


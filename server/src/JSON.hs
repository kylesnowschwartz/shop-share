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

encodeActionConfirmation :: ToJSON v => Action -> v -> LazyByteString.ByteString
encodeActionConfirmation action value =
  JSON.encode $ JSON.object [
  "confirmAction" .= JSON.object
    [ "type" .= JSON.String actionType
    , "data" .= JSON.object data'
    ]
  ]
  where
    updatedLists = [ "lists" .= value ]
    (actionType, data') =
          case action of
            Register ->
              ("Register", [ "clientId" .= value ])
            GetLists ->
              ("GetLists", updatedLists)
            (CreateList _) ->
              ("CreateList", updatedLists)
            (DeleteList _) ->
              ("DeleteList", updatedLists)
            (UpdateListTitle _ _) ->
              ("UpdateList", updatedLists)
            (CreateItem _) ->
              ("CreateItem", updatedLists)
            (UpdateItem _) ->
              ("UpdateItem", updatedLists)
            (DeleteItem _) ->
              ("DeleteItem", updatedLists)
            (SubscribeToList _) ->
              ("SubscribeToList", [])

encodeActionIfSuccess :: ToJSON v => Action -> Maybe v -> LazyByteString.ByteString
encodeActionIfSuccess _ Nothing = encodeError $ Text.pack "Sorry, we couldn't make that change! :-("
encodeActionIfSuccess action (Just value) = encodeActionConfirmation action value

encodeError :: Text -> LazyByteString.ByteString
encodeError err =
  JSON.encode $ JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String err
      ]
  ]


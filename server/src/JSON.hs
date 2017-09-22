{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import           Data.Aeson                    ((.=))
import qualified Data.Aeson                    as JSON
import           Data.ByteString.Internal      (ByteString)
import qualified Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Text                     (Text)
import           Data.UUID                     (UUID)
import           Types



decodeAction :: ByteString -> Either String Action
decodeAction =
  JSON.eitherDecodeStrict

encodeRegistered :: UUID -> LazyByteString.ByteString
encodeRegistered clientId' =
  JSON.encode $ JSON.object [
    "confirmAction" .= JSON.object
      [ "type" .= ("Register" :: Text)
      , "data" .= JSON.object [ "clientId" .= clientId' ]
      ]
    ]

encodeGetLists :: [List] -> LazyByteString.ByteString
encodeGetLists lists =
  JSON.encode $ JSON.object [
  "confirmAction" .= JSON.object
    [ "type" .= ("GetLists" :: Text)
    , "data" .= JSON.object [ "lists" .= lists ]
    ]
  ]

encodeCreatedList :: Maybe List -> LazyByteString.ByteString
encodeCreatedList list =
  case list of
    Nothing ->
      encodeError "Failed to create list."

    Just list' ->
      JSON.encode $ JSON.object [
      "confirmAction" .= JSON.object
        [ "type" .= ("CreateList" :: Text)
        , "data" .= JSON.object [ "list" .= list' ]
        ]
      ]

encodeDeletedList :: LazyByteString.ByteString
encodeDeletedList =
  JSON.encode $ JSON.object [
  "confirmAction" .= JSON.object
    [ "type" .= ("DeleteList" :: Text)
    , "data" .= JSON.object []
    ]
  ]


encodeUpdatedList :: Maybe List -> LazyByteString.ByteString
encodeUpdatedList list =
  case list of
    Nothing ->
      encodeError "Failed to update list."

    Just list' ->
      JSON.encode $ JSON.object [
      "confirmAction" .= JSON.object
        [ "type" .= ("UpdateList" :: Text)
        , "data" .= JSON.object [ "list" .= list' ]
        ]
      ]

encodeCreatedItem :: Maybe Item -> LazyByteString.ByteString
encodeCreatedItem item =
  case item of
    Nothing ->
      encodeError "Failed to create item."

    Just item' ->
      JSON.encode $ JSON.object [
      "confirmAction" .= JSON.object
        [ "type" .= ("CreateItem" :: Text)
        , "data" .= JSON.object [ "item" .= item' ]
        ]
      ]


encodeUpdatedItem :: Maybe Item -> LazyByteString.ByteString
encodeUpdatedItem item =
  case item of
    Nothing ->
      encodeError "Failed to update item."

    Just item' ->
      JSON.encode $ JSON.object [
      "confirmAction" .= JSON.object
        [ "type" .= ("UpdateItem" :: Text)
        , "data" .= JSON.object [ "item" .= item' ]
        ]
      ]


encodeDeletedItem :: LazyByteString.ByteString
encodeDeletedItem =
  JSON.encode $ JSON.object [
  "confirmAction" .= JSON.object
    [ "type" .= ("DeleteItem" :: Text)
    , "data" .= JSON.object []
    ]
  ]


encodeError :: Text -> LazyByteString.ByteString
encodeError err =
  JSON.encode $ JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String err
      ]
  ]


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson                    (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                    as JSON
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Set                      as Set
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (FromRow)
import           GHC.Generics                  (Generic)
import           Network.WebSockets            (Connection)

newtype Config = Config { port :: Int }

newtype State = State { clients :: Set.Set Client }

data Client = Client { clientId :: Text, conn :: Connection }

instance Eq Client where
  (Client id1 _) == (Client id2 _) = id1 == id2

instance Ord Client where
  compare (Client id1 _) (Client id2 _) = compare id1 id2

data List =
  List { id    :: Integer
       , title :: Text
       } deriving (Generic, Show)

instance FromRow List
instance ToJSON List
instance FromJSON List

data Action = Register
            | GetLists
            | CreateList
            | SubscribeToList Text deriving (Generic, Show)

instance ToJSON Action where
  toJSON Register = JSON.object [ "action" .= JSON.object [ "type" .= ("Register" :: JSON.Value) ] ]
  toJSON GetLists = JSON.object [ "action" .= JSON.object [ "type" .= ("GetLists" :: JSON.Value) ] ]
  toJSON CreateList = JSON.object [ "type" .= ("CreateList" :: JSON.Value) ]
  toJSON (SubscribeToList _) = JSON.object [ "type" .= ("SubscribeToList" :: JSON.Value) ]

instance FromJSON Action where
  parseJSON = JSON.withObject "action" $ \obj -> do
    action <- obj .: "action"
    actionType <- action .: "type"

    case actionType of
      "Register"        -> pure Register
      "GetLists"        -> pure GetLists
      "CreateList"      -> pure CreateList
      "SubscribeToList" -> SubscribeToList <$> obj .: "listId"
      _                 -> fail ("unknown action type: " ++ actionType)

decodeAction :: ByteString -> Either String Action
decodeAction =
  JSON.eitherDecodeStrict

encodeRegistered :: Text -> LazyByteString.ByteString
encodeRegistered clientId' =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String ("Register" :: Text)
    , "clientId" .= JSON.String clientId'
    ]
  ]

encodeError :: Text -> LazyByteString.ByteString
encodeError err =
  JSON.encode $ errorEncoder err

errorEncoder :: Text -> JSON.Value
errorEncoder err = JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String err
      ]
  ]

encodeLists :: [List] -> LazyByteString.ByteString
encodeLists =
  JSON.encode

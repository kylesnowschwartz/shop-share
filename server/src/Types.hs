{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson                         (FromJSON, ToJSON, (.:),
                                                     (.=))
import qualified Data.Aeson                         as JSON
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Internal      as LazyByteString
import           Data.Semigroup                     ((<>))
import           Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Database.PostgreSQL.Simple         (FromRow)
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics                       (Generic)
import           Network.WebSockets                 (Connection)

newtype Config = Config { port :: Int }

newtype State = State { clients :: Set.Set Client }

data Client = Client { clientId :: Integer, conn :: Connection }

instance Eq Client where
  (Client id1 _) == (Client id2 _) = id1 == id2

instance Ord Client where
  compare (Client id1 _) (Client id2 _) = compare id1 id2

data List =
  List { listId :: Integer
       , title  :: Text
       , items  :: [Item]
       } deriving (Generic, Show)

instance ToJSON List
instance FromJSON List

instance FromRow List where
  fromRow = List <$> field <*> field <*> pure []

data Item =
  Item { itemId    :: Integer
       , text      :: Text
       , completed :: Bool
       , listsId   :: Integer
       } deriving (Generic, Show)

instance FromRow Item
instance ToJSON Item
instance FromJSON Item

data Action = Register
            | GetLists
            | CreateList Text
            | DeleteList Integer
            | UpdateListTitle Text Integer
            | CreateItem Text Integer
            | UpdateItemText Text Integer
            | SubscribeToList Text deriving (Generic, Show)

instance ToJSON Action where
  toJSON Register = JSON.object [ "action" .= JSON.object [ "type" .= ("Register" :: JSON.Value) ] ]
  toJSON GetLists = JSON.object [ "action" .= JSON.object [ "type" .= ("GetLists" :: JSON.Value) ] ]
  toJSON (SubscribeToList _) = JSON.object [ "type" .= ("SubscribeToList" :: JSON.Value) ]
  toJSON action = errorEncoder $ Text.pack $ "Can't encode action: " <> show action

instance FromJSON Action where
  parseJSON = JSON.withObject "action" $ \obj -> do
    action <- obj .: "action"
    actionType <- action .: "type"

    case actionType of
      "Register"        -> pure Register
      "GetLists"        -> pure GetLists
      "CreateList"      -> CreateList <$> action .: "title"
      "DeleteList"      -> DeleteList <$> action .: "listId"
      "UpdateListTitle" -> UpdateListTitle <$> action .: "title" <*> action .: "listId"
      "CreateItem"      -> CreateItem <$> action .: "text" <*> action .: "listId"
      "UpdateItemText"  -> UpdateItemText <$> action .: "text" <*> action .: "itemId"
      "SubscribeToList" -> SubscribeToList <$> action .: "listId"
      _                 -> fail ("unknown action type: " ++ actionType)

decodeAction :: ByteString -> Either String Action
decodeAction =
  JSON.eitherDecodeStrict

encodeRegistered :: Integer -> LazyByteString.ByteString
encodeRegistered clientId' =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String ("Register" :: Text)
    , "data" .= JSON.object [ "clientId" .= clientId' ]
    ]
  ]

encodeLists :: [List] -> LazyByteString.ByteString
encodeLists lists =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String ("GetLists" :: Text)
    , "data" .= JSON.object [ "lists" .= lists ]
    ]
  ]

encodeList :: Text -> List -> LazyByteString.ByteString
encodeList action list =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String action
    , "data" .= JSON.object [ "list" .= list ]
    ]
  ]

encodeItem :: Text -> Item -> LazyByteString.ByteString
encodeItem action item =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String action
    , "data" .= JSON.object [ "item" .= item ]
    ]
  ]

encodeIfSuccess :: (List -> LazyByteString.ByteString) -> Maybe List -> LazyByteString.ByteString
encodeIfSuccess _ Nothing = encodeError $ Text.pack "Sorry, we couldn't make that change! :-("
encodeIfSuccess encoder (Just list) = encoder list

encodeItemIfSuccess :: (Item -> LazyByteString.ByteString) -> Maybe Item -> LazyByteString.ByteString
encodeItemIfSuccess _ Nothing = encodeError $ Text.pack "Sorry, we couldn't make that change! :-("
encodeItemIfSuccess encoder (Just item) = encoder item

encodeError :: Text -> LazyByteString.ByteString
encodeError err =
  JSON.encode $ errorEncoder err

errorEncoder :: Text -> JSON.Value
errorEncoder err = JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String err
      ]
  ]

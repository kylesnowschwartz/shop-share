{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson                         (FromJSON, ToJSON, (.:),
                                                     (.=))
import qualified Data.Aeson                         as JSON
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Internal      as LazyByteString
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

instance ToJSON Item
instance FromJSON Item
instance FromRow Item

data Action = Register
            | GetLists
            | CreateList Text
            | DeleteList Integer
            | UpdateListTitle Text Integer
            | CreateItem Text Integer
            | UpdateItemText Text Integer
            | SubscribeToList Text deriving (Generic, Show)

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

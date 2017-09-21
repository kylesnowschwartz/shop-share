{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Types where

import           Data.Aeson                         (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                         as JSON
import           Data.Set                           as Set
import           Data.Text                          (Text)
import           Data.Time
import           Data.UUID                          (UUID)
import           Database.PostgreSQL.Simple         (FromRow)
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics                       (Generic)
import           Network.WebSockets                 (Connection)


-- CONFIG, STATE & CLIENTS

newtype Config = Config { port :: Int }

newtype State = State { clients :: Set.Set Client }

data Client = Client { clientId :: UUID, conn :: Connection }

instance Eq Client where
  (Client id1 _) == (Client id2 _) = id1 == id2

instance Ord Client where
  compare (Client id1 _) (Client id2 _) = compare id1 id2


-- LISTS & ITEMS

data List =
  List { listId    :: UUID
       , title     :: Text
       , createdAt :: Maybe UTCTime
       , updatedAt :: Maybe UTCTime
       , items     :: [Item]
       } deriving (Generic, Show)

instance ToJSON List

instance FromRow List where
  fromRow = List <$>
    field <*> -- list_id
    field <*> -- title
    field <*> -- created_at
    field <*> -- updated_at
    pure []

data Item =
  Item { itemId    :: UUID
       , text      :: Text
       , completed :: Bool
       , listsId   :: UUID
       , createdAt :: Maybe UTCTime
       , updatedAt :: Maybe UTCTime
       } deriving (Generic, Show)

instance ToJSON Item
instance FromJSON Item
instance FromRow Item


-- ACTIONS

data Action = Register
            | GetLists
            | CreateList List
            | UpdateList List
            | DeleteList List
            | CreateItem Item
            | UpdateItem Item
            | DeleteItem Item
            | SubscribeToList Text
            deriving (Generic, Show)

instance FromJSON Action where
  parseJSON = JSON.withObject "action" $ \obj -> do
    action <- obj .: "action"
    actionType <- action .: "type"
    data' <- action .: "data"

    let list = List <$>
          data' .: "listId" <*>
          data' .: "title" <*>
          pure Nothing <*>
          pure Nothing <*>
          pure [] -- For now let's ignore any submitted sub-items

    let item = Item <$>
          data' .: "itemId" <*>
          data' .: "text" <*>
          data' .: "completed" <*>
          data' .: "listId" <*>
          pure Nothing <*>
          pure Nothing

    case actionType of
      "Register"        -> pure Register
      "GetLists"        -> pure GetLists
      "CreateList"      -> CreateList <$> list
      "UpdateList"      -> UpdateList <$> list
      "DeleteList"      -> DeleteList <$> list
      "CreateItem"      -> CreateItem <$> item
      "UpdateItem"      -> UpdateItem <$> item
      "DeleteItem"      -> DeleteItem <$> item
      "SubscribeToList" -> SubscribeToList <$> data' .: "listId"
      _                 -> fail ("unknown action type: " ++ actionType)

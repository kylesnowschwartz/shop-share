{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson                         (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                         as JSON
import           Data.Set                           as Set
import           Data.Text                          (Text)
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
  List { listId :: UUID
       , title  :: Text
       , items  :: [Item]
       } deriving (Generic, Show)

instance ToJSON List
instance FromJSON List

instance FromRow List where
  fromRow = List <$> field <*> field <*> pure []

data Item =
  Item { itemId    :: UUID
       , text      :: Text
       , completed :: Bool
       , listsId   :: UUID
       } deriving (Generic, Show)

instance ToJSON Item
instance FromJSON Item
instance FromRow Item


-- ACTIONS

data Action = Register
            | GetLists
            | CreateList Text
            | DeleteList UUID
            | UpdateListTitle Text UUID
            | CreateItem Text UUID
            | UpdateItemText Text UUID
            | SubscribeToList Text deriving (Generic, Show)

instance FromJSON Action where
  parseJSON = JSON.withObject "action" $ \obj -> do
    action <- obj .: "action"
    actionType <- action .: "type"
    data' <- action .: "data"

    case actionType of
      "Register"        -> pure Register
      "GetLists"        -> pure GetLists
      "CreateList"      -> CreateList <$> data' .: "title"
      "DeleteList"      -> DeleteList <$> data' .: "listId"
      "UpdateListTitle" -> UpdateListTitle <$> data' .: "title" <*> data' .: "listId"
      "CreateItem"      -> CreateItem <$> data' .: "text" <*> data' .: "listId"
      "UpdateItemText"  -> UpdateItemText <$> data' .: "text" <*> data' .: "itemId"
      "SubscribeToList" -> SubscribeToList <$> data' .: "listId"
      _                 -> fail ("unknown action type: " ++ actionType)

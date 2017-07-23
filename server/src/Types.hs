{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data List =
  List { id    :: Integer
       , title :: Text
       } deriving (Generic, Show)

instance ToJSON List
instance FromJSON List

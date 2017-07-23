{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Control.Monad.Except
import qualified DB
import           Servant
import           Servant.API
import           Types


-- TYPES

type API = Lists

type Lists = "lists" :> Get '[JSON] [List]


-- API DEFINITION

api :: Servant.Server API
api = getLists


-- ACTIONS

getLists :: ExceptT ServantErr IO [List]
getLists = liftIO . DB.runDB $ DB.selectAllLists

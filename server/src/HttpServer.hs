{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HttpServer where


import           Control.Monad.Except
import qualified DB
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Middleware.Static as WaiStatic
import           Servant
import           Types


type API = Lists

type Lists = "lists" :> Get '[JSON] [List]

httpApp :: Wai.Application
httpApp request respond =
  if isRequestForIndex request
  then respond $ Wai.responseLBS Http.status400 [] "REST API under construction."
  else WaiStatic.static apiApp request respond

apiApp :: Wai.Application
apiApp = Servant.serve (Servant.Proxy :: Servant.Proxy API) api

api :: Servant.Server API
api = getLists

isRequestForIndex :: Wai.Request -> Bool
isRequestForIndex request = null (Wai.pathInfo request)

getLists :: ExceptT ServantErr IO [List]
getLists = liftIO . DB.runDB $ DB.selectAllLists

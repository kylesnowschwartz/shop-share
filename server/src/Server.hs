{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
    ( runServer
    , defaultConfig
    ) where

import           API
import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_,
                                                 newMVar, readMVar)
import qualified Control.Exception              as Exception
import           Control.Monad                  (forM_, forever)
import           Data.Aeson                     (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                     as JSON
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Lazy.Internal  as LazyByteString
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding
import qualified Debug.Trace                    as Debug
import           GHC.Generics                   (Generic)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WSHandler
import qualified Network.Wai.Logger             as WaiLogger
import qualified Network.Wai.Middleware.Static  as WaiStatic
import qualified Network.WebSockets             as WS
import qualified Servant
import qualified Servant.API



-- SERVER TYPES

newtype Config = Config { port :: Int }

newtype State = State { clients :: Clients }

data Client = Client { clientId :: Text, conn :: WS.Connection }

type Clients = Set.Set Client

instance Eq Client where (Client id1 _) == (Client id2 _) = id1 == id2
instance Ord Client where compare (Client id1 _) (Client id2 _) = compare id1 id2

data Action = Register
            | CreateList
            | SubscribeToList Text deriving (Generic, Show)

instance ToJSON Action where
  toJSON Register = JSON.object [ "action" .= JSON.object [ "type" .= ("register" :: JSON.Value) ] ]
  toJSON CreateList = JSON.object [ "type" .= ("createList" :: JSON.Value) ]
  toJSON (SubscribeToList _) = JSON.object [ "type" .= ("subscribeToList" :: JSON.Value) ]

instance FromJSON Action where
  parseJSON = JSON.withObject "action" $ \obj -> do
    action <- obj .: "action"
    actionType <- action .: "type"

    case actionType of
      "register"        -> pure Register
      "createList"      -> pure CreateList
      "subscribeToList" -> SubscribeToList <$> obj .: "listId"
      _                 -> fail ("unknown action type: " ++ actionType)

decodeAction :: ByteString -> Either String Action
decodeAction =
  JSON.eitherDecodeStrict

encodeConnected :: Text -> LazyByteString.ByteString
encodeConnected clientId =
  JSON.encode $ JSON.object
  [
    "confirmAction" .= JSON.object
    [ "type" .= JSON.String ("register" :: Text)
    , "clientId" .= JSON.String clientId
    ]
  ]

encodeError :: Text -> LazyByteString.ByteString
encodeError error =
  JSON.encode $ JSON.object [
  "error" .= JSON.object [
      "message" .= JSON.String error
      ]
  ]


-- INITIAL STATE

initialServerState =
  State { clients = Set.empty }

defaultConfig =
  Config { port = 8000 }


-- REST API SERVER

httpApp :: Wai.Application
httpApp request respond =
  if isRequestForIndex request
  then respond $ Wai.responseLBS Http.status400 [] "REST API under construction."
  else WaiStatic.static apiApp request respond

apiApp :: Wai.Application
apiApp = Servant.serve (Servant.Proxy :: Servant.Proxy API) api

isRequestForIndex :: Wai.Request -> Bool
isRequestForIndex request = null (Wai.pathInfo request)


-- WEBSOCKET SERVER

wsApp :: MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn -- TODO: Should we be using receiveDataMessage here?
  state <- readMVar stateMVar

  let client = Client (nextClientId state) conn

  case decodeAction msg of
    Right Register -> flip Exception.finally (disconnect stateMVar client) $ do
      connect conn stateMVar client
      talk conn stateMVar client

    Left err -> WS.sendTextData conn $ encodeError $ Text.pack err

nextClientId :: State -> Text
nextClientId state =
  "1"

connect :: WS.Connection -> MVar State -> Client -> IO ()
connect conn stateMVar client = do
  modifyMVar_ stateMVar $ updateClients Set.insert client
  WS.sendTextData conn (encodeConnected $ clientId client)

disconnect :: MVar State -> Client -> IO ()
disconnect stateMVar client =
  modifyMVar_ stateMVar $ updateClients Set.delete client

updateClients :: (Client -> Clients -> Clients) -> Client -> State -> IO State
updateClients alteration client state =
  return $ State $ alteration client $ clients state

talk :: WS.Connection -> MVar State -> Client -> IO ()
talk conn stateMVar client = forever $ do
  WS.receiveData conn >>= updateState stateMVar
  newState <- readMVar stateMVar
  broadcast newState

updateState :: MVar State -> Text -> IO State
updateState stateMVar msg =
  modifyMVar stateMVar $ \state ->
    return (state, State $ clients state)

broadcast :: State -> IO ()
broadcast state =
  forM_ (clients state) $ \client ->
    WS.sendTextData (conn client) $ encodeState state

encodeState :: State -> Text
encodeState state =
  Text.concat $ map clientId (Set.elems $ clients state)


-- SERVER

runServer :: Config -> IO ()
runServer config = do
  initialState <- newMVar initialServerState

  WaiLogger.withStdoutLogger $ \logger -> do
    let settings = Warp.setLogger logger . Warp.setPort (port config) $ Warp.defaultSettings

    Warp.runSettings settings $
      WSHandler.websocketsOr WS.defaultConnectionOptions (wsApp initialState) httpApp

{-# LANGUAGE OverloadedStrings #-}

module Server
    ( runServer
    , defaultConfig
    ) where


import           API
import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as ByteString
import qualified Data.Set                       as Set
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WSHandler
import qualified Network.Wai.Middleware.Static  as WaiStatic
import qualified Network.WebSockets             as WS
import qualified Servant
import qualified Servant.API


-- SERVER TYPES

newtype Config = Config { port :: Int }

newtype State = State { clients :: Clients }

data Client = Client { clientId :: ByteString, conn :: WS.Connection }

type Clients = Set.Set Client

instance Eq Client where (Client id1 _) == (Client id2 _) = id1 == id2
instance Ord Client where compare (Client id1 _) (Client id2 _) = compare id1 id2


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

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn -- TODO: Should we be using receiveDataMessage here?
  state <- Concurrent.readMVar stateMVar

  case msg of
    _ | not (msg `ByteString.isPrefixOf` joinAsPrefix) ->
        WS.sendTextData conn ("Please register as a client first." :: ByteString)

      | Set.member client (clients state) ->
        WS.sendTextData conn ("Client already registered." :: ByteString)

      | otherwise -> flip Exception.finally (disconnect stateMVar client) $ do
          connect conn stateMVar client
          talk conn stateMVar client

      where
        joinAsPrefix = "register as: "
        client = Client (nextClientId state) conn


nextClientId :: State -> ByteString
nextClientId state =
  "1"

connect :: WS.Connection -> Concurrent.MVar State -> Client -> IO ()
connect conn stateMVar client =
  Concurrent.modifyMVar_ stateMVar $ updateClients Set.insert client

disconnect :: Concurrent.MVar State -> Client -> IO ()
disconnect stateMVar client =
  Concurrent.modifyMVar_ stateMVar $ updateClients Set.delete client

updateClients :: (Client -> Clients -> Clients) -> Client -> State -> IO State
updateClients alteration client state =
  return $ State $ alteration client $ clients state

talk conn stateMVar client = Monad.forever $ do
  WS.receiveData conn >>= updateState stateMVar
  newState <- Concurrent.readMVar stateMVar
  broadcast newState

updateState :: Concurrent.MVar State -> ByteString -> IO State
updateState stateMVar msg =
  Concurrent.modifyMVar stateMVar $ \state ->
    return (state, State $ clients state)

broadcast :: State -> IO ()
broadcast state =
  Monad.forM_ (clients state) $ \client ->
    WS.sendTextData (conn client) $ encodeState state

encodeState :: State -> ByteString
encodeState state =
  ByteString.concat $ map clientId (Set.elems $ clients state)


-- SERVER

runServer :: Config -> IO ()
runServer config = do
  initialState <- Concurrent.newMVar initialServerState
  Warp.run (port config) $
    WSHandler.websocketsOr WS.defaultConnectionOptions (wsApp initialState) httpApp

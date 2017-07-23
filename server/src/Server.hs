{-# LANGUAGE OverloadedStrings #-}

module Server
    ( runServer
    , defaultConfig
    ) where


import qualified Control.Concurrent             as Concurrent
import           Data.ByteString                (ByteString)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WSHandler
import qualified Network.WebSockets             as WS


-- TYPES

newtype Config = Config { port :: Int }

data State = State { }


-- INITIAL STATE

initialServerState =
  State { }

defaultConfig =
  Config { port = 8000 }


-- REST API SERVER

httpApp :: Wai.Application
httpApp request respond =
  respond $ Wai.responseLBS Http.status400 [] "REST API under construction."


-- WEBSOCKET SERVER

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  conn <- WS.acceptRequest pending
  WS.sendTextData conn ("WS Server under construction." :: ByteString)


-- SERVER

runServer :: Config -> IO ()
runServer config = do
  initialState <- Concurrent.newMVar initialServerState
  Warp.run (port config) $
    WSHandler.websocketsOr WS.defaultConnectionOptions (wsApp initialState) httpApp

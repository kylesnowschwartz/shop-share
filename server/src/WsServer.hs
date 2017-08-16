{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WsServer where

import           Control.Concurrent            (MVar, modifyMVar_, readMVar)

import qualified Control.Exception             as Exception
import           Control.Monad                 (forM_, forever)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Network.WebSockets            as WS
import           Types


wsApp :: MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  connection <- WS.acceptRequest pending
  WS.forkPingThread connection 30
  msg <- WS.receiveData connection -- Should we be using receiveDataMessage here?
  state <- readMVar stateMVar

  let client = Client (nextClientId state) connection

  case decodeAction msg of
    Right Register ->
      flip Exception.finally (disconnect stateMVar client) $ do
        connect stateMVar client
        talk stateMVar client

    Right _ -> WS.sendTextData connection $ encodeError "Please register first."

    Left err -> WS.sendTextData connection $ encodeError $ Text.pack err

nextClientId :: State -> Text
nextClientId _state =
  "1"

connect :: MVar State -> Client -> IO ()
connect stateMVar client@Client{..} = do
  modifyMVar_ stateMVar $ updateClients Set.insert client
  WS.sendTextData conn $ encodeRegistered clientId

disconnect :: MVar State -> Client -> IO ()
disconnect stateMVar client =
  modifyMVar_ stateMVar $ updateClients Set.delete client

talk :: MVar State -> Client -> IO ()
talk stateMVar Client{..} = forever $ do
  response <- WS.receiveData conn >>= updateState stateMVar
  WS.sendTextData conn response

broadcast :: State -> IO ()
broadcast state =
  forM_ (clients state) $ \client ->
    WS.sendTextData (conn client) $ encodeState state

updateState :: MVar State -> ByteString -> IO LazyByteString.ByteString
updateState _stateMVar msg =
  case decodeAction msg of
    Left _err ->
      return $ encodeError $ Text.pack _err

    Right action ->
      case action of
        GetLists ->
          return $ encodeLists []
        _ ->
          return $ encodeError $ Text.pack "Action not yet built. Sorry! Come back later :-)"

updateClients :: (Client -> Set Client -> Set Client) -> Client -> State -> IO State
updateClients alteration client state =
  return $ State (alteration client $ clients state)

encodeState :: State -> Text
encodeState state =
  Text.concat $ map clientId (Set.elems $ clients state)

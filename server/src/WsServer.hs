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
import qualified Data.Text                     as Text
import           DB
import qualified Network.WebSockets            as WS
import           Types


wsApp :: MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  connection <- WS.acceptRequest pending
  WS.forkPingThread connection 30
  msg <- WS.receiveData connection
  state <- readMVar stateMVar

  let client = Client (nextClientId state) connection

  case decodeAction msg of
    Right Register ->
      flip Exception.finally (disconnect stateMVar client) $ do
        connect stateMVar client
        talk stateMVar client

    Right _ -> WS.sendTextData connection $ encodeError "Please register first."

    Left err -> WS.sendTextData connection $ encodeError $ Text.pack err

nextClientId :: State -> Integer
nextClientId state =
  if Set.null cs
  then 1
  else clientId (Set.findMax cs) + 1
  where cs = clients state

connect :: MVar State -> Client -> IO ()
connect stateMVar client@Client{..} = do
  modifyMVar_ stateMVar $ updateClients Set.insert client
  WS.sendTextData conn $ encodeAction Register clientId

disconnect :: MVar State -> Client -> IO ()
disconnect stateMVar client =
  modifyMVar_ stateMVar $ updateClients Set.delete client

talk :: MVar State -> Client -> IO ()
talk stateMVar client = forever $ do
  response <- WS.receiveData (conn client) >>= handleAction client stateMVar
  state <- readMVar stateMVar
  forM_ (clients state) $ \c ->
    WS.sendTextData (conn c) response

handleAction :: Client -> MVar State -> ByteString -> IO LazyByteString.ByteString
handleAction _updatedBy _stateMVar msg =
  case decodeAction msg of
    Left _err ->
      return $ encodeError $ Text.pack _err

    Right action ->
      performAction action

performAction :: Action -> IO LazyByteString.ByteString
performAction action =
  case action of
    GetLists ->
      encodeAction action <$> runDB selectAllLists

    CreateList title ->
      encodeActionIfSuccess action <$> runDB (insertList title)

    DeleteList listId' -> do
      runDB $ deleteList listId'
      encodeAction action <$> runDB selectAllLists

    UpdateListTitle newTitle id' ->
      encodeActionIfSuccess action <$> runDB (updateList newTitle id')

    CreateItem itemText listId' ->
      encodeActionIfSuccess action <$> runDB (insertItem itemText listId')

    UpdateItemText newText id' ->
      encodeActionIfSuccess action <$> runDB (updateItem newText id')

    _ ->
      return $ encodeError $ Text.pack "Action not yet built. Sorry!"

updateClients :: (Client -> Set Client -> Set Client) -> Client -> State -> IO State
updateClients alteration client state =
  return $ State (alteration client $ clients state)

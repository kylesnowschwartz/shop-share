{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WsServer where

import           Control.Concurrent            (MVar, modifyMVar_, readMVar)
import qualified Control.Exception             as Exception
import           Control.Monad                 (forM_, forever)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Data.UUID                     (UUID)
import           Data.UUID.V4                  (nextRandom)
import           DB
import           JSON
import qualified Network.WebSockets            as WS
import           Types


wsApp :: MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  connection <- WS.acceptRequest pending
  WS.forkPingThread connection 30
  msg <- WS.receiveData connection
  client <- Client <$> nextClientId <*> return connection

  case decodeAction msg of
    Right Register ->
      flip Exception.finally (disconnect stateMVar client) $ do
        connect stateMVar client
        talk stateMVar client

    Right _ -> WS.sendTextData connection $ encodeError "Please register first."

    Left err -> WS.sendTextData connection $ encodeError $ Text.pack err

nextClientId :: IO Data.UUID.UUID
nextClientId =
  nextRandom

connect :: MVar State -> Client -> IO ()
connect stateMVar client@Client{..} = do
  modifyMVar_ stateMVar $ updateClients Set.insert client
  WS.sendTextData conn $ encodeRegistered clientId

disconnect :: MVar State -> Client -> IO ()
disconnect stateMVar client =
  modifyMVar_ stateMVar $ updateClients Set.delete client

talk :: MVar State -> Client -> IO ()
talk stateMVar client = forever $ do
  response <- WS.receiveData (conn client) >>= handleAction client stateMVar
  state <- readMVar stateMVar
  forM_ (clients state) $ \c ->
    WS.sendTextData (conn c) response

handleAction :: Client -> MVar State -> ByteString -> IO LBS.ByteString
handleAction _updatedBy _stateMVar msg =
  case decodeAction msg of
    Left _err ->
      return $ encodeError $ Text.pack _err

    Right action ->
      performAction action

performAction :: Action -> IO LBS.ByteString
performAction action =
  case action of
    GetLists -> do
      lists <- runDB selectAllLists
      return $ encodeGetLists lists

    CreateList list -> do
      maybeList <- runDB (insertList $ listId list)
      encodeCreatedList <$> runDB (mapM selectItemsForList maybeList)

    UpdateList list -> do
      maybeList <- runDB $ updateListTitle (listId list) (title list)
      encodeUpdatedList <$> runDB (mapM selectItemsForList maybeList)

    DeleteList list -> do
      runDB $ deleteList $ listId list
      return encodeDeletedList

    CreateItem item -> do
      maybeItem <- runDB $ insertItem item
      return $ encodeCreatedItem maybeItem

    UpdateItem item -> do
      maybeItem <- runDB $ updateItem item
      return $ encodeUpdatedItem maybeItem

    DeleteItem item -> do
      runDB $ deleteItem $ itemId item
      return encodeDeletedItem

    Register ->
      return $ encodeError $ Text.pack "You're already registered on this connection!"

    _ ->
      return $ encodeError $ Text.pack "Action not yet built. Sorry!"

updateClients :: (Client -> Set Client -> Set Client) -> Client -> State -> IO State
updateClients alteration client state =
  return $ State (alteration client $ clients state)

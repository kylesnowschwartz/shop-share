{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Control.Exception               (bracket)
import           Data.ByteString.Internal        (ByteString)
import qualified Database.PostgreSQL.Simple      as PG
import           Database.PostgreSQL.Transaction
import           Types


runDB :: PGTransaction a -> IO a
runDB action =
  withConnection $ \connection ->
                     runPGTransactionIO action connection

withConnection :: (PG.Connection -> IO a) -> IO a
withConnection =
  bracket (PG.connectPostgreSQL connectionString) PG.close

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=shopshare_dev connect_timeout=10"

-- QUERIES

selectAllLists :: PGTransaction [List]
selectAllLists =
  query_ "SELECT * FROM lists"

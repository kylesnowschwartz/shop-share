{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Control.Exception               (bracket)
import qualified Database.PostgreSQL.Simple      as PG
import           Database.PostgreSQL.Transaction
import           Types


instance PG.FromRow List


-- EXECUTION

runDB :: PGTransaction a -> IO a
runDB action = withConnection $ \conn -> runPGTransactionIO action conn

withConnection :: (PG.Connection -> IO a) -> IO a
withConnection = bracket
  (PG.connectPostgreSQL "host=localhost port=5432 dbname=shopshare_dev connect_timeout=10")
  PG.close


-- QUERIES

selectAllLists :: PGTransaction [List]
selectAllLists =
  query_ "SELECT * FROM lists"

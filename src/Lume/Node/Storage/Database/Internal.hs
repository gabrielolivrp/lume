{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lume.Node.Storage.Database.Internal (
  Database (..),
  DatabaseM,
  BlockDatabase,
  ChainStateDatabase,
  DatabaseError (..),
  openBlockDB,
  openChainStateDB,
  dbPut,
  dbGet,
  dbDelete,
  runDatabase,
) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Database.LevelDB qualified as LevelDB
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO)

data Context
  = BlockContext
  | ChainStateContext
  deriving (Show, Eq)

class DatabaseContext (scope :: Context)

instance DatabaseContext 'BlockContext
instance DatabaseContext 'ChainStateContext

type DatabaseM m = LevelDB.MonadResource m

newtype Database (scope :: Context) = Database
  { dbInst :: LevelDB.DB
  }

type BlockDatabase = Database 'BlockContext

type ChainStateDatabase = Database 'ChainStateContext

data DatabaseError
  = DatabaseDecodeError String
  deriving (Show, Eq)

openBlockDB :: (DatabaseM m) => FilePath -> m BlockDatabase
openBlockDB fp = openDatabase @'BlockContext (fp </> "blocks")

openChainStateDB :: (DatabaseM m) => FilePath -> m ChainStateDatabase
openChainStateDB fp = openDatabase @'ChainStateContext (fp </> "chainstate")

openDatabase :: (DatabaseContext scope, DatabaseM m) => FilePath -> m (Database scope)
openDatabase fp = Database <$> LevelDB.open fp LevelDB.defaultOptions{LevelDB.createIfMissing = True}

dbPut :: (DatabaseM m, Binary a) => Database scope -> BS.ByteString -> a -> m ()
dbPut inst k v =
  let serialized = BSL.toStrict $ encode v
   in LevelDB.put (dbInst inst) LevelDB.defaultWriteOptions{LevelDB.sync = True} k serialized

dbGet ::
  (DatabaseM m, Binary a) =>
  Database scope ->
  BS.ByteString ->
  m (Either DatabaseError (Maybe a))
dbGet inst key = do
  mbs <- LevelDB.get (dbInst inst) LevelDB.defaultReadOptions key
  pure (traverse decodeBS mbs)
 where
  decodeBS bs =
    case decodeOrFail (BSL.fromStrict bs) of
      Left (_, _, err) -> Left (DatabaseDecodeError err)
      Right (_, _, val) -> Right val

dbDelete ::
  (DatabaseM m) =>
  Database scope ->
  BS.ByteString ->
  m ()
dbDelete inst = LevelDB.delete (dbInst inst) LevelDB.defaultWriteOptions

runDatabase :: (MonadUnliftIO m) => ResourceT m a -> m a
runDatabase = LevelDB.runResourceT

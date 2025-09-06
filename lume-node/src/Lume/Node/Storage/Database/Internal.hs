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
  openDatabase,
  openBlockDB,
  openChainStateDB,
  dbPut,
  dbGet,
  dbDelete,
  runDatabaseM,
) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Database.LevelDB qualified as LevelDB
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UnliftIO (MonadIO (liftIO), MonadUnliftIO)

data Context
  = BlockContext
  | ChainStateContext
  deriving (Show, Eq)

class DatabaseContext (scope :: Context)

instance DatabaseContext 'BlockContext
instance DatabaseContext 'ChainStateContext

type DatabaseM m = LevelDB.MonadResource m

newtype Inst (scope :: Context) = Inst
  { dbInst :: LevelDB.DB
  }

type BlockDatabase = Inst 'BlockContext

type ChainStateDatabase = Inst 'ChainStateContext

data Database = Database
  { dbBlock :: BlockDatabase
  , dbChainState :: ChainStateDatabase
  }

data DatabaseError
  = DatabaseDecodeError String
  deriving (Show, Eq)

openDatabase :: (DatabaseM m) => FilePath -> m Database
openDatabase fp = do
  blockDB <- openBlockDB fp
  chainStateDB <- openChainStateDB fp
  pure (Database blockDB chainStateDB)

openBlockDB :: (DatabaseM m) => FilePath -> m BlockDatabase
openBlockDB fp = openDatabase' @'BlockContext (fp </> "blocks")

openChainStateDB :: (DatabaseM m) => FilePath -> m ChainStateDatabase
openChainStateDB fp = openDatabase' @'ChainStateContext (fp </> "chainstate")

openDatabase' :: (DatabaseContext scope, DatabaseM m) => FilePath -> m (Inst scope)
openDatabase' fp = do
  liftIO (createDirectoryIfMissing True fp)
  db <- LevelDB.open fp LevelDB.defaultOptions{LevelDB.createIfMissing = True}
  pure (Inst db)

dbPut :: (DatabaseM m, Binary a) => Inst scope -> BS.ByteString -> a -> m ()
dbPut inst k v =
  let serialized = BSL.toStrict (encode v)
   in LevelDB.put (dbInst inst) LevelDB.defaultWriteOptions{LevelDB.sync = True} k serialized

dbGet ::
  (DatabaseM m, Binary a) =>
  Inst scope ->
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
  Inst scope ->
  BS.ByteString ->
  m ()
dbDelete inst = LevelDB.delete (dbInst inst) LevelDB.defaultWriteOptions

runDatabaseM :: (MonadUnliftIO m) => ResourceT m a -> m a
runDatabaseM = LevelDB.runResourceT

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Storage.Database (
  DatabaseConfig (..),
  Database (..),
  Context (..),
  DatabaseGetError (..),
  BlockStatus (..),
  BlockRecord (..),
  FileInfoRecord (..),
  UTXORecord (..),
  openDatabase,
  defaultDatabaseConfig,
  mkBlockKey,
  putBlock,
  getBlock,
  mkFileInfoKey,
  putFileInfo,
  getFileInfo,
  mkLastBlockFileKey,
  putLastBlockFile,
  getLastBlockFile,
  mkUtxoKey,
  putUtxo,
  getUtxo,
  deleteUtxo,
)
where

import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Word (Word32, Word64)
import Database.LevelDB
import GHC.Generics (Generic)
import Lume.Consensus.Difficulty (Bits)
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash (Hash, toRawBytes)
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Amount (Amount)
import System.FilePath ((</>))

data Context
  = BlockContext
  | ChainStateContext
  deriving (Show, Eq)

class DatabaseContext (scope :: Context)

instance DatabaseContext 'BlockContext
instance DatabaseContext 'ChainStateContext

data DatabaseConfig = DatabaseConfig
  { _dbBasePath :: FilePath
  -- ^ Base path for the database
  , _dbCacheSize :: Maybe Int
  -- ^ LRU cache size
  , _dbWriteBufferSize :: Maybe Int
  -- ^ Write buffer size
  , _dbMaxOpenFiles :: Maybe Int
  -- ^ Max open files
  }

newtype Database (scope :: Context) = Database
  { dbInst :: DB
  }

defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig =
  DatabaseConfig
    { _dbBasePath = ".lume"
    , _dbCacheSize = Nothing
    , _dbWriteBufferSize = Nothing
    , _dbMaxOpenFiles = Nothing
    }

openDatabase ::
  (DatabaseContext scope, MonadResource m) =>
  DatabaseConfig ->
  String ->
  m (Database scope)
openDatabase dbconfig dbpath = do
  let fp = _dbBasePath dbconfig </> dbpath
      options = mkOptions dbconfig
  Database <$> open fp options

mkOptions :: DatabaseConfig -> Options
mkOptions config =
  defaultOptions
    { createIfMissing = True
    , cacheSize = fromMaybe (cacheSize defaultOptions) (_dbCacheSize config)
    , writeBufferSize = fromMaybe (writeBufferSize defaultOptions) (_dbWriteBufferSize config)
    , maxOpenFiles = fromMaybe (maxOpenFiles defaultOptions) (_dbMaxOpenFiles config)
    }
{-# INLINE mkOptions #-}

dbPut ::
  (MonadResource m, Binary a) =>
  DB ->
  BS.ByteString ->
  a ->
  m ()
dbPut inst k v =
  let serialized = BSL.toStrict $ encode v
   in put inst defaultWriteOptions{sync = True} k serialized

data DatabaseGetError
  = DecodeError Text
  | ValueNotFound
  deriving stock (Show, Eq)

dbGet ::
  (MonadResource m, Binary a) =>
  DB ->
  BS.ByteString ->
  m (Either DatabaseGetError a)
dbGet inst k = do
  bs <- get inst defaultReadOptions{fillCache = False} k
  case bs of
    Nothing -> pure $ Left ValueNotFound
    Just bs' ->
      case decodeOrFail (BSL.fromStrict bs') of
        Left (_, _, err) -> pure . Left . DecodeError . pack $ err
        Right (_, _, value) -> pure $ Right value

dbDelete ::
  (MonadResource m) =>
  DB ->
  BS.ByteString ->
  m ()
dbDelete inst = delete inst defaultWriteOptions

data BlockStatus
  = BlockStatusUnknown
  | BlockStatusValid
  | BlockStatusHaveData
  | BlockStatusInvalid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

data BlockRecord = BlockRecord
  { _brVersion :: !Word32
  -- ^ Block index version
  , _brHeight :: !Word64
  -- ^ Block index height
  , _brStatus :: !BlockStatus
  -- ^ Block index status
  , _brTxCount :: !Word32
  -- ^ Block index transaction count
  , _brFile :: !Word32
  -- ^ Block index file number
  , _brDataPos :: !Word32
  -- ^ Block index data position
  , _brMerkleRoot :: !Hash
  -- ^ Block index merkle root hash
  , _brTimestamp :: !Timestamp
  -- ^ Block index timestamp
  , _brBits :: !Bits
  -- ^ Block index bits
  , _brNonce :: !Word32
  -- ^ Block index nonce
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

mkBlockKey :: Hash -> BS.ByteString
mkBlockKey h = "b" <> toRawBytes h
{-# INLINE mkBlockKey #-}

putBlock ::
  (MonadResource m) =>
  Database 'BlockContext ->
  Hash ->
  BlockRecord ->
  m ()
putBlock db = dbPut (dbInst db) . mkBlockKey

getBlock ::
  (MonadResource m) =>
  Database 'BlockContext ->
  Hash ->
  m (Either DatabaseGetError BlockRecord)
getBlock db = dbGet (dbInst db) . mkBlockKey

data FileInfoRecord = FileInfoRecord
  { _fiBlockCount :: !Word32
  -- ^ The number of blocks stored in the block file.
  , _fiFileSize :: !Word32
  -- ^ The size in bytes of the block file.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

mkFileInfoKey :: Word32 -> BS.ByteString
mkFileInfoKey db = "f" <> Char8.pack (show db)
{-# INLINE mkFileInfoKey #-}

putFileInfo ::
  (MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  FileInfoRecord ->
  m ()
putFileInfo db = dbPut (dbInst db) . mkFileInfoKey

getFileInfo ::
  (MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  m (Either DatabaseGetError FileInfoRecord)
getFileInfo db = dbGet (dbInst db) . mkFileInfoKey

mkLastBlockFileKey :: BS.ByteString
mkLastBlockFileKey = "l"
{-# INLINE mkLastBlockFileKey #-}

putLastBlockFile ::
  (MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  m ()
putLastBlockFile db = dbPut (dbInst db) mkLastBlockFileKey

getLastBlockFile ::
  (MonadResource m) =>
  Database 'BlockContext ->
  m (Either DatabaseGetError Word32)
getLastBlockFile db = dbGet (dbInst db) mkLastBlockFileKey

data UTXORecord = UTXORecord
  { _urIsCoinbase :: !Bool
  -- ^ Is the UTXO a coinbase transaction?
  , _urValue :: !Amount
  -- ^ Amount of coin in the output
  , _uIdx :: !Word32
  -- ^ Index of the output in the transaction
  , _urOwner :: Address
  -- ^ Address that owns this unspent output
  , _urHeight :: !Word64
  -- ^ Height of the block that created this UTXO
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

mkUtxoKey :: Hash -> Word32 -> BS.ByteString
mkUtxoKey txh vout = "u" <> toRawBytes txh <> Char8.pack (show vout)
{-# INLINE mkUtxoKey #-}

putUtxo ::
  (MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  UTXORecord ->
  m ()
putUtxo db txh vout = dbPut (dbInst db) (mkUtxoKey txh vout)

getUtxo ::
  (MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  m (Either DatabaseGetError UTXORecord)
getUtxo db txh vout = dbGet (dbInst db) (mkUtxoKey txh vout)

deleteUtxo ::
  (MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  m ()
deleteUtxo db txh vout = dbDelete (dbInst db) (mkUtxoKey txh vout)

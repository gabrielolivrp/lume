{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.Database (
  -- * Types
  Context (..),
  Database (..),
  DatabaseConfig (..),
  GetDatabaseError (..),
  BlockStatus (..),
  BlockModel (..),
  FileInfoModel (..),
  UTXOModel (..),

  -- * Database
  openDatabase,
  defaultDatabaseConfig,

  -- * Blocks
  putBlock,
  getBlock,
  putFileInfo,
  getFileInfo,
  putLastBlockFile,
  getLastBlockFile,

  -- * Chainstate
  putUtxo,
  getUtxo,
  deleteUtxo,
)
where

import Data.Binary (Binary (get, put), decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Word (Word32, Word64)
import Database.LevelDB qualified as LevelDB
import GHC.Generics (Generic)
import Lume.Core.Block.Difficulty (Bits)
import Lume.Core.Crypto.Address (Address)
import Lume.Core.Crypto.Hash (Hash, toRawBytes)
import Lume.Core.Time.Timestamp (Timestamp)
import Lume.Core.Transaction (Coin)
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
  { dbInst :: LevelDB.DB
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
  (DatabaseContext scope, LevelDB.MonadResource m) =>
  DatabaseConfig ->
  String ->
  m (Database scope)
openDatabase dbconfig dbpath = do
  let fp = _dbBasePath dbconfig </> dbpath
      options = mkOptions dbconfig
  Database <$> LevelDB.open fp options

mkOptions :: DatabaseConfig -> LevelDB.Options
mkOptions config =
  LevelDB.defaultOptions
    { LevelDB.createIfMissing = True
    , LevelDB.cacheSize = fromMaybe (LevelDB.cacheSize LevelDB.defaultOptions) (_dbCacheSize config)
    , LevelDB.writeBufferSize = fromMaybe (LevelDB.writeBufferSize LevelDB.defaultOptions) (_dbWriteBufferSize config)
    , LevelDB.maxOpenFiles = fromMaybe (LevelDB.maxOpenFiles LevelDB.defaultOptions) (_dbMaxOpenFiles config)
    }
{-# INLINE mkOptions #-}

dbPut ::
  (LevelDB.MonadResource m, Binary a) =>
  LevelDB.DB ->
  BS.ByteString ->
  a ->
  m ()
dbPut inst k v =
  let serialized = BSL.toStrict $ encode v
   in LevelDB.put inst LevelDB.defaultWriteOptions{LevelDB.sync = True} k serialized

data GetDatabaseError
  = DatabaseDecodeError Text
  | DatabaseValueNotFoundError
  deriving stock (Show, Eq)

dbGet ::
  (LevelDB.MonadResource m, Binary a) =>
  LevelDB.DB ->
  BS.ByteString ->
  m (Either GetDatabaseError a)
dbGet inst k = do
  bs <- LevelDB.get inst LevelDB.defaultReadOptions{LevelDB.fillCache = False} k
  case bs of
    Nothing -> pure $ Left DatabaseValueNotFoundError
    Just bs' ->
      case decodeOrFail (BSL.fromStrict bs') of
        Left (_, _, err) -> pure . Left . DatabaseDecodeError . pack $ err
        Right (_, _, value) -> pure $ Right value

dbDelete ::
  (LevelDB.MonadResource m) =>
  LevelDB.DB ->
  BS.ByteString ->
  m ()
dbDelete inst = LevelDB.delete inst LevelDB.defaultWriteOptions

data BlockStatus
  = BlockStatusUnknown
  | BlockStatusValid
  | BlockStatusHaveData
  | BlockStatusInvalid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

data BlockModel = BlockModel
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

instance Binary BlockModel where
  put (BlockModel version height status txCount file dataPos mroot timestamp bits nonce) = do
    put version
    put height
    put status
    put txCount
    put file
    put dataPos
    put mroot
    put timestamp
    put bits
    put nonce
  get =
    BlockModel
      <$> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get

mkBlockKey :: Hash -> BS.ByteString
mkBlockKey h = "b" <> toRawBytes h
{-# INLINE mkBlockKey #-}

putBlock ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  Hash ->
  BlockModel ->
  m ()
putBlock db = dbPut (dbInst db) . mkBlockKey

getBlock ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  Hash ->
  m (Either GetDatabaseError BlockModel)
getBlock db = dbGet (dbInst db) . mkBlockKey

data FileInfoModel = FileInfoModel
  { _fiBlockCount :: !Word32
  -- ^ The number of blocks stored in the block file.
  , _fiFileSize :: !Word32
  -- ^ The size in bytes of the block file.
  }
  deriving stock (Show, Eq, Generic)

instance Binary FileInfoModel where
  put (FileInfoModel blockCount fileSize) = do
    put blockCount
    put fileSize
  get = FileInfoModel <$> get <*> get

mkFileInfoKey :: Word32 -> BS.ByteString
mkFileInfoKey db = "f" <> Char8.pack (show db)
{-# INLINE mkFileInfoKey #-}

putFileInfo ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  FileInfoModel ->
  m ()
putFileInfo db = dbPut (dbInst db) . mkFileInfoKey

getFileInfo ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  m (Either GetDatabaseError FileInfoModel)
getFileInfo db = dbGet (dbInst db) . mkFileInfoKey

mkLastBlockFileKey :: BS.ByteString
mkLastBlockFileKey = "l"
{-# INLINE mkLastBlockFileKey #-}

putLastBlockFile ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  Word32 ->
  m ()
putLastBlockFile db = dbPut (dbInst db) mkLastBlockFileKey

getLastBlockFile ::
  (LevelDB.MonadResource m) =>
  Database 'BlockContext ->
  m (Either GetDatabaseError Word32)
getLastBlockFile db = dbGet (dbInst db) mkLastBlockFileKey

data UTXOModel = UTXOModel
  { _urIsCoinbase :: !Bool
  -- ^ Is the UTXO a coinbase transaction?
  , _urValue :: !Coin
  -- ^ Amount of coin in the output
  , _uIdx :: !Word32
  -- ^ Index of the output in the transaction
  , _urOwner :: Address
  -- ^ Address that owns this unspent output
  , _urHeight :: !Word64
  -- ^ Height of the block that created this UTXO
  }
  deriving stock (Show, Eq, Generic)

instance Binary UTXOModel where
  put (UTXOModel isCoinbase value idx owner height) = do
    put isCoinbase
    put value
    put idx
    put owner
    put height
  get =
    UTXOModel
      <$> get
      <*> get
      <*> get
      <*> get
      <*> get

mkUtxoKey :: Hash -> Word32 -> BS.ByteString
mkUtxoKey txh vout = "u" <> toRawBytes txh <> Char8.pack (show vout)
{-# INLINE mkUtxoKey #-}

putUtxo ::
  (LevelDB.MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  UTXOModel ->
  m ()
putUtxo db txh vout = dbPut (dbInst db) (mkUtxoKey txh vout)

getUtxo ::
  (LevelDB.MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  m (Either GetDatabaseError UTXOModel)
getUtxo db txh vout = dbGet (dbInst db) (mkUtxoKey txh vout)

deleteUtxo ::
  (LevelDB.MonadResource m) =>
  Database 'ChainStateContext ->
  Hash ->
  Word32 ->
  m ()
deleteUtxo db txh vout = dbDelete (dbInst db) (mkUtxoKey txh vout)

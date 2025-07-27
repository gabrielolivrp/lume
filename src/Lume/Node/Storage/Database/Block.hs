{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.Database.Block where

import Control.Lens
import Data.Binary (Binary (get, put))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lume.Core
import Lume.Core.Crypto.Hash (Hash, toRawBytes)
import Lume.Node.Storage.Database.Internal
import Lume.Node.Storage.FileStorage qualified as FS

data BlockStatus
  = BlockStatusUnknown
  | BlockStatusValid
  | BlockStatusHaveData
  | BlockStatusInvalid
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary)

data BlockModel = BlockModel
  { bmVersion :: !Word32
  -- ^ Block index version
  , bmHeight :: !Word64
  -- ^ Block index height
  , bmStatus :: !BlockStatus
  -- ^ Block index status
  , bmTxCount :: !Word32
  -- ^ Block index transaction count
  , bmFile :: !Word32
  -- ^ Block index file number
  , bmDataPos :: !Word32
  -- ^ Block index data position
  , bmMerkleRoot :: !Hash
  -- ^ Block index merkle root hash
  , bmTimestamp :: !Timestamp
  -- ^ Block index timestamp
  , bmBits :: !Bits
  -- ^ Block index bits
  , bmNonce :: !Word32
  -- ^ Block index nonce
  }
  deriving (Show, Eq, Generic)

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

data FileInfoModel = FileInfoModel
  { fiBlockCount :: !Word32
  -- ^ The number of blocks stored in the block file.
  , fiFileSize :: !Word32
  -- ^ The size in bytes of the block file.
  }
  deriving (Show, Eq, Generic)

instance Binary FileInfoModel where
  put (FileInfoModel blockCount fileSize) = do
    put blockCount
    put fileSize
  get = FileInfoModel <$> get <*> get

newtype HeightToHashModel = HeightToHashModel
  { hhHash :: Hash
  }

instance Binary HeightToHashModel where
  put (HeightToHashModel hash) = put hash
  get = HeightToHashModel <$> get

mkBlockKey :: Hash -> BS.ByteString
mkBlockKey h = "b" <> toRawBytes h
{-# INLINE mkBlockKey #-}

putBlock ::
  (DatabaseM m) =>
  BlockDatabase ->
  Hash ->
  BlockModel ->
  m ()
putBlock db = dbPut db . mkBlockKey

getBlock ::
  (DatabaseM m) =>
  BlockDatabase ->
  Hash ->
  m (Either DatabaseError (Maybe BlockModel))
getBlock db = dbGet db . mkBlockKey

mkFileInfoKey :: Word32 -> BS.ByteString
mkFileInfoKey db = "f" <> Char8.pack (show db)
{-# INLINE mkFileInfoKey #-}

putFileInfo ::
  (DatabaseM m) =>
  BlockDatabase ->
  Word32 ->
  FileInfoModel ->
  m ()
putFileInfo db = dbPut db . mkFileInfoKey

getFileInfo ::
  (DatabaseM m) =>
  BlockDatabase ->
  Word32 ->
  m (Either DatabaseError (Maybe FileInfoModel))
getFileInfo db fileIndex = dbGet db (mkFileInfoKey fileIndex)

mkHeightToHashKey :: Word64 -> BS.ByteString
mkHeightToHashKey h = "h" <> Char8.pack (show h)

putHeightToHash ::
  (DatabaseM m) =>
  BlockDatabase ->
  Word64 ->
  HeightToHashModel ->
  m ()
putHeightToHash db h = dbPut db (mkHeightToHashKey h)

getHeightToHash ::
  (DatabaseM m) =>
  BlockDatabase ->
  Word64 ->
  m (Either DatabaseError (Maybe HeightToHashModel))
getHeightToHash db h = dbGet db (mkHeightToHashKey h)

lastBlockFileKey :: BS.ByteString
lastBlockFileKey = "l"
{-# INLINE lastBlockFileKey #-}

getLastBlockFile ::
  (DatabaseM m) =>
  BlockDatabase ->
  m (Either DatabaseError (Maybe Word32))
getLastBlockFile db = dbGet db lastBlockFileKey

putLastBlockFile ::
  (DatabaseM m) =>
  BlockDatabase ->
  Word32 ->
  m ()
putLastBlockFile db = dbPut db lastBlockFileKey

toBlockModel :: Block -> BlockStatus -> FS.BlockPosition -> FS.FileIndex -> BlockModel
toBlockModel block status position fileIndex =
  BlockModel
    { bmVersion = block ^. (bHeader . bVersion)
    , bmHeight = block ^. bHeader . bHeight
    , bmStatus = status
    , bmTxCount = fromIntegral $ NE.length (getTxs $ block ^. bTxs)
    , bmFile = fileIndex
    , bmDataPos = fromIntegral position
    , bmMerkleRoot = block ^. (bHeader . bMerkleRoot)
    , bmTimestamp = block ^. (bHeader . bTimestamp)
    , bmBits = block ^. (bHeader . bBits)
    , bmNonce = block ^. (bHeader . bNonce)
    }
{-# INLINE toBlockModel #-}

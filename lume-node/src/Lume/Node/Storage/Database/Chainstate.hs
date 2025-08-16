{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.Database.Chainstate where

import Data.Binary (Binary (get, put))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lume.Core
import Lume.Core.Crypto.Address (Address)
import Lume.Core.Crypto.Hash (Hash, toRawBytes)
import Lume.Node.Storage.Database.Internal

data UTXOModel = UTXOModel
  { umIsCoinbase :: !Bool
  -- ^ Is the UTXO a coinbase transaction?
  , umValue :: !Coin
  -- ^ Amount of coin in the output
  , umIdx :: !Word32
  -- ^ Index of the output in the transaction
  , umOwner :: Address
  -- ^ Address that owns this unspent output
  , umHeight :: !Word64
  -- ^ Height of the block that created this UTXO
  }
  deriving (Show, Eq, Generic)

instance Binary UTXOModel where
  put (UTXOModel isCoinbase' value idx owner height) = do
    put isCoinbase'
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

getUtxo ::
  (DatabaseM m) =>
  ChainStateDatabase ->
  Hash ->
  Word32 ->
  m (Either DatabaseError (Maybe UTXOModel))
getUtxo db txh vout = dbGet db (mkUtxoKey txh vout)

putUtxo ::
  (DatabaseM m) =>
  ChainStateDatabase ->
  Hash ->
  Word32 ->
  UTXOModel ->
  m ()
putUtxo db txh vout = dbPut db (mkUtxoKey txh vout)

deleteUtxo ::
  (DatabaseM m) =>
  ChainStateDatabase ->
  Hash ->
  Word32 ->
  m ()
deleteUtxo db txh vout = dbDelete db (mkUtxoKey txh vout)

bestBlockKey :: BS.ByteString
bestBlockKey = "bestblock"
{-# INLINE bestBlockKey #-}

putBestBlock ::
  (DatabaseM m) =>
  ChainStateDatabase ->
  Hash ->
  m ()
putBestBlock db = dbPut db bestBlockKey

getBestBlock ::
  (DatabaseM m) =>
  ChainStateDatabase ->
  m (Either DatabaseError (Maybe Hash))
getBestBlock db = dbGet db bestBlockKey

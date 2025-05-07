{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Transaction.Types where

import Control.Lens (makeLenses)
import Data.Binary (Binary)
import Data.Map qualified as M
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash (Hash, ToHash)
import Lume.Crypto.Signature (PublicKey, Signature)
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Amount (Amount)
import Lume.Types (Version)

newtype Idx = Idx Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (Binary)

data Outpoint = Outpoint
  { _outpId :: Hash
  , _outpIdx :: Idx
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''Outpoint

data TxIn = TxIn
  { _txInPrevOut :: Outpoint
  , _txInSignature :: Signature
  , _txInPubKey :: PublicKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxIn

data TxOut = TxOut
  { _txOutAddress :: Address
  , _txOutValue :: Amount
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxOut

data Tx = Tx
  { _txIn :: ![TxIn]
  , _txOut :: ![TxOut]
  , _txVersion :: !Version
  , _txTimestamp :: !Timestamp
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Tx

newtype Txs = Txs [Tx]
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

makeLenses ''Txs

data UTXO = UTXO
  { _utxoId :: Hash
  , _utxoIdx :: Idx
  , _utxoOwner :: Address
  , _utxoValue :: Amount
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''UTXO

newtype UtxoSet = UtxoSet (M.Map Outpoint UTXO)
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

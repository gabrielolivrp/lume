{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Transaction.Types where

import Control.Lens
import Data.Binary (Binary)
import Data.Map qualified as M
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash (Hash, ToHash)
import Lume.Crypto.Signature (PublicKey, Signature)
import Lume.Transaction.Amount (Amount)

data Outpoint = Outpoint
  { _outpId :: !Hash
  -- ^ Hash of the transaction
  , _outpIdx :: !Word64
  -- ^ Output index within the source transaction
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

makeLenses ''Outpoint

data TxIn = TxIn
  { _txInPrevOut :: !Outpoint
  -- ^ Previous output of the transaction (UTXO)
  , _txInSignature :: !Signature
  -- ^ Signature of the transaction
  , _txInPubKey :: !PublicKey
  -- ^ Public key of the transaction
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxIn

data TxOut = TxOut
  { _txOutAddress :: !Address
  -- ^ Address that will receive the funds
  , _txOutValue :: !Amount
  -- ^ Amount to be transferred
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxOut

data Tx = Tx
  { _txIn :: ![TxIn]
  -- ^ Inputs of the transaction
  , _txOut :: ![TxOut]
  -- ^ Outputs of the transaction
  , _txVersion :: !Word32
  -- ^ Version of the transaction
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Tx

newtype Txs = Txs {getTxs :: [Tx]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''Txs

data UTXO = UTXO
  { _utxoId :: !Hash
  -- ^ Hash of the transaction
  , _utxoIdx :: !Word64
  -- ^ Index of the output in the transaction
  , _utxoOwner :: !Address
  -- ^ Address that owns this unspent output
  , _utxoValue :: !Amount
  -- ^ Amount of money in the output
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''UTXO

newtype UtxoSet = UtxoSet (M.Map Outpoint UTXO)
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

-- | A transaction is a coinbase transaction if it has no inputs.
isCoinbase :: Tx -> Bool
isCoinbase tx = null (tx ^. txIn)

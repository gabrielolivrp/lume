{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.UTXO where

import Control.Lens (makeLenses)
import Data.Binary (Binary)
import GHC.Generics
import Lume.Crypto.Address (Address)
import Lume.Core.Amount (Amount)
import Lume.Core.Idx (Idx)
import Lume.Crypto.Hash (Hash)
import Lume.Crypto.Signature (PublicKey, Signature)

data Outpoint = Outpoint
  { _outpId :: Hash
  , _outpIdx :: Idx
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''Outpoint

data UTXO = UTXO
  { _utxoId :: Hash
  , _utxoIdx :: Idx
  , _utxoOwner :: Address
  , _utxoValue :: Amount
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''UTXO

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

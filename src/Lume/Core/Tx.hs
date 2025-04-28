{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.Tx where

import Control.Lens (makeLenses)
import Data.Binary (Binary)
import Data.Sequence qualified as S
import GHC.Generics (Generic)
import Lume.Core.Timestamp (Timestamp)
import Lume.Core.UTXO (TxIn, TxOut)
import Lume.Core.Version (Version)
import Lume.Crypto.Hash (ToHash)

data Tx = Tx
  { _txIn :: !(S.Seq TxIn)
  , _txOut :: !(S.Seq TxOut)
  , _txVersion :: !Version
  , _txTimestamp :: !Timestamp
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Tx

newtype Txs = Txs (S.Seq Tx)
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

makeLenses ''Txs

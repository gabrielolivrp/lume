{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.State where

import Control.Lens (makeLenses)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Lume.Core.Block (Block)
import Lume.Core.Timestamp (Timestamp)
import Lume.Core.UTXO (Outpoint, UTXO)
import Lume.Crypto.Hash (Hash)

data ChainState = Blockchain
  { _csBlocks :: !(NE.NonEmpty Block)
  , _csUtxoSet :: !(M.Map Outpoint UTXO)
  , _csLastBlockHash :: !Hash
  , _csLastBlockTimestamp :: !Timestamp
  }

makeLenses ''ChainState

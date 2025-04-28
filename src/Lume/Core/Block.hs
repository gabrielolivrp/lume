{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.Block where

import Control.Lens (makeLenses)
import Data.Binary (Binary)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lume.Core.Idx (Idx)
import Lume.Core.Nonce (Nonce)
import Lume.Core.Timestamp (Timestamp)
import Lume.Core.Tx (Txs)
import Lume.Core.Version (Version)
import Lume.Crypto.Hash (Hash, ToHash)

data BlockHeader = BlockHeader
  { _bVersion :: !Version
  , _bIdx :: !Idx
  , _bNonce :: !Nonce
  , _bMerkleRoot :: !Hash
  , _bHashPrevBlock :: !Hash
  , _bTimestamp :: !Timestamp
  , _bDifficulty :: !Word64
  , _bHeight :: !Word64
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''BlockHeader

data Block = Block
  { _bHeader :: !BlockHeader
  , _bSize :: !Word64
  , _bHash :: !Hash
  , _bTxs :: !Txs
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Block

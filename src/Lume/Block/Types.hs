{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Block.Types where

import Control.Lens
import Data.Binary (Binary)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lume.Consensus.Difficulty (Bits)
import Lume.Consensus.PoW (Nonce)
import Lume.Crypto.Hash (Hash, ToHash)
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Types (Txs (..))
import Lume.Types (Version)

data BlockException

data BlockHeader = BlockHeader
  { _bVersion :: !Version
  , _bNonce :: !Nonce
  , _bMerkleRoot :: !Hash
  , _bHashPrevBlock :: !Hash
  , _bTimestamp :: !Timestamp
  , _bBits :: !Bits
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''BlockHeader

data Block = Block
  { _bHeader :: !BlockHeader
  , _bHeight :: !Word64
  , _bSize :: !Word64
  , _bTxs :: !Txs
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Block

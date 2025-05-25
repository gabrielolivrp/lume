{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Block.Types where

import Control.Lens
import Data.Binary (Binary)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lume.Consensus.Difficulty (Bits)
import Lume.Crypto.Hash (Hash, ToHash)
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Types (Txs (..))

data BlockHeader = BlockHeader
  { _bVersion :: !Word32
  -- ^ Block format version
  , _bNonce :: !Word32
  -- ^ Nonce for solving the PoW problem.
  , _bMerkleRoot :: !Hash
  -- ^ Top hash of the Merkle tree built from all transactions
  , _bHashPrevBlock :: !Hash
  -- ^ Hash of previous block header
  , _bTimestamp :: !Timestamp
  -- ^ Timestamp of block creation
  , _bBits :: !Bits
  -- ^ Compact target for the PoW problem
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''BlockHeader

data Block = Block
  { _bHeader :: !BlockHeader
  -- ^ Block header
  , _bHeight :: !Word64
  -- ^ Block height in the chain
  , _bTxs :: !Txs
  -- ^ Transactions in the block
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, ToHash)

makeLenses ''Block

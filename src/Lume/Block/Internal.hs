{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Block.Internal (
  -- * Types
  BlockHeader (..),
  Block (..),

  -- * Errors
  BlockError (..),

  -- * Lenses
  bVersion,
  bNonce,
  bMerkleRoot,
  bHashPrevBlock,
  bTimestamp,
  bBits,
  bHeader,
  bHeight,
  bTxs,

  -- * Functions
  blockVersion,
  buildBlockHeader,
  buildBlock,
  hashBlock,
  computeMerkleRoot,

  -- * Validation
  validateBlockIndex,
  validatePrevBlockHash,
  validateMerkleRoot,
  validateTransactions,
)
where

import Control.Lens
import Control.Monad.Except (MonadError, throwError, unless)
import Data.Binary (Binary)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32, Word64)
import GHC.Base (when)
import GHC.Generics (Generic)
import Lume.Consensus.Difficulty (Bits, initialBits)
import Lume.Crypto.Hash (Hash, ToHash (toHash))
import Lume.Crypto.MerkleTree qualified as MTree
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction (TransactionError, Txs (..), UtxoSet, hashTx, isCoinbase, validateTx)

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

data BlockError
  = BlockInvalidHeightError
  | BlockInvalidMerkleRootError
  | BlockHashPrevBlockMismatchError
  | BlockInvalidTransactionError TransactionError
  | BlockDuplicateTransactionError
  | BlockInvalidCoinbaseTransactionError
  | BlockMissingCoinbaseTransactionError
  deriving (Show, Eq)

blockVersion :: Word32
blockVersion = 0x00000001
{-# INLINE blockVersion #-}

buildBlockHeader :: Block -> Hash -> Timestamp -> BlockHeader
buildBlockHeader prevBlock mroot timestamp =
  BlockHeader
    { _bVersion = blockVersion
    , _bNonce = 0
    , _bMerkleRoot = mroot
    , _bTimestamp = timestamp
    , _bBits = initialBits
    , _bHashPrevBlock = hashBlock prevBlock
    }

buildBlock :: Block -> Timestamp -> Txs -> Block
buildBlock prevBlock timestamp txs =
  let mroot = computeMerkleRoot txs
      blockHeader = buildBlockHeader prevBlock mroot timestamp
      blockHeight = (prevBlock ^. bHeight) + 1
   in Block blockHeader blockHeight txs

hashBlock :: Block -> Hash
hashBlock block = toHash (block ^. bHeader)
{-# INLINE hashBlock #-}

computeMerkleRoot :: Txs -> Hash
computeMerkleRoot = snd . MTree.fromListWithRoot . toList . getTxs
{-# INLINE computeMerkleRoot #-}

validateMerkleRoot :: (MonadError BlockError m) => Hash -> Txs -> m ()
validateMerkleRoot expectedRoot txs
  | expectedRoot == computeMerkleRoot txs = pure ()
  | otherwise = throwError BlockInvalidMerkleRootError
{-# INLINE validateMerkleRoot #-}

validateBlockIndex :: (MonadError BlockError m) => Block -> Block -> m ()
validateBlockIndex prevBlock block =
  let expectedHeight = prevBlock ^. bHeight + 1
   in when (block ^. bHeight /= expectedHeight) $ throwError BlockInvalidHeightError
{-# INLINE validateBlockIndex #-}

validatePrevBlockHash :: (MonadError BlockError m) => Block -> Block -> m ()
validatePrevBlockHash prevBlock block =
  let expectedHash = hashBlock prevBlock
   in when (block ^. bHeader . bHashPrevBlock /= expectedHash) $
        throwError BlockHashPrevBlockMismatchError
{-# INLINE validatePrevBlockHash #-}

validateTransactions :: (MonadError BlockError m) => UtxoSet -> Block -> m ()
validateTransactions utxoSet block = do
  let txs = getTxs $ block ^. bTxs

  unless (isCoinbase (NE.head txs)) $
    throwError BlockMissingCoinbaseTransactionError

  let txHashes = NE.map hashTx txs
  when (length txHashes /= length (NE.nub txHashes)) $
    throwError BlockDuplicateTransactionError

  mapM_ validate (NE.tail txs)
 where
  validate tx = do
    when (isCoinbase tx) $ throwError BlockInvalidCoinbaseTransactionError
    case validateTx utxoSet tx of
      Left err -> throwError (BlockInvalidTransactionError err)
      Right _ -> pure ()

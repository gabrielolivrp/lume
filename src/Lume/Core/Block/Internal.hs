{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.Block.Internal (
  -- * Types
  BlockHeader (..),
  Block (..),

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

  -- * Errors
  BlockError (..),

  -- * Validations
  validateMerkleRoot,
  validateBlockIndex,
  validatePrevBlockHash,
  validateTransactions,

  -- * Block Builder
  buildBlock,
  genesisBlock,

  -- * Functions
  blockVersion,
  hashBlock,
  computeMerkleRoot,
)
where

import Control.Lens
import Control.Monad.Except (MonadError, throwError, unless)
import Data.Binary (Binary (get, put))
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32, Word64)
import GHC.Base (when)
import GHC.Generics (Generic)
import Lume.Core.Block.Difficulty (Bits, initialBits)
import Lume.Core.Crypto.Address (Address (Address))
import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Core.Crypto.MerkleTree qualified as MTree
import Lume.Core.Time.Timestamp (Timestamp (..))
import Lume.Core.Transaction

data BlockHeader = BlockHeader
  { _bVersion :: !Word32
  -- ^ Block format version
  , _bNonce :: !Word32
  -- ^ Nonce for solving the PoW problem.
  , _bMerkleRoot :: !Hash.Hash
  -- ^ Top hash of the Merkle tree built from all transactions
  , _bHashPrevBlock :: !Hash.Hash
  -- ^ Hash of previous block header
  , _bTimestamp :: !Timestamp
  -- ^ Timestamp of block creation
  , _bBits :: !Bits
  -- ^ Compact target for the PoW problem
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hash.ToHash)

instance Binary BlockHeader where
  put (BlockHeader version nonce mroot hashPrev timestamp bits) = do
    put version
    put nonce
    put mroot
    put hashPrev
    put timestamp
    put bits

  get = BlockHeader <$> get <*> get <*> get <*> get <*> get <*> get

makeLenses ''BlockHeader

data Block = Block
  { _bHeader :: !BlockHeader
  -- ^ Block header
  , _bHeight :: !Word64
  -- ^ Block height in the chain
  , _bTxs :: !Txs
  -- ^ Transactions in the block
  }
  deriving (Eq, Show, Generic)

instance Binary Block where
  put (Block header height txs) = do
    put header
    put height
    put txs
  get = Block <$> get <*> get <*> get

makeLenses ''Block

blockVersion :: Word32
blockVersion = 0x00000001
{-# INLINE blockVersion #-}

hashBlock :: Block -> Hash.Hash
hashBlock block = Hash.toHash (block ^. bHeader)
{-# INLINE hashBlock #-}

computeMerkleRoot :: Txs -> Hash.Hash
computeMerkleRoot = snd . MTree.fromListWithRoot . toList . getTxs
{-# INLINE computeMerkleRoot #-}

-------------------
-- Block Builders
-------------------

buildBlockHeader :: Block -> Bits -> Hash.Hash -> Timestamp -> BlockHeader
buildBlockHeader prevBlock bits mroot timestamp =
  BlockHeader
    { _bVersion = blockVersion
    , _bNonce = 0
    , _bMerkleRoot = mroot
    , _bTimestamp = timestamp
    , _bBits = bits
    , _bHashPrevBlock = hashBlock prevBlock
    }

buildBlock :: Block -> Bits -> Timestamp -> Txs -> Block
buildBlock prevBlock bits timestamp txs =
  let mroot = computeMerkleRoot txs
      blockHeader = buildBlockHeader prevBlock bits mroot timestamp
      blockHeight = (prevBlock ^. bHeight) + 1
   in Block blockHeader blockHeight txs

-----------------
-- Genesis block
-----------------

genesisHashPrev :: Hash.Hash
genesisHashPrev = Hash.hash' "genesis block"

genesisTimestamp :: Timestamp
genesisTimestamp = Timestamp 1717699200
{-# INLINE genesisTimestamp #-}

genesisNonce :: Word32
genesisNonce = 0
{-# INLINE genesisNonce #-}

genesisCoinbase :: Tx
genesisCoinbase = buildCoinbaseTx $ NE.singleton (genesisAddress, genesisAmount)
 where
  genesisAddress = Address "lume_addr_14ch3yrpyqcychs7l2err55cf8mpzpedufaj02xthqyz0hrf3uxzsvrk4kv"
  genesisAmount = Coin 1000000

genesisMerkleRoot :: Hash.Hash
genesisMerkleRoot = computeMerkleRoot (Txs (NE.singleton genesisCoinbase))
{-# NOINLINE genesisMerkleRoot #-}

genesisBlock :: Block
genesisBlock =
  let header =
        BlockHeader
          { _bVersion = blockVersion
          , _bNonce = genesisNonce
          , _bMerkleRoot = genesisMerkleRoot
          , _bHashPrevBlock = genesisHashPrev
          , _bTimestamp = genesisTimestamp
          , _bBits = initialBits
          }
      height = 0
      txs = Txs (NE.singleton genesisCoinbase)
   in Block header height txs

----------------------------
-- Block Validations
----------------------------

data BlockError
  = BlockInvalidHeightError
  | BlockInvalidMerkleRootError
  | BlockHashPrevBlockMismatchError
  | BlockInvalidTransactionError TransactionError
  | BlockDuplicateTransactionError
  | BlockInvalidCoinbaseTransactionError
  | BlockMissingCoinbaseTransactionError
  deriving (Show, Eq)

validateMerkleRoot :: (MonadError BlockError m) => Hash.Hash -> Txs -> m ()
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

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
  validateBlock,

  -- * Block Builder
  buildBlock,
  genesisBlock,

  -- * Functions
  blockVersion,
  blockHash,
  computeMerkleRoot,
)
where

import Control.Lens hiding ((.=))
import Control.Monad (unless, when)
import Data.Aeson
import Data.Binary (Binary (get, put))
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32, Word64)
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
  , _bHeight :: !Word64
  -- ^ Block height in the chain
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hash.ToHash)

instance Binary BlockHeader where
  put (BlockHeader version nonce mroot hashPrev timestamp bits height) = do
    put version
    put nonce
    put mroot
    put hashPrev
    put timestamp
    put bits
    put height

  get = BlockHeader <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance ToJSON BlockHeader where
  toJSON (BlockHeader version nonce mroot hashPrev timestamp bits height) =
    object
      [ "version" .= version
      , "nonce" .= nonce
      , "merkle_root" .= mroot
      , "previous_block_hash" .= hashPrev
      , "timestamp" .= timestamp
      , "bits" .= bits
      , "height" .= height
      ]

instance FromJSON BlockHeader where
  parseJSON = withObject "BlockHeader" $ \v ->
    BlockHeader
      <$> v .: "version"
      <*> v .: "nonce"
      <*> v .: "merkle_root"
      <*> v .: "previous_block_hash"
      <*> v .: "timestamp"
      <*> v .: "bits"
      <*> v .: "height"

makeLenses ''BlockHeader

data Block = Block
  { _bHeader :: !BlockHeader
  -- ^ Block header
  , _bTxs :: !Txs
  -- ^ Transactions in the block
  }
  deriving (Eq, Show, Generic)

instance ToJSON Block where
  toJSON (Block header txs) =
    object
      [ "header" .= header
      , "transactions" .= txs
      ]

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v ->
    Block
      <$> v .: "header"
      <*> v .: "transactions"

instance Binary Block where
  put (Block header txs) = do
    put header
    put txs
  get = Block <$> get <*> get

makeLenses ''Block

blockVersion :: Word32
blockVersion = 0x00000001
{-# INLINE blockVersion #-}

blockHash :: Block -> Hash.Hash
blockHash block = Hash.toHash (block ^. bHeader)
{-# INLINE blockHash #-}

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
    , _bHashPrevBlock = blockHash prevBlock
    , _bHeight = (prevBlock ^. bHeader . bHeight) + 1
    }

buildBlock :: Block -> Bits -> Timestamp -> Txs -> Block
buildBlock prevBlock bits timestamp txs =
  let mroot = computeMerkleRoot txs
      blockHeader = buildBlockHeader prevBlock bits mroot timestamp
   in Block blockHeader txs

-----------------
-- Genesis block
-----------------

genesisHashPrev :: Hash.Hash
genesisHashPrev = Hash.hash' "block_hash"

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
  genesisAmount = Coin 100000000

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
          , _bHeight = 0
          }
      txs = Txs (NE.singleton genesisCoinbase)
   in Block header txs

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

validateMerkleRoot :: Hash.Hash -> Txs -> Either BlockError ()
validateMerkleRoot expectedRoot txs
  | expectedRoot == computeMerkleRoot txs = pure ()
  | otherwise = Left BlockInvalidMerkleRootError
{-# INLINE validateMerkleRoot #-}

validateBlockIndex :: Block -> Block -> Either BlockError ()
validateBlockIndex prevBlock block =
  let expectedHeight = prevBlock ^. bHeader . bHeight + 1
   in when (block ^. bHeader . bHeight /= expectedHeight) $ Left BlockInvalidHeightError
{-# INLINE validateBlockIndex #-}

validatePrevBlockHash :: Block -> Block -> Either BlockError ()
validatePrevBlockHash prevBlock block =
  let expectedHash = blockHash prevBlock
   in when (block ^. bHeader . bHashPrevBlock /= expectedHash) $
        Left BlockHashPrevBlockMismatchError
{-# INLINE validatePrevBlockHash #-}

validateTransactions :: UtxoSet -> Block -> Either BlockError ()
validateTransactions utxoSet block = do
  let txs = getTxs (block ^. bTxs)

  unless (isCoinbase (NE.head txs)) $
    Left BlockMissingCoinbaseTransactionError

  let txHashes = NE.map txHash txs
  when (length txHashes /= length (NE.nub txHashes)) $
    Left BlockDuplicateTransactionError

  mapM_ validate (NE.tail txs)
 where
  validate tx = do
    when (isCoinbase tx) $ Left BlockInvalidCoinbaseTransactionError
    case validateTx utxoSet tx of
      Left err -> Left (BlockInvalidTransactionError err)
      Right _ -> pure ()

validateBlock :: UtxoSet -> Block -> Block -> Either BlockError ()
validateBlock utxoSet prevBlock block = do
  validateBlockIndex prevBlock block
  validatePrevBlockHash prevBlock block
  validateMerkleRoot (block ^. bHeader . bMerkleRoot) (block ^. bTxs)
  validateTransactions utxoSet block

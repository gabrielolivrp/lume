{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Block.Builder (
  buildBlock,
  hashBlock,
  bVersion,
  BlockBuilderError (..),
) where

import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (Foldable (toList))
import Data.Word (Word32)
import GHC.Base (when)
import Lume.Block.Types (Block (..), BlockHeader (..), bHeader, bHeight)
import Lume.Consensus.Difficulty (initialBits)
import Lume.Crypto.Hash (Hash, ToHash (toHash))
import Lume.Crypto.MerkleTree qualified as MTree
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Types (Txs (..))

data BlockBuilderError
  = InvalidHeight
  | MissingTransactions
  deriving (Show, Eq)

bVersion :: Word32
bVersion = 0x00000001
{-# INLINE bVersion #-}

buildBlockHeader :: Block -> Hash -> Timestamp -> BlockHeader
buildBlockHeader prevBlock mroot timestamp =
  BlockHeader
    { _bVersion = bVersion
    , _bNonce = 0
    , _bMerkleRoot = mroot
    , _bTimestamp = timestamp
    , _bBits = initialBits
    , _bHashPrevBlock = hashBlock prevBlock
    }

buildBlock :: (MonadError BlockBuilderError m) => Block -> Timestamp -> Txs -> m Block
buildBlock prevBlock timestamp txs = do
  let mroot = txMerkleRoot txs
      blockHeader = buildBlockHeader prevBlock mroot timestamp
      blockHeight = (prevBlock ^. bHeight) + 1
  when (null . getTxs $ txs) $ throwError MissingTransactions
  when (blockHeight > 0xFFFFFFFF) $ throwError InvalidHeight
  pure $ Block blockHeader blockHeight txs

txMerkleRoot :: Txs -> Hash
txMerkleRoot (Txs txs) =
  let (_, mroot) = MTree.fromListWithRoot (toList txs)
   in mroot
{-# INLINE txMerkleRoot #-}

hashBlock :: Block -> Hash
hashBlock block =
  let blockHeader = block ^. bHeader
   in toHash blockHeader
{-# INLINE hashBlock #-}

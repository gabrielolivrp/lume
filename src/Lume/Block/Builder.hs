{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Block.Builder (buildBlock, blockSize, bVersion) where

import Control.Lens
import Data.Binary (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (Foldable (toList))
import Data.Word (Word32, Word64)
import GHC.Base (when)
import Lume.Block.Types (Block (..), BlockHeader (..), bHeader, bHeight)
import Lume.Consensus.Difficulty (initialBits)
import Lume.Crypto.Hash (Hash, ToHash (toHash))
import Lume.Crypto.MerkleTree qualified as MTree
import Lume.Time.Timestamp (Timestamp)
import Lume.Transaction.Types (Txs (..))

newtype BuildBlockException = BuildBlockException String
  deriving (Show)

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
    , _bHashPrevBlock = toHash (prevBlock ^. bHeader)
    }

buildBlock :: Block -> Timestamp -> Txs -> Either BuildBlockException Block
buildBlock prevBlock timestamp txs = do
  let mroot = txMerkleRoot txs
      blockHeader = buildBlockHeader prevBlock mroot timestamp
      blockHeight = (prevBlock ^. bHeight) + 1

  when (blockHeight > 0xFFFFFFFF) $
    Left $
      BuildBlockException "Block height exceeds maximum value"

  Right $ Block blockHeader blockHeight txs

txMerkleRoot :: Txs -> Hash
txMerkleRoot (Txs txs) =
  let (_, mroot) = MTree.fromListWithRoot (toList txs)
   in mroot
{-# INLINE txMerkleRoot #-}

blockSize :: Txs -> Word64
blockSize = fromIntegral . BSL.length . encode
{-# INLINE blockSize #-}

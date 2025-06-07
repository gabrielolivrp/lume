{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Block.Genesis (
  genesisBlock,
)
where

import Data.List.NonEmpty qualified as NE
import Data.Word (Word32)
import Lume.Block.Internal
import Lume.Consensus (initialBits)
import Lume.Crypto.Address (Address (Address))
import Lume.Crypto.Hash (Hash, hash')
import Lume.Time.Timestamp (Timestamp (Timestamp))
import Lume.Transaction (Coin (Coin), Tx, Txs (Txs), buildCoinbaseTx)

genesisHashPrev :: Hash
genesisHashPrev = hash' "genesis block"

genesisTimestamp :: Timestamp
genesisTimestamp = Timestamp 1717699200
{-# INLINE genesisTimestamp #-}

genesisNonce :: Word32
genesisNonce = 0
{-# INLINE genesisNonce #-}

genesisCoinbase :: Tx
genesisCoinbase =
  case buildCoinbaseTx $ NE.singleton (genesisAddress, genesisAmount) of
    Left err -> error $ "Failed to build genesis coinbase transaction: " ++ show err
    Right tx -> tx
 where
  genesisAddress = Address "foo"
  genesisAmount = Coin 1000000

genesisMerkleRoot :: Hash
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

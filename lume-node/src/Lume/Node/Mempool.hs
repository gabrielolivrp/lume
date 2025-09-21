{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Mempool where

import Data.Map qualified as M
import Lume.Core (Tx)
import Lume.Core.Crypto.Hash (Hash)

newtype Mempool = Mempool
  { getTxs :: M.Map Hash Tx
  }

empty :: Mempool
empty = Mempool M.empty

insertTx :: Hash -> Tx -> Mempool -> Mempool
insertTx hash tx (Mempool txs) = Mempool $ M.insert hash tx txs

deleteTx :: Hash -> Mempool -> Mempool
deleteTx hash (Mempool txs) = Mempool $ M.delete hash txs

memberTx :: Hash -> Mempool -> Bool
memberTx hash (Mempool txs) = M.member hash txs

lookupTx :: Mempool -> Hash -> Maybe Tx
lookupTx (Mempool txs) hash = M.lookup hash txs

size :: Mempool -> Int
size (Mempool txs) = M.size txs

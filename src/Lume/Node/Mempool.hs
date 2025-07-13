{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Mempool where

import Data.Map qualified as M
import Lume.Core (Tx)
import Lume.Core.Crypto.Hash (Hash)

newtype Mempool = Mempool
  { getMemTxs :: M.Map Hash Tx
  }

emptyMempool :: Mempool
emptyMempool = Mempool M.empty

addTransaction :: Hash -> Tx -> Mempool -> Mempool
addTransaction hash tx (Mempool txs) = Mempool $ M.insert hash tx txs

removeTransaction :: Hash -> Mempool -> Mempool
removeTransaction hash (Mempool txs) = Mempool $ M.delete hash txs

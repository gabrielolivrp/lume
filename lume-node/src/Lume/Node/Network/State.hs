{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Network.State (
  State,
  mkState,
  getPeers,
  addPeer,
  removePeer,
  addTx,
  removeTx,
  getMempool,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.Set qualified as S
import Lume.Core (Tx, txHash)
import Lume.Node.Mempool (Mempool, addTransaction, emptyMempool, removeTransaction)
import Lume.Node.Network.Peer (Peer)

data State = State
  { sPeers :: MVar (S.Set Peer)
  , sMempool :: MVar Mempool
  }

mkState :: IO State
mkState = do
  peers <- newMVar S.empty
  mempool <- newMVar emptyMempool
  pure State{sPeers = peers, sMempool = mempool}

getPeers :: State -> IO (S.Set Peer)
getPeers state = readMVar (sPeers state)

addPeer :: State -> Peer -> IO ()
addPeer state peer = do
  let peers = sPeers state
  modifyMVar_ peers $ \ps -> pure (S.union ps (S.singleton peer))

removePeer :: State -> Peer -> IO ()
removePeer state peer = do
  let peers = sPeers state
  modifyMVar_ peers $ \ps -> pure (S.delete peer ps)

addTx :: State -> Tx -> IO ()
addTx state tx = do
  let mempool = sMempool state
  let hash = txHash tx
  modifyMVar_ mempool $ \mem -> pure (addTransaction hash tx mem)

removeTx :: State -> Tx -> IO ()
removeTx state tx = do
  let mempool = sMempool state
  let hash = txHash tx
  modifyMVar_ mempool $ \mem -> pure (removeTransaction hash mem)

getMempool :: State -> IO Mempool
getMempool state = readMVar (sMempool state)

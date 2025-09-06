{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Network.State (
  NodeContext (..),
  State,
  initState,
  getPeers,
  addPeer,
  getNodeIds,
  removePeer,
  insertMempoolTx,
  deleteMempoolTx,
  lookupMempoolTx,
  memberMempoolTx,
  getPeer,
  updatePeer,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Distributed.Process (NodeId)
import Control.Distributed.Process.Node
import Data.Foldable (find)
import Data.Word
import Lume.Core (Tx, txHash)
import Lume.Core.Crypto.Hash (Hash)
import Lume.Node.Config (Config)
import Lume.Node.Mempool qualified as Mem
import Lume.Node.Network.Peer (Peer (pNodeId))
import Lume.Node.Storage.Database qualified as DB

data NodeContext = NodeContext
  { nLocalNode :: LocalNode
  , nSelfNodeId :: NodeId
  , nState :: State
  , nDatabase :: DB.Database
  , nConfig :: Config
  , nProtocolVersion :: Word32
  , nMinProtocolVersion :: Word32
  }

data State = State
  { sPeers :: MVar [Peer]
  , sMempool :: MVar Mem.Mempool
  }

initState :: IO State
initState = do
  peers <- newMVar []
  mempool <- newMVar Mem.empty
  pure State{sPeers = peers, sMempool = mempool}

getPeers :: State -> IO [Peer]
getPeers state = readMVar (sPeers state)

getNodeIds :: State -> IO [NodeId]
getNodeIds state = do
  peers <- readMVar (sPeers state)
  pure $ map pNodeId peers

getPeer :: State -> NodeId -> IO (Maybe Peer)
getPeer state nid = do
  peers <- readMVar (sPeers state)
  pure $ find (\p -> pNodeId p == nid) peers

updatePeer :: State -> Peer -> IO ()
updatePeer state peer = do
  let peers = sPeers state
  modifyMVar_ peers $ \ps -> do
    let ps' = filter (\p -> pNodeId p /= pNodeId peer) ps
    pure (peer : ps')

addPeer :: State -> Peer -> IO ()
addPeer state peer = do
  let peers = sPeers state
  modifyMVar_ peers $ \ps -> pure (peer : ps)

removePeer :: State -> Peer -> IO ()
removePeer state peer = do
  let peers = sPeers state
  modifyMVar_ peers $ \ps -> do
    let ps' = filter (\p -> pNodeId p /= pNodeId peer) ps
    pure ps'

insertMempoolTx :: State -> Tx -> IO ()
insertMempoolTx state tx = do
  let mempool = sMempool state
  let hash = txHash tx
  modifyMVar_ mempool $ \mem -> pure (Mem.insertTx hash tx mem)

deleteMempoolTx :: State -> Tx -> IO ()
deleteMempoolTx state tx = do
  let mempool = sMempool state
  let hash = txHash tx
  modifyMVar_ mempool $ \mem -> pure (Mem.deleteTx hash mem)

memberMempoolTx :: State -> Hash -> IO Bool
memberMempoolTx state txid = do
  let mempool = sMempool state
  mem <- readMVar mempool
  pure $ Mem.memberTx txid mem

lookupMempoolTx :: State -> Hash -> IO (Maybe Tx)
lookupMempoolTx state txid = do
  mempool <- readMVar (sMempool state)
  pure $ Mem.lookupTx mempool txid

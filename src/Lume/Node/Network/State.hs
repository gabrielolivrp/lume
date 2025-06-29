module Lume.Node.Network.State (
  State,
  mkState,
  getPeers,
  addPeer,
  removePeer,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Set as S
import Lume.Node.Network.Types (Peer)

data State = State
  { statePeers :: MVar (S.Set Peer)
  }

mkState :: IO State
mkState = do
  peers <- newMVar S.empty
  pure State{statePeers = peers}

getPeers :: State -> IO (S.Set Peer)
getPeers state = do
  let peersVar = statePeers state
  readMVar peersVar

addPeer :: State -> Peer -> IO ()
addPeer state peer = do
  let peersVar = statePeers state
  modifyMVar_ peersVar $ \peers -> pure (S.union peers (S.singleton peer))

removePeer :: State -> Peer -> IO ()
removePeer state peer = do
  let peersVar = statePeers state
  modifyMVar_ peersVar $ \peers -> pure (S.delete peer peers)

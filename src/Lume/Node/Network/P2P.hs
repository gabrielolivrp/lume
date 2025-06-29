{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Network.P2P (
  startNode,
)
where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Set as S
import Lume.Node.Config (NodeConfig (ncHost, ncPeers, ncPort))
import Lume.Node.Network.State (State, addPeer, getPeers, mkState, removePeer)
import Lume.Node.Network.Types (Peer (Peer, getNodeId))
import Network.Transport (EndPointAddress (EndPointAddress))
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)

mkNodeId :: String -> NodeId
mkNodeId addr = NodeId . EndPointAddress . BS.concat $ [Char8.pack addr, ":0"]

createLocalNode :: String -> String -> IO LocalNode
createLocalNode host port = do
  let tcpAddr = defaultTCPAddr host port
  result <- createTransport tcpAddr defaultTCPParameters
  case result of
    Left err -> error $ "Error creating transport: " ++ show err
    Right transport -> newLocalNode transport initRemoteTable

startNode :: NodeConfig -> IO ()
startNode config = do
  let host = ncHost config
      port = show $ ncPort config
  localNode <- createLocalNode host port
  state <- mkState
  let bootnodes = map mkNodeId (ncPeers config)
  _ <- forkProcess localNode $ peerController state bootnodes
  runProcess localNode $ messageController state

peerControllerName :: String
peerControllerName = "peer-controller"

peerController :: State -> [NodeId] -> Process ()
peerController state bootnodes = do
  register peerControllerName =<< getSelfPid
  -- Discover peers
  mapM_ discoverPeer bootnodes
  -- Add self state as a peer
  selfNid <- getSelfNode
  liftIO (addPeer state (Peer selfNid))
  -- Send known peers to all discovered bootnodes
  forever $
    receiveWait
      [ match onWhereIsReply -- Handle replies to whereis requests
      , match onPeersReceived -- Handle received peers
      , match onProcessDown -- Handle process down notifications
      ]
 where
  discoverPeer :: NodeId -> Process ()
  discoverPeer nid = do
    say $ "Discovering peer: " ++ show nid
    whereisRemoteAsync nid peerControllerName

  onWhereIsReply :: WhereIsReply -> Process ()
  onWhereIsReply (WhereIsReply _ mpid) = do
    case mpid of
      Nothing -> say "No peer found for the given NodeId"
      Just pid -> do
        let peerNid = processNodeId pid
        say $ "Discovered peer: " ++ show peerNid
        -- Add the discovered peer to the state's peer list
        liftIO (addPeer state (Peer peerNid))
        -- Monitor the peer process
        _ <- monitor pid
        -- Send known peers to the discovered peer
        knownPeers <- liftIO (getPeers state)
        nsendRemote peerNid peerControllerName knownPeers

  onPeersReceived :: S.Set Peer -> Process ()
  onPeersReceived newPeers = do
    say $ "Received peers: " ++ show newPeers
    -- Discover new peers that are not already known
    knownPeers <- liftIO (getPeers state)
    -- Filter out already known peers
    mapM_ (discoverPeer . getNodeId) (newPeers `S.difference` knownPeers)

  onProcessDown :: ProcessMonitorNotification -> Process ()
  onProcessDown (ProcessMonitorNotification _ pid reason) = do
    say $ "Peer process died: " ++ show pid ++ " Reason: " ++ show reason
    let nid = processNodeId pid
    liftIO $ removePeer state (Peer nid)

messageControllerName :: String
messageControllerName = "message-controller"

messageController :: State -> Process ()
messageController _node = do
  register messageControllerName =<< getSelfPid
  selfNid <- getSelfNode
  say $ "Node started with NodeId: " ++ show selfNid
  forever $
    receiveWait
      []

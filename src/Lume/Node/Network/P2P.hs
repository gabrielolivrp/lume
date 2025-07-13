{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Network.P2P (
  startNode,
)
where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Set qualified as S
import Lume.Node.Config (Config (cHost, cPeers, cPort))
import Lume.Node.Network.Peer (Peer (Peer, getNodeId))
import Lume.Node.Network.RPC (startServer)
import Lume.Node.Network.State (State, addPeer, getPeers, mkState, removePeer)
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

startNode :: Config -> IO ()
startNode config = do
  let host = cHost config
      port = show (cPort config)
      bootnodes = map mkNodeId (cPeers config)
  state <- mkState
  localNode <- createLocalNode host port
  void $ forkProcess localNode (rpcController config state)
  void $ forkProcess localNode (peerController state bootnodes)
  runProcess localNode (messageController state)

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
        void $ monitor pid
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

rpcControllerName :: String
rpcControllerName = "rpc-controller"

rpcController :: Config -> State -> Process ()
rpcController config state = do
  say "Starting RPC server"
  register rpcControllerName =<< getSelfPid
  liftIO (startServer config state)

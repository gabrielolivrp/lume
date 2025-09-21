{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Network.P2P (
  startP2PServer,
)
where

import Control.Distributed.Process
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Lume.Node.Config (Config (cHost, cPeers, cPort))
import Lume.Node.Network.Message (onMessageReceived, sendVersion)
import Lume.Node.Network.Peer (HandshakeFlag (..), markPeer, mkPeer)
import Lume.Node.Network.State (NodeContext (NodeContext), addPeer, getPeer, removePeer)
import Network.Transport (EndPointAddress (EndPointAddress))

mkNodeId :: String -> NodeId
mkNodeId addr = NodeId . EndPointAddress . BS.concat $ [Char8.pack addr, ":0"]

startP2PServer :: Config -> NodeContext -> Process ()
startP2PServer config context@(NodeContext _ selfnid state _ _ _ _) = do
  register "p2p-server" =<< getSelfPid

  let host = cHost config
      port = show (cPort config)

  say $ "Starting P2P server on " ++ host ++ ":" ++ port

  let selfPeer = mkPeer selfnid

  -- Add self state as a peer
  liftIO (addPeer state selfPeer)

  let bootnodes = map mkNodeId (cPeers config)

  -- Discover peers
  mapM_ (`whereisRemoteAsync` "p2p-server") bootnodes

  -- Send known peers to all discovered bootnodes
  forever $
    receiveWait
      [ match (onMessageReceived context)
      , match onWhereIsReply
      , match onProcessDown
      ]
 where
  onWhereIsReply :: WhereIsReply -> Process ()
  onWhereIsReply (WhereIsReply _ mpid) = do
    case mpid of
      Nothing -> say "No peer found for the given nid"
      Just pid -> do
        let nid = processNodeId pid
        let newPeer = markPeer SentVersion . mkPeer $ nid
        liftIO (addPeer state newPeer)
        say $ "Discovered new peer: " ++ show nid
        say $ "Sending version message to " ++ show nid
        sendVersion context newPeer
        void $ monitor pid

  onProcessDown :: ProcessMonitorNotification -> Process ()
  onProcessDown (ProcessMonitorNotification _ pid reason) = do
    let nid = processNodeId pid
    say $ "Peer disconnected: " ++ show nid ++ " reason: " ++ show reason
    mpeer <- liftIO $ getPeer state nid
    case mpeer of
      Nothing -> say $ "Unknown peer disconnected: " ++ show nid ++ " reason: " ++ show reason
      Just peer -> liftIO $ removePeer state peer

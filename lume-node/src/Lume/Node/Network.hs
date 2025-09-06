{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Network (startNode) where

import Control.Distributed.Process (getSelfNode, liftIO)
import Control.Distributed.Process.Internal.Types (LocalNode)
import Control.Distributed.Process.Node (forkProcess, initRemoteTable, newLocalNode, runProcess)
import Control.Monad (void)
import Data.Word (Word32)
import Lume.Node.Config (Config (cDataDir, cHost, cPort))
import Lume.Node.Network.P2P (startP2PServer)
import Lume.Node.Network.RPC (startRPCServer)
import Lume.Node.Network.State (NodeContext (NodeContext), initState)
import Lume.Node.Storage.Database qualified as DB
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)

protocolVersion :: Word32
protocolVersion = 1

minProtocolVersion :: Word32
minProtocolVersion = 1

createLocalNode :: String -> String -> IO LocalNode
createLocalNode host port = do
  let tcpAddr = defaultTCPAddr host port
  result <- createTransport tcpAddr defaultTCPParameters
  case result of
    Left err -> fail $ "Error creating transport: " ++ show err
    Right transport -> newLocalNode transport initRemoteTable

startNode :: Config -> IO ()
startNode config = do
  let host = cHost config
      port = show (cPort config)
  DB.runDatabaseM $ do
    database <- DB.openDatabase (cDataDir config)
    state <- liftIO initState
    localNode <- liftIO (createLocalNode host port)

    let mkContext selfnid = NodeContext localNode selfnid state database config protocolVersion minProtocolVersion

    void . liftIO $ forkProcess localNode $ do
      selfnid <- getSelfNode
      startRPCServer config (mkContext selfnid)

    liftIO . runProcess localNode $ do
      selfnid <- getSelfNode
      startP2PServer config (mkContext selfnid)

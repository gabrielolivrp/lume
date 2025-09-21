{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lume.Node.Network.Message where

import Control.Distributed.Process (NodeId, Process, getSelfNode, nsendRemote, say, whereisRemoteAsync)
import Control.Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Binary (Binary)
import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Word
import GHC.Generics (Generic)
import Lume.Core (Block, BlockHeader, Tx, bHeader, bHeight, blockHash, txHash)
import Lume.Core.Crypto.Hash (Hash, hash')
import Lume.Node.Chain (getBestBlock, getBestHeight, getBlockByHash, getBlockByHeight, runChainM, validateTransaction)
import Lume.Node.Network.Peer (HandshakeFlag (..), Peer (..), hasFlag, isReady, markPeer, mkPeer)
import Lume.Node.Network.State
import System.Random (randomRIO)

data Version = Version
  { vVersion :: Word32
  , vBestHeight :: Word64
  , vAddrFrom :: NodeId
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

newtype Addr = Addr
  { getAddrs :: [NodeId]
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

data InvType
  = InvTx
  | InvBlock
  deriving (Show, Generic)
  deriving anyclass (Binary)

newtype Inv = Inv
  { getInventory :: [(InvType, Hash)]
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

newtype GetData = GetData
  { getData :: [(InvType, Hash)]
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

newtype Headers = Headers
  { getHeaders :: [BlockHeader]
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

data GetHeaders = GetHeaders
  { ghVersion :: Word32
  , ghBlockLocatorHashes :: [Hash]
  , ghHashStop :: Hash
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

data MsgHeader = MsgHeader
  { mTtl :: Int
  , mSender :: NodeId
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

data MsgPayload
  = MVersion Version
  | MVerAck
  | MAddr Addr
  | MGetAddr
  | MInv Inv
  | MTx Tx
  | MGetData GetData
  | MGetHeaders GetHeaders
  | MHeaders Headers
  deriving (Show, Generic)
  deriving anyclass (Binary)

data Msg = Msg
  { mHeader :: MsgHeader
  , mPayload :: MsgPayload
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

mkTxInv :: Hash -> MsgPayload
mkTxInv hash = MInv (Inv [(InvTx, hash)])

mkTxBlock :: Hash -> MsgPayload
mkTxBlock hash = MInv (Inv [(InvBlock, hash)])

mkGetTxData :: Hash -> MsgPayload
mkGetTxData hash = MGetData (GetData [(InvTx, hash)])

onMessageReceived :: NodeContext -> Msg -> Process ()
onMessageReceived context msg@Msg{mHeader, mPayload} = do
  let nid = mSender mHeader
  case mPayload of
    MVersion version -> handleVersion context version
    MVerAck -> handleVerAck context nid
    MAddr addr -> handleAddr context addr
    MGetAddr -> handleGetAddr context nid
    MInv inv -> handleInv context inv nid >> forward context msg
    MGetData data' -> handleGetData context data' nid
    MTx tx -> handleTx context tx
    MGetHeaders headers -> handleGetHeaders context nid headers
    MHeaders headers -> handleHeaders context headers

handleVersion :: NodeContext -> Version -> Process ()
handleVersion context@NodeContext{nState, nMinProtocolVersion} (Version version bestHeight addrFrom)
  | version < nMinProtocolVersion =
      say ("Peer version " ++ show version ++ " is below minimum protocol version " ++ show nMinProtocolVersion ++ ", disconnecting from nid: " ++ show addrFrom)
  | otherwise = do
      mpeer <- liftIO (getPeer nState addrFrom)
      maybe handleNewPeer handleExistingPeer mpeer
 where
  handleNewPeer :: Process ()
  handleNewPeer = do
    say $ "No peer found for nid: " ++ show addrFrom ++ ", adding new peer"
    let newPeer = foldr markPeer (mkPeer addrFrom) [GotVersion, SentVerAck, SentVersion]
    liftIO $ addPeer nState (newPeer{pBestHeight = Just bestHeight})
    sendVerAck newPeer
    sendVersion context newPeer

  handleExistingPeer :: Peer -> Process ()
  handleExistingPeer peer = do
    say $ "Peer found for nid: " ++ show addrFrom
    unless (hasFlag GotVersion peer) $ do
      let updatedPeer = foldr markPeer peer [GotVersion, SentVerAck]
      liftIO (updatePeer nState (updatedPeer{pBestHeight = Just bestHeight}))
      sendVerAck updatedPeer
      onPeerReady context updatedPeer

handleHeaders :: NodeContext -> Headers -> Process ()
handleHeaders _ (Headers headers) = do
  say $ "Received headers with " ++ show (length headers) ++ " headers"
  forM_ headers $ \header -> do
    say $ "Header: " ++ show header

handleGetHeaders :: NodeContext -> NodeId -> GetHeaders -> Process ()
handleGetHeaders context nid (GetHeaders _ locatorHashes hashStop) = do
  commonAncestor <- liftIO $ findCommonAncestor context locatorHashes
  case commonAncestor of
    Nothing -> do
      say $ "No common ancestor found for getheaders from nid: " ++ show nid
      unicast nid (MHeaders (Headers []))
    Just ancestor -> do
      headers <- liftIO $ collectHeaders context ancestor hashStop 2000
      unicast nid (MHeaders $ Headers (map (^. bHeader) headers))

findCommonAncestor :: NodeContext -> [Hash] -> IO (Maybe Block)
findCommonAncestor ctx = go
 where
  go [] = pure Nothing
  go (h : hs) = do
    mb <- runChainM (nConfig ctx) (nDatabase ctx) (getBlockByHash h)
    case mb of
      Right (Just blk) -> pure (Just blk)
      Right Nothing -> go hs
      Left _ -> go hs

collectHeaders :: NodeContext -> Block -> Hash -> Word64 -> IO [Block]
collectHeaders ctx ancestor hashStop limit = go (ancestor ^. bHeader . bHeight + 1) [] 0
 where
  go _ acc n | n >= limit = pure acc
  go height acc n = do
    mb <- runChainM (nConfig ctx) (nDatabase ctx) (getBlockByHeight height)
    case mb of
      Right (Just blk) ->
        if blockHash blk == hashStop
          then pure (acc ++ [blk])
          else go (height + 1) (acc ++ [blk]) (n + 1)
      Right Nothing -> pure acc
      _ -> pure acc

handleVerAck :: NodeContext -> NodeId -> Process ()
handleVerAck context@NodeContext{nState} nid = do
  say $ "Received verack from nid: " ++ show nid
  mpeer <- liftIO $ getPeer nState nid
  forM_ mpeer $ \peer -> do
    let updatedPeer = markPeer GotVerAck peer
    liftIO (updatePeer nState updatedPeer)
    onPeerReady context updatedPeer

onPeerReady :: NodeContext -> Peer -> Process ()
onPeerReady context peer = when (isReady peer) $ do
  sendGetAddr peer
  syncChain context peer

syncChain :: NodeContext -> Peer -> Process ()
syncChain context peer = do
  forM_ (pBestHeight peer) $ \bestHeightPeer -> do
    bestBlockResult <- liftIO $ runChainM (nConfig context) (nDatabase context) getBestBlock
    case bestBlockResult of
      Left err -> say $ "Error getting best block height: " ++ show err
      Right (Just bestHeight)
        | (bestHeight ^. bHeader . bHeight) < bestHeightPeer -> do
            say $ "Local chain is behind peer " ++ show (pNodeId peer) ++ ", syncing..."
            locatorHashes <- liftIO $ makeLocator context (bestHeight ^. bHeader . bHeight)
            let getHeaders = GetHeaders (nProtocolVersion context) locatorHashes (hash' BS.empty)
            unicast (pNodeId peer) (MGetHeaders getHeaders)
        | otherwise -> say $ "Local chain is up to date with peer " ++ show (pNodeId peer)
      Right Nothing -> say "No best block found in local chain, cannot sync"

handleAddr :: NodeContext -> Addr -> Process ()
handleAddr NodeContext{nState} (Addr addrs) = do
  say $ "Received addr with " ++ show (length addrs) ++ " addresses"
  knownNodeIds <- liftIO (getNodeIds nState)
  mapM_ discovery (S.fromList addrs `S.difference` S.fromList knownNodeIds)
 where
  discovery nid = whereisRemoteAsync nid "p2p-server"

handleGetAddr :: NodeContext -> NodeId -> Process ()
handleGetAddr context@NodeContext{nState} nid = do
  say $ "Received getaddr from nid: " ++ show nid
  mpeer <- liftIO (getPeer nState nid)
  forM_ mpeer $ \peer -> do
    say $ "Peer found for getaddr from nid: " ++ show nid
    sendAddr context peer

handleInv :: NodeContext -> Inv -> NodeId -> Process ()
handleInv NodeContext{nState} (Inv inv) nid =
  forM_ inv $ \(itype, hash) -> do
    case itype of
      InvTx -> handleInvTx hash
      InvBlock -> say "Received Inv message of type Block"
 where
  handleInvTx txid = do
    say $ "Received Inv message of type Tx for hash: " ++ show txid
    result <- liftIO (memberMempoolTx nState txid)
    unless result $ unicast nid (mkGetTxData txid)

handleGetData :: NodeContext -> GetData -> NodeId -> Process ()
handleGetData NodeContext{nState} (GetData items) nid =
  forM_ items $ \(itype, hash) -> do
    case itype of
      InvTx -> handleGetTxData hash
      InvBlock -> say "Received getdata of type Block"
 where
  handleGetTxData txid = do
    say $ "Received getdata of type Tx for txid: " ++ show txid
    result <- liftIO (memberMempoolTx nState txid)
    when result $ do
      mtx <- liftIO (lookupMempoolTx nState txid)
      forM_ mtx $ \tx -> unicast nid (MTx tx)

handleTx :: NodeContext -> Tx -> Process ()
handleTx context@NodeContext{nState} tx = do
  let txid = txHash tx
  say $ "Received Tx with hash: " ++ show txid
  hasTx <- liftIO (memberMempoolTx nState txid)
  unless hasTx $ do
    txValidated <- liftIO $ runChainM (nConfig context) (nDatabase context) (validateTransaction tx)
    case txValidated of
      Left err -> say $ "Transaction validation failed: " ++ show err
      Right () -> do
        say $ "Added Tx to mempool with hash: " ++ show txid
        liftIO (insertMempoolTx nState tx)
        broadcast context (mkTxInv txid)

makeLocator :: NodeContext -> Word64 -> IO [Hash]
makeLocator NodeContext{nConfig, nDatabase} bestHeight = go bestHeight 1 [] 0
 where
  go :: Word64 -> Word64 -> [Hash] -> Int -> IO [Hash]
  go height step acc count
    | height == 0 = do
        g <- runChainM nConfig nDatabase (getBlockByHeight 0)
        case g of
          Right Nothing -> pure acc
          Right (Just genesis) -> pure (acc ++ [blockHash genesis])
          Left _ -> pure acc
    | otherwise = do
        h <- runChainM nConfig nDatabase (getBlockByHeight height)
        case h of
          Right Nothing -> pure acc
          Right (Just h') -> do
            let acc' = acc ++ [blockHash h']
                height' = if height > step then height - step else 0
                step' = if count >= 10 then step * 2 else step
            go height' step' acc' (count + 1)
          Left _ -> pure acc

sendVersion :: NodeContext -> Peer -> Process ()
sendVersion context@NodeContext{nSelfNodeId, nProtocolVersion} peer = do
  bestHeightResult <- liftIO $ runChainM (nConfig context) (nDatabase context) getBestHeight
  case bestHeightResult of
    Left err -> say $ "Error getting best block height: " ++ show err
    Right bestHeight -> do
      let version = Version nProtocolVersion bestHeight nSelfNodeId
      unicast (pNodeId peer) (MVersion version)

sendVerAck :: Peer -> Process ()
sendVerAck peer = unicast (pNodeId peer) MVerAck

sendAddr :: NodeContext -> Peer -> Process ()
sendAddr NodeContext{nSelfNodeId, nState} peer = do
  peers <- liftIO (getPeers nState)
  let addrs = [pNodeId p | p <- peers, pNodeId p /= pNodeId peer && (pNodeId p /= nSelfNodeId)]
  unicast (pNodeId peer) (MAddr (Addr addrs))

sendGetAddr :: Peer -> Process ()
sendGetAddr peer = unicast (pNodeId peer) MGetAddr

unicast :: NodeId -> MsgPayload -> Process ()
unicast nid payload = do
  selfnid <- getSelfNode
  let msg = Msg (MsgHeader 0 selfnid) payload
  nsendRemote nid "p2p-server" msg

multicast :: NodeContext -> Maybe NodeId -> Msg -> Process ()
multicast context except msg = do
  peers <- liftIO (getRandomPeers context except 2)
  forM_ peers $ \peer ->
    when (isReady peer) $ nsendRemote (pNodeId peer) "p2p-server" msg

broadcast :: NodeContext -> MsgPayload -> Process ()
broadcast context payload = do
  selfnid <- getSelfNode
  let msg = Msg (MsgHeader 3 selfnid) payload
  multicast context Nothing msg

forward :: NodeContext -> Msg -> Process ()
forward context msg@Msg{mHeader} = do
  let newTtl = mTtl mHeader - 1
  let except = Just (mSender mHeader)
  let msg' = msg{mHeader = mHeader{mTtl = newTtl}}
  when (newTtl > 0) $ multicast context except msg'

getRandomPeers :: NodeContext -> Maybe NodeId -> Int -> IO [Peer]
getRandomPeers (NodeContext{nSelfNodeId, nState}) except n = do
  peers <- getPeers nState
  let availablePeers =
        filter (\p -> pNodeId p /= nSelfNodeId && Just (pNodeId p) /= except) peers
  if null availablePeers
    then pure []
    else shuffleList (min n (length availablePeers)) availablePeers
 where
  shuffleList :: Int -> [a] -> IO [a]
  shuffleList n' xs = mapM (const $ random xs) [1 .. n']

  random :: [a] -> IO a
  random xs = do
    i <- randomRIO (0, length xs - 1)
    pure (xs !! i)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Network.Peer where

import Control.Distributed.Process (NodeId)
import Data.Binary (Binary)
import Data.Set qualified as S
import Data.Word
import GHC.Generics

data HandshakeFlag
  = SentVersion
  | GotVersion
  | SentVerAck
  | GotVerAck
  deriving (Eq, Ord, Show, Generic, Binary)

data Peer = Peer
  { pNodeId :: NodeId
  , pFlags :: S.Set HandshakeFlag
  , pBestHeight :: Maybe Word64
  }
  deriving (Eq, Ord, Show, Generic, Binary)

mkPeer :: NodeId -> Peer
mkPeer nid = Peer nid S.empty Nothing

markPeer :: HandshakeFlag -> Peer -> Peer
markPeer flag peer = peer{pFlags = S.insert flag (pFlags peer)}

hasFlag :: HandshakeFlag -> Peer -> Bool
hasFlag flag peer = S.member flag (pFlags peer)

isReady :: Peer -> Bool
isReady peer =
  all
    (`S.member` pFlags peer)
    [ SentVersion
    , GotVersion
    , SentVerAck
    , GotVerAck
    ]

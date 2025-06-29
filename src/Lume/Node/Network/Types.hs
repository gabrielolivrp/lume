{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lume.Node.Network.Types where

import Control.Distributed.Process (NodeId)
import Data.Binary (Binary)
import GHC.Generics

newtype Peer = Peer {getNodeId :: NodeId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

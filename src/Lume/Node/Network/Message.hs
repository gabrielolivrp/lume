{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lume.Node.Network.Message where

import Data.Binary (Binary)
import Data.Word (Word32)
import GHC.Generics
import Lume.Core.Crypto.Address

data Msg
  = VersionMsg
  { version :: Word32
  , bestHeight :: Word32
  , addrFrom :: Address
  }
  deriving (Show, Generic)
  deriving anyclass (Binary)

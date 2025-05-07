{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Consensus.PoW where

import Data.Binary (Binary)
import Data.Word (Word32)
import GHC.Generics (Generic)

newtype Nonce = Nonce Word32
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (Binary)

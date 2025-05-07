{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Types where

import Data.Binary (Binary)
import Data.Word (Word32)
import GHC.Generics (Generic)

newtype Version = Version Word32
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (Binary)

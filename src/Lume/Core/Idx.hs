{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Core.Idx where

import Data.Binary (Binary)
import Data.Word (Word64)
import GHC.Generics (Generic)

newtype Idx = Idx Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (Binary)

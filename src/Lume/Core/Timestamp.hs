{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Core.Timestamp where

import Data.Binary
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics

newtype Timestamp = Timestamp Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (Binary)

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp . floor <$> getPOSIXTime
{-# INLINE getCurrentTimestamp #-}

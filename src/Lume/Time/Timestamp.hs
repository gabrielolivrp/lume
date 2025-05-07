{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Time.Timestamp where

import Data.Binary (Binary)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import GHC.Generics

newtype Timestamp = Timestamp Word32
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Real, Integral, Enum, Bounded)
  deriving anyclass (Binary)

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp . floor <$> getPOSIXTime
{-# INLINE getCurrentTimestamp #-}

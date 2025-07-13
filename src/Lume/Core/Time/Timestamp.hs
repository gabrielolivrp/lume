{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Core.Time.Timestamp where

import Data.Aeson
import Data.Binary (Binary (get, put))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import GHC.Generics

newtype Timestamp = Timestamp Word32
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Num, Real, Integral, Enum, Bounded)

instance Binary Timestamp where
  put (Timestamp ts) = put ts
  get = Timestamp <$> get

instance ToJSON Timestamp where
  toJSON (Timestamp ts) = toJSON ts

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp . floor <$> getPOSIXTime
{-# INLINE getCurrentTimestamp #-}

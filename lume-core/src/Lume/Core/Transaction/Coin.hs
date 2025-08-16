{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Core.Transaction.Coin (
  -- * Types
  Coin (..),

  -- * Functions
  maxCoin,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (get, put))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Text.Printf

newtype Coin = Coin Natural
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)
  deriving newtype (Num, Enum, Real, Integral)

instance Binary Coin where
  put (Coin n) = put n
  get = Coin <$> get

instance Show Coin where
  show = format

coin :: Coin
coin = Coin 100000000 -- 1 Coin = 100,000,000
{-# INLINE coin #-}

maxCoin :: Coin
maxCoin = Coin (21000000 * fromIntegral coin)
{-# INLINE maxCoin #-}

format :: Coin -> String
format (Coin n) = printf "%.8f" (fromIntegral n / fromIntegral coin :: Double)
{-# INLINE format #-}

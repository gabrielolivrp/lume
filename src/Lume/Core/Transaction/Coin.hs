{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Core.Transaction.Coin (
  -- * Types
  Coin (..),

  -- * Functions
  maxCoin,
  safeSubtract,
) where

import Data.Binary (Binary (get, put))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Text.Printf

newtype Coin = Coin Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral)

instance Binary Coin where
  put (Coin n) = put n
  get = Coin <$> get

instance Show Coin where
  show (Coin n) = printf "%.8f" (fromIntegral n / 100000000 :: Double)

maxCoin :: Coin
maxCoin = Coin 2100000000000000
{-# INLINE maxCoin #-}

safeSubtract :: Coin -> Coin -> Maybe Coin
safeSubtract (Coin a) (Coin b)
  | a >= b = Just (Coin (a - b))
  | otherwise = Nothing
{-# INLINE safeSubtract #-}

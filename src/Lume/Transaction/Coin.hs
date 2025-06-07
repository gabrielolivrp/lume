{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Transaction.Coin (
  -- * Types
  Coin (..),

  -- * Functions
  maxCoin,
  safeSubtract,
) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

newtype Coin = Coin Natural
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral)
  deriving anyclass (Binary)

maxCoin :: Coin
maxCoin = Coin 2100000000000000
{-# INLINE maxCoin #-}

safeSubtract :: Coin -> Coin -> Maybe Coin
safeSubtract (Coin a) (Coin b)
  | a >= b = Just (Coin (a - b))
  | otherwise = Nothing
{-# INLINE safeSubtract #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lume.Transaction.Amount (
  Amount (..),
  safeSubtract,
  maxAmount,
) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

newtype Amount = Amount Natural
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral)
  deriving anyclass (Binary)

maxAmount :: Amount
maxAmount = Amount 2100000000000000
{-# INLINE maxAmount #-}

safeSubtract :: Amount -> Amount -> Maybe Amount
safeSubtract (Amount a) (Amount b)
  | a >= b = Just (Amount (a - b))
  | otherwise = Nothing
{-# INLINE safeSubtract #-}

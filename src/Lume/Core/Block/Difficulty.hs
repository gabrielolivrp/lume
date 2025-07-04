{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Core.Block.Difficulty (
  -- * Types
  Bits (..),
  Target (..),

  -- * Constants
  initialBits,
  maximumTarget,

  -- * Functions
  toTarget,
  fromTarget,
  adjustDifficulty,
)
where

import Data.Binary
import Data.Bits qualified as B
import GHC.Generics (Generic)
import Lume.Core.Time.Timestamp (Timestamp (Timestamp))

newtype Bits = Bits Word32
  deriving (Show, Eq, Ord, Generic)

instance Binary Bits where
  put (Bits bits) = put bits
  get = Bits <$> get

newtype Target = Target Integer
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Num, Real, Integral, Enum)

instance Binary Target where
  put (Target target) = put target
  get = Target <$> get

initialBits :: Bits
initialBits = Bits 0x1d00ffff
{-# INLINE initialBits #-}

maximumTarget :: Target
maximumTarget = Target 0x00000000FFFF0000000000000000000000000000000000000000000000000000
{-# INLINE maximumTarget #-}

toTarget :: Bits -> Target
toTarget (Bits bits) =
  let exponent' = fromIntegral $ bits `B.shiftR` 24
      coefficient = fromIntegral $ bits B..&. 0x007fffff
   in Target $ coefficient `B.shiftL` (8 * (exponent' - 3))

fromTarget :: Target -> Bits
fromTarget (Target target)
  | target <= 0 = Bits 0
  | otherwise =
      let msb = findMostSignificantByte target
          (initialCoef, initialExp) = getCoefAndExp target msb
          (coef, exp') = normalize initialCoef initialExp
       in Bits (packBits coef exp')
 where
  packBits :: Integer -> Int -> Word32
  packBits c e = (fromIntegral e `B.shiftL` 24) B..|. fromIntegral (c B..&. 0x007fffff)

  findMostSignificantByte :: Integer -> Int
  findMostSignificantByte 0 = 0
  findMostSignificantByte x = 1 + findMostSignificantByte (x `B.shiftR` 8)

  getCoefAndExp :: Integer -> Int -> (Integer, Int)
  getCoefAndExp target' pos
    | pos <= 3 = (target' `B.shiftL` (8 * (3 - pos)), pos)
    | otherwise = (target' `B.shiftR` (8 * (pos - 3)), pos)

  normalize :: Integer -> Int -> (Integer, Int)
  normalize c e
    | c B..&. 0x00800000 /= 0 = (c `B.shiftR` 8, e + 1)
    | otherwise = (c, e)

adjustDifficulty :: Bits -> Timestamp -> Timestamp -> Bits
adjustDifficulty currentBits (Timestamp actualTimeSeconds) (Timestamp expectedTimeSeconds) =
  let
    currentTarget = toTarget currentBits

    timeRatio = toRational actualTimeSeconds / toRational expectedTimeSeconds

    boundedRatio = max 0.25 (min timeRatio 4.0)
    newTargetFloat = toRational currentTarget * boundedRatio
    newTarget = round newTargetFloat

    boundedTarget = min newTarget maximumTarget

    newBits = fromTarget boundedTarget
   in
    newBits

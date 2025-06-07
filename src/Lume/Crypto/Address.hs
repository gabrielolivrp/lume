{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Crypto.Address (
  -- * Types
  Address (..),
  AddressError (..),

  -- * Functions
  fromPublicKey,
  toPublicKey,
  isValidAddress,
  prefix,
)
where

import Codec.Binary.Bech32
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lume.Crypto.Signature (PublicKey, fromRawBytes, toRawBytes)

data AddressError
  = EncodingFailed
  | DecodingFailed
  | InvalidAddress
  | InvalidHumanReadablePart
  deriving (Show, Eq)

newtype Address = Address Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

prefix :: Text
prefix = "lume_addr_"

makeHumanReadablePart :: Text -> Either AddressError HumanReadablePart
makeHumanReadablePart hrp =
  case humanReadablePartFromText hrp of
    Left _err -> Left InvalidHumanReadablePart
    Right hrp' -> Right hrp'

encodeAddress :: HumanReadablePart -> DataPart -> Either AddressError Address
encodeAddress hrp dataPart =
  case encode hrp dataPart of
    Left _err -> Left EncodingFailed
    Right addr -> Right (Address addr)

decodeAddress :: Address -> Either AddressError (HumanReadablePart, DataPart)
decodeAddress (Address addr) =
  case decode addr of
    Left _err -> Left DecodingFailed
    Right (hrp, dp) -> Right (hrp, dp)

fromPublicKey :: PublicKey -> Either AddressError Address
fromPublicKey pk = do
  hrp <- makeHumanReadablePart prefix
  let addrBytes = toRawBytes pk
      dp = dataPartFromBytes addrBytes
  encodeAddress hrp dp

toPublicKey :: Address -> Either AddressError PublicKey
toPublicKey addr = do
  (_, dp) <- decodeAddress addr
  case dataPartToBytes dp >>= fromRawBytes of
    Nothing -> Left InvalidAddress
    Just addrBytes -> Right addrBytes

isValidAddress :: Address -> Bool
isValidAddress addr =
  case decodeAddress addr of
    Left _ -> False
    Right _ -> True
{-# INLINE isValidAddress #-}

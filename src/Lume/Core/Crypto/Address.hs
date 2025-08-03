{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Crypto.Address (
  -- * Types
  Address (..),
  AddressError (..),

  -- * Functions
  fromPublicKey,
  toPublicKey,
  isValidAddress,
  prefix,
  mkAddress,
)
where

import Codec.Binary.Bech32
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Binary (Binary (get, put))
import Data.Text (Text)
import GHC.Generics (Generic)
import Lume.Core.Crypto.Signature qualified as Sig

data AddressError
  = EncodingFailed
  | DecodingFailed
  | InvalidAddress
  | InvalidHumanReadablePart
  deriving (Show, Eq)

newtype Address = Address Text
  deriving stock (Show, Eq, Generic)

instance Binary Address where
  put (Address addr) = put addr
  get = Address <$> get

instance ToJSON Address where
  toJSON (Address addr) = toJSON addr

instance FromJSON Address where
  parseJSON = withText "Address" $ \t ->
    case mkAddress t of
      Left _ -> fail "Invalid address format"
      Right addr -> pure addr

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

fromPublicKey :: Sig.PublicKey -> Either AddressError Address
fromPublicKey pk = do
  hrp <- makeHumanReadablePart prefix
  let addrBytes = Sig.toRawBytes pk
      dp = dataPartFromBytes addrBytes
  encodeAddress hrp dp

toPublicKey :: Address -> Either AddressError Sig.PublicKey
toPublicKey addr = do
  (_, dp) <- decodeAddress addr
  case dataPartToBytes dp >>= Sig.fromRawBytes of
    Nothing -> Left InvalidAddress
    Just addrBytes -> Right addrBytes

isValidAddress :: Address -> Bool
isValidAddress addr =
  case decodeAddress addr of
    Left _ -> False
    Right _ -> True
{-# INLINE isValidAddress #-}

mkAddress :: Text -> Either AddressError Address
mkAddress addrText = do
  hrp <- makeHumanReadablePart prefix
  case decodeAddress (Address addrText) of
    Left _ -> Left InvalidAddress
    Right (hrp', dp) ->
      if hrp' == hrp
        then encodeAddress hrp dp
        else Left InvalidHumanReadablePart
{-# INLINE mkAddress #-}

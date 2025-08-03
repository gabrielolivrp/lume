{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}

module Lume.Core.Crypto.Hash (
  -- * Types
  Hash (..),
  ToHash (..),

  -- * Functions
  hash',
  toHex,
  fromHex,
  toRawBytes,
  fromRawBytes,
  hash2Integer,
)
where

import Crypto.Hash (Digest, SHA256, digestFromByteString, hash)
import Data.Aeson (FromJSON, ToJSON (toJSON), withText)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Binary (Binary (get, put), Get, encode)
import Data.Bits (Bits (shiftL, (.|.)))
import Data.ByteArray (convert)
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

class ToHash a where
  toHash :: a -> Hash
  default toHash :: (Binary a) => a -> Hash
  toHash = hash' . BS.toStrict . encode
  {-# INLINE toHash #-}

newtype Hash = Hash (Digest SHA256)
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)

instance Binary Hash where
  put (Hash digest) = put (convert digest :: BS.ByteString)
  get = do
    bs <- get :: Get BS.ByteString
    case digestFromByteString bs of
      Just d -> pure (Hash d)
      Nothing -> fail "Invalid SHA256 digest"

instance ToJSON Hash where
  toJSON h = toJSON (Char8.unpack $ toHex h)

instance FromJSON Hash where
  parseJSON = withText "Hash" $ \txt -> do
    let bs = TE.encodeUtf8 txt
    case fromHex bs of
      Just decoded -> pure decoded
      Nothing -> fail "Invalid hash format"

instance ToHash Hash where
  toHash = hash' . toRawBytes

instance ToHash BS.ByteString

hash' :: BS.ByteString -> Hash
hash' = Hash . hash
{-# INLINE hash' #-}

toHex :: Hash -> BS.ByteString
toHex (Hash digest) = BAE.convertToBase BAE.Base16 digest
{-# INLINE toHex #-}

fromHex :: BS.ByteString -> Maybe Hash
fromHex bs = case BAE.convertFromBase BAE.Base16 bs of
  Left _ -> Nothing
  Right rawBytes -> fromRawBytes rawBytes
{-# INLINE fromHex #-}

toRawBytes :: Hash -> BS.ByteString
toRawBytes (Hash digest) = convert digest
{-# INLINE toRawBytes #-}

fromRawBytes :: BS.ByteString -> Maybe Hash
fromRawBytes bs = Hash <$> digestFromByteString bs
{-# INLINE fromRawBytes #-}

hash2Integer :: Hash -> Integer
hash2Integer (Hash digest) = BS.foldr step 0 (convert digest)
 where
  step byte acc = fromIntegral byte .|. (acc `shiftL` 8)

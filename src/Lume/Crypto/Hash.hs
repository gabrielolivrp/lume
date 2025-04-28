{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Crypto.Hash where

import Crypto.Hash (Digest, SHA256, digestFromByteString, hash)
import Data.Binary (Binary (get, put), Get, encode)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
  put (Hash digest) = put (convert digest :: ByteString)
  get = do
    bs <- get :: Get ByteString
    case digestFromByteString bs of
      Just d -> return (Hash d)
      Nothing -> fail "Invalid SHA256 digest"

instance ToHash Hash where
  toHash = id

instance ToHash BS.ByteString

hash' :: ByteString -> Hash
hash' = Hash . hash
{-# INLINE hash' #-}

toHex :: Hash -> BS.ByteString
toHex (Hash digest) = BAE.convertToBase BAE.Base16 digest
{-# INLINE toHex #-}

toRawBytes :: Hash -> BS.ByteString
toRawBytes (Hash digest) = convert digest
{-# INLINE toRawBytes #-}

fromRawBytes :: BS.ByteString -> Maybe Hash
fromRawBytes bs = Hash <$> digestFromByteString bs
{-# INLINE fromRawBytes #-}

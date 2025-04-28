{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Crypto.Signature where

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)

newtype Signature = Signature Ed25519.Signature
  deriving stock (Show, Eq, Generic)

instance Binary Signature where
  get = do
    bs <- getByteString Ed25519.signatureSize
    case Ed25519.signature bs of
      CryptoPassed sig -> pure (Signature sig)
      CryptoFailed err -> fail $ "Invalid Ed25519 signature: " ++ show err
  put (Signature sig) = putByteString (convert sig)

newtype PrivateKey = PrivateKey Ed25519.SecretKey
  deriving stock (Show, Eq, Generic)

instance Binary PrivateKey where
  get = do
    bs <- getByteString Ed25519.secretKeySize
    case Ed25519.secretKey bs of
      CryptoPassed sk -> pure (PrivateKey sk)
      CryptoFailed err -> fail $ "Invalid Ed25519 secret key: " ++ show err
  put (PrivateKey sk) = putByteString (convert sk)

newtype PublicKey = PublicKey Ed25519.PublicKey
  deriving stock (Show, Eq, Generic)

instance Binary PublicKey where
  get = do
    bs <- getByteString Ed25519.publicKeySize
    case Ed25519.publicKey bs of
      CryptoPassed pk -> pure (PublicKey pk)
      CryptoFailed err -> fail $ "Invalid Ed25519 public key: " ++ show err

  put (PublicKey pk) = putByteString (convert pk)

newtype KeyPair = KeyPair (PublicKey, PrivateKey)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

generateKeyPair :: IO KeyPair
generateKeyPair = do
  sk <- Ed25519.generateSecretKey
  let pk = Ed25519.toPublic sk
  pure $ KeyPair (PublicKey pk, PrivateKey sk)

sign :: PrivateKey -> BS.ByteString -> Signature
sign (PrivateKey sk) = Signature . Ed25519.sign sk (Ed25519.toPublic sk)
{-# INLINE sign #-}

verify :: PublicKey -> BS.ByteString -> Signature -> Bool
verify (PublicKey pk) xs (Signature sig) = Ed25519.verify pk xs sig
{-# INLINE verify #-}

toPublicKey :: PrivateKey -> PublicKey
toPublicKey (PrivateKey sk) = PublicKey (Ed25519.toPublic sk)
{-# INLINE toPublicKey #-}

toRawBytes :: PublicKey -> BS.ByteString
toRawBytes (PublicKey pk) = convert pk
{-# INLINE toRawBytes #-}

fromRawBytes :: BS.ByteString -> Maybe PublicKey
fromRawBytes bs = case Ed25519.publicKey bs of
  CryptoPassed pk -> Just (PublicKey pk)
  CryptoFailed _ -> Nothing
{-# INLINE fromRawBytes #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Crypto.Signature (
  Signature (..),
  PrivateKey (..),
  PublicKey (..),
  KeyPair (..),
  generateKeyPair,
  sign,
  verify,
  toPublicKey,
  toRawBytes,
  fromRawBytes,
  emptySignature,
  emptyPublicKey,
) where

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)

newtype Signature = Signature BS.ByteString
  deriving stock (Show, Eq, Generic)

instance Binary Signature where
  get = Signature <$> getByteString Ed25519.signatureSize
  put (Signature sig)
    | BS.length sig == Ed25519.signatureSize = putByteString sig
    | otherwise = error "Invalid signature size"

newtype PrivateKey = PrivateKey BS.ByteString
  deriving stock (Show, Eq, Generic)

instance Binary PrivateKey where
  get = PrivateKey <$> getByteString Ed25519.secretKeySize
  put (PrivateKey sk)
    | BS.length sk == Ed25519.secretKeySize = putByteString sk
    | otherwise = error "Invalid private key size"

newtype PublicKey = PublicKey BS.ByteString
  deriving stock (Show, Eq, Generic)

instance Binary PublicKey where
  get = PublicKey <$> getByteString Ed25519.publicKeySize
  put (PublicKey pk)
    | BS.length pk == Ed25519.publicKeySize = putByteString pk
    | otherwise = error "Invalid public key size"

newtype KeyPair = KeyPair (PublicKey, PrivateKey)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

generateKeyPair :: IO KeyPair
generateKeyPair = do
  sk <- Ed25519.generateSecretKey
  let pk = Ed25519.toPublic sk
  pure $ KeyPair (PublicKey . convert $ pk, PrivateKey . convert $ sk)

sign :: PrivateKey -> BS.ByteString -> Signature
sign (PrivateKey sk) xs = case Ed25519.secretKey sk of
  CryptoPassed sk' ->
    let pk = Ed25519.toPublic sk'
        sig = Ed25519.sign sk' pk xs
     in Signature (convert sig)
  CryptoFailed _ -> error "Invalid private key for signing"
{-# INLINE sign #-}

verify :: PublicKey -> BS.ByteString -> Signature -> Bool
verify (PublicKey pk) xs (Signature sig) =
  case Ed25519.publicKey pk of
    CryptoPassed pk' ->
      case Ed25519.signature sig of
        CryptoPassed sig' -> Ed25519.verify pk' xs sig'
        CryptoFailed _ -> False
    CryptoFailed _ -> False
{-# INLINE verify #-}

toPublicKey :: PrivateKey -> PublicKey
toPublicKey (PrivateKey sk) = case Ed25519.secretKey sk of
  CryptoPassed sk' -> PublicKey (convert (Ed25519.toPublic sk'))
  CryptoFailed _ -> error "Invalid private key for deriving public key"
{-# INLINE toPublicKey #-}

toRawBytes :: PublicKey -> BS.ByteString
toRawBytes (PublicKey pk) = pk
{-# INLINE toRawBytes #-}

fromRawBytes :: BS.ByteString -> Maybe PublicKey
fromRawBytes pk
  | BS.length pk == Ed25519.publicKeySize = Just (PublicKey pk)
  | otherwise = Nothing
{-# INLINE fromRawBytes #-}

emptySignature :: Signature
emptySignature = Signature (BS.replicate Ed25519.signatureSize 0)
{-# NOINLINE emptySignature #-}

emptyPublicKey :: PublicKey
emptyPublicKey = PublicKey (BS.replicate Ed25519.publicKeySize 0)
{-# NOINLINE emptyPublicKey #-}

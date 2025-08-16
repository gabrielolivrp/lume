{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Core.Crypto.Signature (
  -- * Types
  Signature (..),
  PrivateKey (..),
  PublicKey (..),
  KeyPair (..),
  Key (..),

  -- * Functions
  generateKeyPair,
  sign,
  verify,
  toPublicKey,
  emptySignature,
) where

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson hiding (Key)
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as Char8
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

class Key k where
  toRawBytes :: k -> BS.ByteString
  fromRawBytes :: BS.ByteString -> Maybe k
  toBase18 :: k -> String
  emptyKey :: k

newtype Signature = Signature BS.ByteString
  deriving stock (Show, Eq, Generic)

instance Binary Signature where
  get = Signature <$> getByteString Ed25519.signatureSize
  put (Signature sig)
    | BS.length sig == Ed25519.signatureSize = putByteString sig
    | otherwise = error "Invalid signature size"

instance ToJSON Signature where
  toJSON (Signature sig) = toJSON $ toBase18' sig

instance FromJSON Signature where
  parseJSON = withText "Signature" $ \txt ->
    let bs = TE.encodeUtf8 txt
     in case B16.decode bs of
          Right decoded
            | BS.length decoded == Ed25519.signatureSize ->
                pure (Signature decoded)
          _ -> fail $ "Invalid signature format: " ++ show txt

newtype PrivateKey = PrivateKey BS.ByteString
  deriving stock (Eq, Generic)

instance Show PrivateKey where
  show (PrivateKey sk) = toBase18' sk

instance Binary PrivateKey where
  get = PrivateKey <$> getByteString Ed25519.secretKeySize
  put (PrivateKey sk)
    | BS.length sk == Ed25519.secretKeySize = putByteString sk
    | otherwise = error "Invalid private key size"

instance Key PrivateKey where
  toRawBytes (PrivateKey sk) = sk
  fromRawBytes sk
    | BS.length sk == Ed25519.secretKeySize = Just (PrivateKey sk)
    | otherwise = Nothing
  toBase18 (PrivateKey sk) = toBase18' sk
  emptyKey = PrivateKey (BS.replicate Ed25519.secretKeySize 0)

newtype PublicKey = PublicKey BS.ByteString
  deriving stock (Eq, Ord, Generic)

instance Show PublicKey where
  show (PublicKey pk) = toBase18' pk

instance Binary PublicKey where
  get = PublicKey <$> getByteString Ed25519.publicKeySize
  put (PublicKey pk)
    | BS.length pk == Ed25519.publicKeySize = putByteString pk
    | otherwise = error "Invalid public key size"

instance Key PublicKey where
  toRawBytes (PublicKey pk) = pk
  fromRawBytes pk
    | BS.length pk == Ed25519.publicKeySize = Just (PublicKey pk)
    | otherwise = Nothing
  toBase18 (PublicKey pk) = toBase18' pk
  emptyKey = PublicKey (BS.replicate Ed25519.publicKeySize 0)

instance ToJSON PublicKey where
  toJSON (PublicKey pk) = toJSON $ toBase18' pk

instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" $ \txt ->
    let bs = TE.encodeUtf8 txt
     in case B16.decode bs of
          Right decoded
            | BS.length decoded == Ed25519.publicKeySize ->
                pure (PublicKey decoded)
          _ -> fail $ "Invalid public key format: " ++ show txt

newtype KeyPair = KeyPair (PublicKey, PrivateKey)
  deriving stock (Show, Eq, Generic)

instance Binary KeyPair where
  put (KeyPair (PublicKey pk, PrivateKey sk)) = do
    putByteString pk
    putByteString sk

  get = do
    pk <- get
    sk <- get
    pure $ KeyPair (pk, sk)

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

toBase18' :: BS.ByteString -> String
toBase18' = Char8.unpack . B16.encode
{-# INLINE toBase18' #-}

emptySignature :: Signature
emptySignature = Signature (BS.replicate Ed25519.signatureSize 0)
{-# NOINLINE emptySignature #-}

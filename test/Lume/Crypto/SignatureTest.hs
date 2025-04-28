{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Crypto.SignatureTest where

import Data.Binary (decode, encode)
import Data.ByteString.Char8 qualified as BS
import Lume.Crypto.Signature qualified as Sig
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

signatureTests :: TestTree
signatureTests =
  testGroup
    "Signature Tests"
    [ testCase "Sign and verify a message successfully" $ do
        -- Generate a new key pair and sign a message using the private key.
        (Sig.KeyPair (pubKey, privKey)) <- Sig.generateKeyPair
        let message = BS.pack "The quick brown fox jumps over the lazy dog"
            sig = Sig.sign privKey message
        -- Verify that the signature is valid for the given message and public key.
        assertBool "Valid signature should verify" (Sig.verify pubKey message sig)
    , testCase "Verification fails for altered message" $ do
        -- Ensure that if the message is modified, the signature verification fails.
        (Sig.KeyPair (pubKey, privKey)) <- Sig.generateKeyPair
        let originalMessage = BS.pack "foo!"
            alteredMessage = BS.pack "foo?"
            sig = Sig.sign privKey originalMessage
        assertBool "Signature should fail for altered message" (not $ Sig.verify pubKey alteredMessage sig)
    , testCase "Verification fails with a different public key" $ do
        -- Sign a message with one key pair and attempt verification with a different public key.
        (Sig.KeyPair (_, privKey1)) <- Sig.generateKeyPair
        (Sig.KeyPair (pubKey2, _)) <- Sig.generateKeyPair
        let message = BS.pack "Test message"
            sig = Sig.sign privKey1 message
        assertBool "Signature should fail with a different public key" (not $ Sig.verify pubKey2 message sig)
    , testCase "Private to public key conversion returns correct public key" $ do
        -- Verify that converting a private key to its corresponding public key is correct.
        (Sig.KeyPair (pubKey, privKey)) <- Sig.generateKeyPair
        assertBool "Private key to public key conversion should match" $
          pubKey == Sig.toPublicKey privKey
    , testCase "Binary serialization round-trip for KeyPair" $ do
        -- Test that a key pair can be serialized and deserialized without loss.
        originalKeyPair <- Sig.generateKeyPair
        let serialized = encode originalKeyPair
            deserialized = decode serialized :: Sig.KeyPair
        assertBool "KeyPair serialization should be reversible" $
          originalKeyPair == deserialized
    ]

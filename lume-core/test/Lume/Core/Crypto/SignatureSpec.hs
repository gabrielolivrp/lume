{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Crypto.SignatureSpec where

import Data.Binary (decode, encode)
import Data.ByteString.Char8 qualified as BS
import Lume.Core.Crypto.Signature qualified as Sig
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

signatureSpec :: TestTree
signatureSpec =
  testGroup
    "Crypto.Signature"
    [ testCase "should sign and verify message successfully" $ do
        (Sig.KeyPair (pubKey, privKey)) <- Sig.generateKeyPair
        let message = BS.pack "The quick brown fox jumps over the lazy dog"
            signature = Sig.sign privKey message
        assertBool "valid signature should verify" $
          Sig.verify pubKey message signature
    , testCase "should fail verification for altered message" $ do
        (Sig.KeyPair (pubKey, privKey)) <- Sig.generateKeyPair
        let originalMessage = BS.pack "foo!"
            alteredMessage = BS.pack "foo?"
            signature = Sig.sign privKey originalMessage
        assertBool "signature should fail for altered message" $
          not $
            Sig.verify pubKey alteredMessage signature
    , testCase "should fail verification with different public key" $ do
        (Sig.KeyPair (_, privKey1)) <- Sig.generateKeyPair
        (Sig.KeyPair (pubKey2, _)) <- Sig.generateKeyPair
        let message = BS.pack "Test message"
            signature = Sig.sign privKey1 message
        assertBool "signature should fail with different public key" $
          not $
            Sig.verify pubKey2 message signature
    , testCase "should convert private key to correct public key" $ do
        (Sig.KeyPair (expectedPubKey, privKey)) <- Sig.generateKeyPair
        let derivedPubKey = Sig.toPublicKey privKey
        assertBool "derived public key should match original" $
          expectedPubKey == derivedPubKey
    , testCase "should preserve KeyPair through binary serialization" $ do
        originalKeyPair <- Sig.generateKeyPair
        let serialized = encode originalKeyPair
            deserializedKeyPair = decode serialized :: Sig.KeyPair
        assertBool "deserialized KeyPair should match original" $
          originalKeyPair == deserializedKeyPair
    ]

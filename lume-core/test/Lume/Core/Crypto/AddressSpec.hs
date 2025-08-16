{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Crypto.AddressSpec where

import Data.Text qualified as T
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Crypto.Signature qualified as Sig
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

addressSpec :: TestTree
addressSpec =
  testGroup
    "Crypto.Address"
    [ testCase "should preserve public key through round-trip conversion" $ do
        (Sig.KeyPair (pubKey, _)) <- Sig.generateKeyPair
        let addrResult = Addr.fromPublicKey pubKey
        case addrResult of
          Left err ->
            assertFailure $ "fromPublicKey failed unexpectedly: " <> show err
          Right addr -> do
            let pubKeyResult = Addr.toPublicKey addr
            case pubKeyResult of
              Left err ->
                assertFailure $ "toPublicKey failed unexpectedly: " <> show err
              Right reconstructedPubKey ->
                assertEqual "round-trip public key" pubKey reconstructedPubKey
    , testCase "should generate address with correct prefix" $ do
        (Sig.KeyPair (pubKey, _)) <- Sig.generateKeyPair
        case Addr.fromPublicKey pubKey of
          Left err ->
            assertFailure $ "fromPublicKey failed unexpectedly: " <> show err
          Right (Addr.Address addrText) ->
            assertBool "address should have correct prefix" $
              T.isPrefixOf Addr.prefix addrText
    , testCase "should fail to decode invalid address" $ do
        let invalidAddr = Addr.Address "invalid_address"
            result = Addr.toPublicKey invalidAddr
        case result of
          Right pk ->
            assertFailure $ "toPublicKey should have failed for invalid address, got: " <> show pk
          Left err -> case err of
            Addr.DecodingFailed ->
              pure ()
            _ ->
              assertFailure $ "expected DecodingFailed error, got: " <> show err
    , testCase "should generate different addresses for different public keys" $ do
        (Sig.KeyPair (pubKey1, _)) <- Sig.generateKeyPair
        (Sig.KeyPair (pubKey2, _)) <- Sig.generateKeyPair
        case (Addr.fromPublicKey pubKey1, Addr.fromPublicKey pubKey2) of
          (Right addr1, Right addr2) ->
            assertBool "addresses should be different for different public keys" $
              addr1 /= addr2
          (Left err, _) ->
            assertFailure $ "fromPublicKey failed for first key: " <> show err
          (_, Left err) ->
            assertFailure $ "fromPublicKey failed for second key: " <> show err
    ]

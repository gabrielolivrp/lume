{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Crypto.AddressTest where

import Data.Text qualified as T
import Lume.Crypto.Address qualified as Addr
import Lume.Crypto.Signature qualified as Sig
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

addressTests :: TestTree
addressTests =
  testGroup
    "Address Tests"
    [ testCase "Round-trip conversion fromPublicKey -> toPublicKey returns the original public key" $ do
        (Sig.KeyPair (pubKey, _)) <- Sig.generateKeyPair
        let addrResult = Addr.fromPublicKey pubKey
        case addrResult of
          Left err ->
            assertFailure $ "fromPublicKey failed with error: " ++ show err
          Right addr -> do
            -- Convert the address back to a public key
            let pubKeyResult = Addr.toPublicKey addr
            case pubKeyResult of
              Left err ->
                assertFailure $ "toPublicKey failed with error: " ++ show err
              Right pubKey' ->
                assertEqual "Public key should match after round-trip conversion" pubKey pubKey'
    , testCase "Address string should start with the correct prefix" $ do
        (Sig.KeyPair (pubKey, _)) <- Sig.generateKeyPair
        case Addr.fromPublicKey pubKey of
          Left err ->
            assertFailure $ "fromPublicKey failed with error: " ++ show err
          Right (Addr.Address addrText) ->
            assertBool
              ("Address should start with prefix " ++ T.unpack Addr.prefix)
              (T.isPrefixOf Addr.prefix addrText)
    , testCase "toPublicKey fails with an invalid address" $ do
        -- Construct an Address with an invalid string.
        let invalidAddr = Addr.Address "invalid_address"
            result = Addr.toPublicKey invalidAddr
        case result of
          Right pk ->
            assertFailure $ "Expected failure when decoding an invalid address, but got: " ++ show pk
          Left err -> case err of
            Addr.DecodingFailed -> pure ()
            _ -> assertFailure $ "Expected an AddressDecodeError but got: " ++ show err
    , testCase "Different public keys produce different addresses" $ do
        (Sig.KeyPair (pubKey1, _)) <- Sig.generateKeyPair
        (Sig.KeyPair (pubKey2, _)) <- Sig.generateKeyPair
        case (Addr.fromPublicKey pubKey1, Addr.fromPublicKey pubKey2) of
          (Right addr1, Right addr2) ->
            assertBool "Different public keys should produce different addresses" (addr1 /= addr2)
          (Left err, _) ->
            assertFailure $ "First fromPublicKey failed with error: " ++ show err
          (_, Left err) ->
            assertFailure $ "Second fromPublicKey failed with error: " ++ show err
    ]

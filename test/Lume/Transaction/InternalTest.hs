{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Transaction.InternalTest where

import Control.Lens
import Data.List.NonEmpty qualified as NE
import Lume.Crypto.Signature (KeyPair (KeyPair), emptyPublicKey, emptySignature, generateKeyPair)
import Lume.Mocks
import Lume.Transaction
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

transactionInternalTests :: TestTree
transactionInternalTests =
  testGroup
    "Transaction.Internal"
    [ testCase "should build transaction with single input and output" $ do
        let outputs = NE.singleton (mockAddress1, 1000)
            outpoints = NE.singleton (Outpoint mockHash1 0)
            result = buildTx outpoints outputs
        case result of
          Left err ->
            assertFailure $ "buildTx failed unexpectedly: " <> show err
          Right tx -> do
            assertEqual "transaction input count" 1 (length (tx ^. txIn))
            assertEqual "transaction output count" 1 (length (tx ^. txOut))
            assertEqual "transaction version" transactionVersion (tx ^. txVersion)
    , testCase "should fail when amount exceeds maximum" $ do
        let outputs = NE.singleton (mockAddress1, maxCoin + 1)
            outpoints = NE.singleton (Outpoint mockHash1 0)
            result = buildTx outpoints outputs
        case result of
          Left TransactionBuilderValueOverflowError -> pure ()
          _ -> assertFailure "buildTx should have failed for invalid amount"
    , testCase "should sign transaction with provided signature inputs" $ do
        KeyPair (_, privKey) <- generateKeyPair
        let outputs = NE.singleton (mockAddress1, 1000)
            outpoints = NE.singleton (Outpoint mockHash1 0)
            sigInputs = NE.singleton (SigInput (Outpoint mockHash1 0) privKey)
        case buildTx outpoints outputs of
          Left err ->
            assertFailure $ "buildTx failed unexpectedly: " <> show err
          Right unsignedTx ->
            case signTx unsignedTx sigInputs of
              Left _ -> assertFailure "signTx failed unexpectedly"
              Right signedTx -> do
                let firstInput = head (signedTx ^. txIn)
                assertBool "signed transaction should have non-empty signature" $
                  firstInput ^. txInSignature /= emptySignature
                assertBool "signed transaction should have non-empty public key" $
                  firstInput ^. txInPubKey /= emptyPublicKey
    ]

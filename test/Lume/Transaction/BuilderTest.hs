{-# LANGUAGE OverloadedStrings #-}

module Lume.Transaction.BuilderTest where

import Control.Lens
import Lume.Crypto.Signature (KeyPair (KeyPair), emptyPublicKey, emptySignature, generateKeyPair)
import Lume.Mocks
import Lume.Transaction.Amount (maxAmount)
import Lume.Transaction.Builder
import Lume.Transaction.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

transactionBuilderTests :: TestTree
transactionBuilderTests =
  testGroup
    "Transaction.Builder"
    [ testCase "should build transaction with single input and output" $ do
        let outputs = [(mockAddress1, 1000)]
            outpoints = [Outpoint mockHash1 0]
            result = buildTx outpoints outputs
        case result of
          Left err ->
            assertFailure $ "buildTx failed unexpectedly: " <> show err
          Right tx -> do
            assertEqual "transaction input count" 1 (length (tx ^. txIn))
            assertEqual "transaction output count" 1 (length (tx ^. txOut))
            assertEqual "transaction version" tVersion (tx ^. txVersion)
    , testCase "should fail when amount exceeds maximum" $ do
        let outputs = [(mockAddress1, maxAmount + 1)]
            outpoints = [Outpoint mockHash1 0]
            result = buildTx outpoints outputs
        case result of
          Left ValueExceedsMaxAmount -> pure ()
          _ -> assertFailure "buildTx should have failed for invalid amount"
    , testCase "should sign transaction with provided signature inputs" $ do
        KeyPair (_, privKey) <- generateKeyPair
        let outpoints = [Outpoint mockHash1 0]
            outputs = [(mockAddress1, 1000)]
            sigInputs = [SigInput (Outpoint mockHash1 0) privKey]
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
    , testCase "should fail when signature input is missing" $ do
        case buildTx [Outpoint mockHash1 0] [(mockAddress1, 1000)] of
          Left err ->
            assertFailure $ "buildTx failed unexpectedly: " <> show err
          Right unsignedTx -> do
            let emptySigInputs = []
                result = signTx unsignedTx emptySigInputs
            case result of
              Left _ -> pure ()
              _ -> assertFailure "signTx should have failed for missing signature input"
    ]

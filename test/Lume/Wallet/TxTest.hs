{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.TxTest where

import Control.Lens
import Data.Either (isRight)
import Lume.Mocks
import Lume.Transaction.Types
import Lume.Wallet.Tx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

walletTxTests :: TestTree
walletTxTests =
  testGroup
    "Wallet.Tx"
    [ testCase "should build unsigned transaction with exact funds" $ do
        let params = BuildUnsignedTxParams mockAddress1 mockAddress2 900 100
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        assertBool "transaction should build successfully with exact funds" $ isRight result
    , testCase "should create change output when funds exceed requirements" $ do
        let params = BuildUnsignedTxParams mockAddress1 mockAddress2 800 100
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        case result of
          Right tx -> do
            let outputs = tx ^. txOut
            length outputs @?= 2
            let recipientAmount = _txOutValue (head outputs)
                changeAmount = _txOutValue (outputs !! 1)
            recipientAmount @?= 800
            changeAmount @?= 100
          Left err -> assertFailure $ "buildUnsignedTx failed unexpectedly: " <> show err
    , testCase "should fail with insufficient funds" $ do
        let params = BuildUnsignedTxParams mockAddress1 mockAddress2 950 100
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        result @?= Left InsufficientFunds
    , testCase "should fail when no UTXOs available for address" $ do
        let params = BuildUnsignedTxParams mockAddress3 mockAddress2 500 100
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        result @?= Left NoUnspentOutputsAvailable
    , testCase "should fail when transaction value is zero" $ do
        let params = BuildUnsignedTxParams mockAddress1 mockAddress2 0 100
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        case result of
          Right _ -> assertFailure "buildUnsignedTx should have failed for zero value"
          Left (InvalidTransactionValue _) -> pure ()
          Left err -> assertFailure $ "expected InvalidTransactionValue error, got: " <> show err
    , testCase "should fail when transaction fee is zero" $ do
        let params = BuildUnsignedTxParams mockAddress1 mockAddress2 500 0
            utxoSet = mockUtxoSet [utxo1]
            result = buildUnsignedTx utxoSet params
        case result of
          Right _ -> assertFailure "buildUnsignedTx should have failed for zero fee"
          Left (InvalidTransactionFee _) -> pure ()
          Left err -> assertFailure $ "expected InvalidTransactionFee error, got: " <> show err
    ]

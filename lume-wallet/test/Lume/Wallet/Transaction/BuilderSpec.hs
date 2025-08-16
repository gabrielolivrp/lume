{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Transaction.BuilderSpec where

import Control.Lens
import Data.Either (isRight)
import Lume.Core.Crypto.Address (Address (..))
import Lume.Core.Crypto.Hash (hash')
import Lume.Core.Transaction
import Lume.Wallet.Transaction
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

address1 :: Address
address1 = Address "lume_addr_1"

address2 :: Address
address2 = Address "lume_addr_2"

address3 :: Address
address3 = Address "lume_addr_3"

utxo1 :: UTXO
utxo1 = UTXO (hash' "tx1") 0 address1 1000

transactionBuilderSpec :: TestTree
transactionBuilderSpec =
  testGroup
    "Transaction.Builder"
    [ testCase "should build unsigned transaction with exact funds" $ do
        let params = BuildUnsignedTxParams address1 address2 900 100
            result = buildUnsignedTx [utxo1] params
        assertBool "transaction should build successfully with exact funds" $ isRight result
    , testCase "should create change output when funds exceed requirements" $ do
        let params = BuildUnsignedTxParams address1 address2 800 100
            result = buildUnsignedTx [utxo1] params
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
        let params = BuildUnsignedTxParams address1 address2 950 100
            result = buildUnsignedTx [utxo1] params
        result @?= Left WalletInsufficientFundsError
    , testCase "should fail when no UTXOs available for address" $ do
        let params = BuildUnsignedTxParams address3 address2 500 100
            result = buildUnsignedTx [] params
        result @?= Left WalletNoUnspentOutputsError
    , testCase "should fail when transaction value is zero" $ do
        let params = BuildUnsignedTxParams address1 address2 0 100
            result = buildUnsignedTx [utxo1] params
        case result of
          Right _ -> assertFailure "buildUnsignedTx should have failed for zero value"
          Left (WalletInvalidTransactionValueError _) -> pure ()
          Left err -> assertFailure $ "expected InvalidTransactionValue error, got: " <> show err
    , testCase "should fail when transaction fee is zero" $ do
        let params = BuildUnsignedTxParams address1 address2 500 0
            result = buildUnsignedTx [utxo1] params
        case result of
          Right _ -> assertFailure "buildUnsignedTx should have failed for zero fee"
          Left (WalletInvalidTransactionFeeError _) -> pure ()
          Left err -> assertFailure $ "expected InvalidTransactionFee error, got: " <> show err
    ]

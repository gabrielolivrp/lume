{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Transaction.SignerTest where

import Control.Lens
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Core.Transaction
import Lume.Mocks
import Lume.Wallet.Transaction.Signer
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

walletSignerTests :: TestTree
walletSignerTests =
  testGroup
    "Signer"
    [ testCase "should sign transaction with provided signature inputs" $ do
        Sig.KeyPair (_, privKey) <- Sig.generateKeyPair
        let outputs = NE.singleton (mockAddress1, 1000)
            outpoints = NE.singleton (Outpoint mockHash1 0)
            sigTxIns = NE.singleton (SigTxIn (Outpoint mockHash1 0) privKey)
            unsignedTx = buildTx outpoints outputs
        case signTx unsignedTx sigTxIns of
          Left _ -> assertFailure "signTx failed unexpectedly"
          Right signedTx -> do
            let firstInput = head (signedTx ^. txIn)
            assertBool "signed transaction should have non-empty signature" $
              firstInput ^. txInSignature /= Sig.emptySignature
            assertBool "signed transaction should have non-empty public key" $
              firstInput ^. txInPubKey /= Sig.emptyKey
    , testCase "should fail to sign transaction with invalid signature inputs" $ do
        Sig.KeyPair (_, privKey) <- Sig.generateKeyPair
        let outputs = [TxOut mockAddress1 1000]
            coinbase = mockCoinbaseTx 1 outputs
        let invalidSigTxIns = NE.singleton (SigTxIn (Outpoint mockHash2 0) privKey)
        assertBool "signed transaction should fail with invalid inputs" $
          isLeft (signTx coinbase invalidSigTxIns)
    ]

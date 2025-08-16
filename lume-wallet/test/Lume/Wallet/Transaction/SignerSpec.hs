{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Transaction.SignerSpec where

import Control.Lens
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Lume.Core
import Lume.Core.Crypto.Address (Address (..))
import Lume.Core.Crypto.Hash (hash')
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Wallet.Transaction.Signer
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

address1 :: Address
address1 = Address "lume_addr_1"

signerSpec :: TestTree
signerSpec =
  testGroup
    "Signer"
    [ testCase "should sign transaction with provided signature inputs" $ do
        Sig.KeyPair (_, privKey) <- Sig.generateKeyPair
        let outputs = NE.singleton (address1, 1000)
            outpoints = NE.singleton (Outpoint (hash' "1") 0)
            sigTxIns = NE.singleton (SigTxIn (Outpoint (hash' "1") 0) privKey)
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
        let outputs = [TxOut address1 1000]
            coinbase =
              Tx
                { _txVersion = 1
                , _txIn = []
                , _txOut = outputs
                }
        let invalidSigTxIns = NE.singleton (SigTxIn (Outpoint (hash' "1") 0) privKey)
        assertBool "signed transaction should fail with invalid inputs" $
          isLeft (signTx coinbase invalidSigTxIns)
    ]

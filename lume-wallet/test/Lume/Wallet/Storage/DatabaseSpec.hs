{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Storage.DatabaseSpec where

import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Wallet.Storage.Database
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

storageDatabaseSpec :: TestTree
storageDatabaseSpec =
  testGroup
    "Storage.Database"
    [ testCase "should store and load WalletModel" $
        withSystemTempDirectory "wallet-db-test" $ \dir -> do
          let path = dir </> "wallet.sqlite"
              wallet =
                WalletModel
                  { wmName = "Test Wallet"
                  , wmPublicKey = "pubkey"
                  , wmPrivateKey = "privkey"
                  , wmAddress = "wallet-address"
                  }
          result <- runExceptT $ do
            db <- openDatabase path
            storeWallet db wallet
            mLoaded <- loadWallet db
            liftIO $ case mLoaded of
              Nothing -> assertFailure "Expected wallet to be loaded"
              Just loaded -> assertEqual "Loaded wallet" wallet loaded
          case result of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right _ -> pure ()
    , testCase "should store and retrieve UTXOModel" $
        withSystemTempDirectory "wallet-db-test" $ \dir -> do
          let path = dir </> "wallet.sqlite"
              txid = Hash.hash' "1"
              utxo =
                UTXOModel
                  { umTxId = Hash.toHex txid
                  , umOutIdx = 0
                  , umAmount = 10000
                  , umAddress = "wallet-address"
                  , umSpent = False
                  }
          result <- runExceptT $ do
            db <- openDatabase path
            storeUTXO db utxo
            utxos <- getUTXOs db
            liftIO $ assertEqual "UTXOs after insert" [utxo] utxos
          case result of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right _ -> pure ()
    , testCase "should mark UTXO as spent" $
        withSystemTempDirectory "wallet-db-test" $ \dir -> do
          let path = dir </> "wallet.sqlite"
              txid = Hash.hash' "1"
              utxo =
                UTXOModel
                  { umTxId = Hash.toHex txid
                  , umOutIdx = 1
                  , umAmount = 20000
                  , umAddress = "wallet-address"
                  , umSpent = False
                  }
          result <- runExceptT $ do
            db <- openDatabase path
            storeUTXO db utxo
            spendUTXO db txid 1
            utxos <- getUTXOs db
            liftIO $ assertBool "No unspent UTXOs should remain" (null utxos)
          case result of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right _ -> pure ()
    ]

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.InternalSpec where

import Control.Monad.Except (MonadIO (liftIO))
import qualified Lume.Core.Crypto.Address as Addr
import qualified Lume.Core.Crypto.Hash as Hash
import qualified Lume.Core.Transaction as Tx
import Lume.Wallet.Config (Config (..), WalletRpcConfig (..))
import Lume.Wallet.Internal
import Lume.Wallet.Types
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

defaultConfig :: FilePath -> Config
defaultConfig dataDir =
  Config
    { cDataDir = dataDir
    , cRpc =
        WalletRpcConfig
          { rpcHost = "localhost"
          , rpcPort = 8080
          }
    }

internalSpec :: TestTree
internalSpec =
  testGroup
    "Internal"
    [ testCase "should create a new wallet with a valid address and correct name" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          let config = defaultConfig dir
          let walletName = mkWalletName "testWallet"
          wallet <-
            newWallet config walletName >>= \case
              Left err -> assertFailure $ "Failed to create wallet: " ++ show err
              Right wallet -> pure wallet
          assertEqual "Wallet name should match" walletName (wName wallet)
          assertBool "Wallet address should be valid" (Addr.isValidAddress (wAddr wallet))
    , testCase "should store and load a wallet correctly using WalletStorage" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          let config = defaultConfig dir
          let walletName = mkWalletName "testWallet"
          wallet <-
            newWallet config walletName >>= \case
              Left err -> assertFailure $ "Failed to create wallet: " ++ show err
              Right wallet -> pure wallet
          result <-
            withWallet config walletName $ do
              storeWallet wallet
              loaded <- loadWallet
              case loaded of
                Nothing -> fail "Failed to load wallet"
                Just w2 -> liftIO $ assertEqual "Loaded wallet should match the stored one" wallet w2
          case result of
            Left err -> assertFailure $ "Error during wallet operations: " ++ show err
            Right _ -> pure ()
    , testCase "should store, retrieve and mark UTXO as spent using WalletStorage" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          let config = defaultConfig dir
          let walletName = mkWalletName "testWallet"
          wallet <-
            newWallet config walletName
              >>= \case
                Left err -> assertFailure $ "Failed to create wallet: " ++ show err
                Right wallet -> pure wallet
          result <-
            withWallet config walletName $ do
              storeWallet wallet
              let txId = Hash.hash' "abcd1234"
                  utxo = Tx.UTXO txId 0 (wAddr wallet) 1000
              storeUTXO utxo
              utxos <- getUTXOs
              liftIO $ assertEqual "Should have one UTXO stored" 1 (length utxos)
              spendUTXO utxo
              utxos2 <- getUTXOs
              liftIO $ assertEqual "UTXO should still exist after marking as spent" 0 (length utxos2)
          case result of
            Left err -> assertFailure $ "Error during wallet operations: " ++ show err
            Right _ -> pure ()
            {-- , testCase "should send transaction with valid inputs and outputs" $
                withSystemTempDirectory "wallet-internal-test" $ \dir -> do
                  let config = defaultConfig dir
                      walletName1 = mkWalletName "w1"
                      walletName2 = mkWalletName "w2"
                  wallet1 <-
                    newWallet config walletName1
                      >>= \case
                        Left err -> assertFailure $ "Failed to create wallet: " ++ show err
                        Right wallet -> pure wallet
                  wallet2 <-
                    newWallet config walletName2
                      >>= \case
                        Left err -> assertFailure $ "Failed to create wallet: " ++ show err
                        Right wallet -> pure wallet

                  resultWallet1 <-
                    withWallet config walletName1 $ do
                      storeWallet wallet1

                  case resultWallet1 of
                    Left err -> assertFailure $ "Error during wallet operations: " ++ show err
                    Right _ -> pure ()

                  resultWallet2 <-
                    withWallet config walletName2 $ do
                      storeWallet wallet2
                      let utxo1 = UTXO (Hash.hash' "tx1") 0 (wAddr wallet1) 100000
                          utxo2 = UTXO (Hash.hash' "tx2") 0 (wAddr wallet1) 100000
                          utxo3 = UTXO (Hash.hash' "tx3") 0 (wAddr wallet1) 100000
                          utxo4 = UTXO (Hash.hash' "tx4") 0 (wAddr wallet1) 100000
                      storeUTXO utxo1
                      storeUTXO utxo2
                      storeUTXO utxo3
                      storeUTXO utxo4
                      sendTransaction (wAddr wallet2) 500
                  case resultWallet2 of
                    Left err -> assertFailure $ "Error during wallet operations: " ++ show err
                    Right _ -> pure () --}
    ]

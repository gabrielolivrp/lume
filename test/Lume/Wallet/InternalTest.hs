{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.InternalTest where

import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import qualified Data.Text as T
import qualified Lume.Core.Crypto.Address as Addr
import qualified Lume.Core.Crypto.Hash as Hash
import qualified Lume.Core.Transaction as Tx
import Lume.Mocks (mockUTXO)
import Lume.Wallet.Internal
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

walletInternalTests :: TestTree
walletInternalTests =
  testGroup
    "Internal"
    [ testCase "should create a new wallet with a valid address and correct name" $ do
        let testWalletName = mkWalletName "testWallet"
        result <- runExceptT $ newWallet testWalletName
        case result of
          Left err -> assertFailure $ "Failed to create wallet: " ++ show err
          Right wallet -> do
            let Wallet name _ _ (Addr.Address addr) = wallet
            assertEqual "Wallet name should match" testWalletName name
            assertBool "Address should not be empty" (not $ T.null addr)
    , testCase "should store and load a wallet correctly using WalletStorage" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          let testWalletName = mkWalletName "testWallet"
          _ <-
            runExceptT $
              newWallet testWalletName >>= \wallet -> do
                walletStorage <- mkWalletStorage dir testWalletName
                store walletStorage wallet
                loaded <- load walletStorage
                case loaded of
                  Nothing -> fail "Failed to load wallet"
                  Just w2 -> liftIO $ assertEqual "Loaded wallet should match the stored one" wallet w2
          pure ()
    , testCase "should store, retrieve and mark UTXO as spent using WalletStorage" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          let testWalletName = mkWalletName "testWallet"
          _ <- runExceptT $ do
            wallet <- newWallet testWalletName
            walletStorage <- mkWalletStorage dir testWalletName
            store walletStorage wallet

            -- Store UTXO
            let txId = Hash.hash' "abcd1234" -- mock tx id
                utxo = Tx.UTXO txId 0 (wAddr wallet) 1000
            storeUTXO walletStorage utxo

            -- Verify it's stored
            utxos <- getUTXOs walletStorage
            liftIO $ assertEqual "Should have one UTXO stored" 1 (length utxos)

            -- Mark as spent
            markUTXOAsSpent walletStorage txId 0

            -- Get again — still exists unless you implement filtering of spent UTXOs
            utxos2 <- getUTXOs walletStorage
            liftIO $ assertEqual "UTXO should still exist after marking as spent" 0 (length utxos2)
          pure ()
    , testCase "should send transaction with valid inputs and outputs" $
        withSystemTempDirectory "wallet-internal-test" $ \dir -> do
          result <- runExceptT $ do
            let walletName1 = mkWalletName "w1"
                walletName2 = mkWalletName "w2"

            -- Create wallets
            wallet1 <- newWallet walletName1
            walletStorage1 <- mkWalletStorage dir walletName1
            store walletStorage1 wallet1

            wallet2 <- newWallet walletName2
            walletStorage2 <- mkWalletStorage dir walletName2
            store walletStorage2 wallet1

            -- Store UTXOs
            let utxo1 = mockUTXO (Hash.hash' "tx1") 0 (wAddr wallet1) 100000
                utxo2 = mockUTXO (Hash.hash' "tx2") 0 (wAddr wallet1) 100000
                utxo3 = mockUTXO (Hash.hash' "tx3") 0 (wAddr wallet1) 100000
                utxo4 = mockUTXO (Hash.hash' "tx4") 0 (wAddr wallet1) 100000

            storeUTXO walletStorage1 utxo1
            storeUTXO walletStorage1 utxo2
            storeUTXO walletStorage1 utxo3
            storeUTXO walletStorage1 utxo4

            sendTransaction wallet1 walletStorage1 (wAddr wallet2) 500

          assertEqual "Transaction should be sent successfully" (Right ()) result
    ]

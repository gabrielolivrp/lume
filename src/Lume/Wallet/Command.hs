{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Lume.Wallet.Command where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (MonadError (throwError))
import Data.Text qualified as T
import GHC.Natural (Natural)
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Transaction.Coin
import Lume.Wallet.Config (Config (cDataDir), parseConfig)
import Lume.Wallet.Internal

separator :: String
separator = "====================================================="

printSection :: String -> IO ()
printSection title = do
  putStrLn $ "\n" ++ title
  putStrLn separator

newWalletCommand :: FilePath -> WalletName -> IO ()
newWalletCommand configPath walletName = do
  config <- parseConfig configPath
  result <- runWalletM $ do
    exists <- checkWalletExists (cDataDir config) walletName
    when exists (throwError $ WalletAlreadyExistsError walletName)
    walletStorage <- mkWalletStorage (cDataDir config) walletName
    w <- newWallet walletName
    store walletStorage w
    pure w
  case result of
    Left err -> putStrLn $ "❌ Could not create wallet: " ++ show err
    Right wallet -> do
      putStrLn "\n✅ Wallet created successfully!"
      putStrLn separator
      showWalletInfo wallet
      putStrLn separator

getWalletInfoCommand :: FilePath -> WalletName -> IO ()
getWalletInfoCommand configPath walletName = do
  config <- parseConfig configPath
  result <- runWalletM $ do
    walletStorage <- mkWalletStorage (cDataDir config) walletName
    load walletStorage
  case result of
    Left err -> putStrLn $ "❌ Unable to load wallet: " ++ show err
    Right Nothing -> putStrLn "⚠️ Wallet not found."
    Right (Just wallet) -> do
      printSection "📄 Wallet Information"
      showWalletInfo wallet
      putStrLn separator

listAddressesCommand :: FilePath -> IO ()
listAddressesCommand configPath = do
  config <- parseConfig configPath
  result <- runWalletM $ loadAllWallets (cDataDir config)
  case result of
    Left err -> putStrLn $ "❌ Could not retrieve wallets: " ++ show err
    Right wallets
      | null wallets -> putStrLn "⚠️ No wallets found."
      | otherwise -> do
          printSection "🗂 Available Wallets"
          forM_ wallets $ \wallet -> do
            showWalletInfo wallet
            putStrLn separator

sendTransactionCommand :: FilePath -> WalletName -> String -> Natural -> IO ()
sendTransactionCommand configPath walletName to amount = do
  config <- parseConfig configPath
  result <- runWalletM $ do
    exists <- checkWalletExists (cDataDir config) walletName
    unless exists $ throwError (WalletNotFoundError walletName)

    walletStorage <- mkWalletStorage (cDataDir config) walletName
    wallet <-
      load walletStorage >>= \case
        Nothing -> throwError $ WalletNotFoundError walletName
        Just w -> pure w

    case Addr.mkAddress (T.pack to) of
      Left err -> throwError (WalletCryptoError $ show err)
      Right addr -> do
        let coin = Coin amount
        sendTransaction wallet walletStorage addr coin

  case result of
    Left err -> putStrLn $ "❌ Transaction failed: " ++ show err
    Right _ -> putStrLn "✅ Transaction completed successfully!"

showWalletInfo :: Wallet -> IO ()
showWalletInfo (Wallet walletName _ pubKey (Addr.Address addr)) = do
  putStrLn $ "🧾 Wallet Name : " ++ T.unpack (getWalletName walletName)
  putStrLn $ "🔐 Public Key  : " ++ show pubKey
  putStrLn $ "🏦 Address     : " ++ T.unpack addr

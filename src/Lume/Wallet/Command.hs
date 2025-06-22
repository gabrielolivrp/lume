{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Lume.Wallet.Command where

import Control.Monad.Except (MonadError (throwError), forM_, runExceptT, throwError, unless)
import Data.Text qualified as T
import GHC.Natural (Natural)
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Transaction.Coin
import Lume.Wallet.Internal

separator :: String
separator = "====================================================="

newWalletCommand :: WalletName -> IO ()
newWalletCommand walletName = do
  result <- runExceptT $ do
    walletStorage <- mkWalletStorage ".lume" walletName
    w <- newWallet ".lume" walletName
    store walletStorage w
    pure w
  case result of
    Left err -> putStrLn $ "❌ Failed to create wallet: " ++ show err
    Right wallet -> do
      putStrLn "✅ Wallet successfully created!\n"
      putStrLn separator
      showWalletInfo wallet
      putStrLn separator

getWalletInfoCommand :: WalletName -> IO ()
getWalletInfoCommand walletName = do
  result <- runExceptT $ do
    walletStorage <- mkWalletStorage ".lume" walletName
    load walletStorage
  case result of
    Left err -> putStrLn $ "❌ Failed to load wallet: " ++ show err
    Right Nothing -> putStrLn "❌ Wallet not found."
    Right (Just wallet) -> do
      putStrLn "📄 Wallet Information\n"
      putStrLn separator
      showWalletInfo wallet
      putStrLn separator

listAddressesCommand :: IO ()
listAddressesCommand = do
  result <- runExceptT $ loadAllWallets ".lume"
  case result of
    Left err -> putStrLn $ "❌ Failed to load wallets: " ++ show err
    Right wallets
      | null wallets -> putStrLn "❌ No wallets found."
      | otherwise -> do
          putStrLn "🗂 Available Wallets"
          putStrLn separator
          forM_ wallets $ \wallet -> do
            showWalletInfo wallet
            putStrLn separator

sendTransactionCommand :: WalletName -> String -> Natural -> IO ()
sendTransactionCommand walletName to amount = do
  result <- runExceptT $ do
    -- Check if the wallet exists
    exists <- checkWalletExists ".lume" walletName
    unless exists $ throwError (WalletNotFoundError walletName)

    -- Load the wallet
    walletStorage <- mkWalletStorage ".lume" walletName
    wallet <-
      load walletStorage >>= \case
        Nothing -> throwError $ WalletNotFoundError walletName
        Just w -> pure w

    case Addr.mkAddress (T.pack to) of
      Left err -> throwError (WalletCryptoError . T.pack $ show err)
      Right addr -> do
        let coin = Coin amount
        sendTransaction wallet walletStorage addr coin

  case result of
    Left err -> putStrLn $ "❌ Failed to send transaction: " ++ show err
    Right _ -> putStrLn "✅ Transaction successfully sent!"

showWalletInfo :: Wallet -> IO ()
showWalletInfo (Wallet walletName _ pubKey (Addr.Address addr)) = do
  putStrLn $ "🧾 Wallet Name : " ++ T.unpack (getWalletName walletName)
  putStrLn $ "🔐 Public Key  : " ++ show pubKey
  putStrLn $ "🏦 Address     : " ++ T.unpack addr

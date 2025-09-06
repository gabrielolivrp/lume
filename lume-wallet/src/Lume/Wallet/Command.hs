{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Wallet.Command where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError (throwError))
import Data.Text qualified as T
import GHC.Natural (Natural)
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Transaction.Coin
import Lume.Wallet.Config (Config (cDataDir), parseConfig)
import Lume.Wallet.FileSystem (checkWalletExists)
import Lume.Wallet.Internal
import Lume.Wallet.Types

separator :: String
separator = "====================================================="

printSection :: String -> IO ()
printSection title = do
  putStrLn $ "\n" ++ title
  putStrLn separator

newWalletCommand :: FilePath -> WalletName -> IO ()
newWalletCommand configPath walletName = do
  config <- parseConfig configPath
  result <- newWallet config walletName
  case result of
    Left err -> putStrLn $ "❌ Failed to generate wallet: " ++ show err
    Right wallet -> do
      storedWalletResult <- withWallet config walletName (storeWallet wallet >> pure wallet)
      case storedWalletResult of
        Left err -> putStrLn $ "❌ Could not store wallet: " ++ show err
        Right storedWallet -> do
          putStrLn "\n✅ Wallet created successfully!"
          putStrLn separator
          showWalletInfo storedWallet
          putStrLn separator

getWalletInfoCommand :: FilePath -> WalletName -> IO ()
getWalletInfoCommand configPath walletName = do
  config <- parseConfig configPath
  result <- withWallet config walletName loadWallet
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
  result <- loadAllWallets config
  case result of
    Left errors -> do
      putStrLn "❌ Errors occurred while loading wallets:"
      forM_ errors $ \err -> putStrLn $ "  - " ++ show err
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
  exists <- checkWalletExists (cDataDir config) walletName
  if not exists
    then putStrLn $ "❌ Wallet '" ++ getWalletName walletName ++ "' does not exist."
    else do
      result <- withWallet config walletName $ do
        case Addr.mkAddress (T.pack to) of
          Left err -> throwError (WalletCryptoError $ show err)
          Right addr -> do
            let coin = Coin amount
            sendTransaction addr coin
      case result of
        Left err -> putStrLn $ "❌ Transaction failed: " ++ show err
        Right txid -> putStrLn $ "✅ Transaction sent successfully! TXID: " ++ txid

scanFullUTXOCommand :: FilePath -> IO ()
scanFullUTXOCommand configPath = do
  config <- parseConfig configPath
  failures <- scanFullUTXO config
  if null failures
    then putStrLn "✅ Full UTXO scan completed successfully."
    else do
      putStrLn "❌ Full UTXO scan completed with errors:"
      forM_ failures $ \err -> putStrLn $ "  - " ++ show err

showWalletInfo :: Wallet -> IO ()
showWalletInfo (Wallet walletName _ pubKey (Addr.Address addr)) = do
  putStrLn $ "🧾 Wallet Name : " ++ getWalletName walletName
  putStrLn $ "🔐 Public Key  : " ++ show pubKey
  putStrLn $ "🏦 Address     : " ++ T.unpack addr

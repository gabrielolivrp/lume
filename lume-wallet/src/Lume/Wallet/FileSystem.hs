module Lume.Wallet.FileSystem where

import Data.List (isPrefixOf)
import Lume.Wallet.Types (WalletName (..), mkWalletName)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

mkBasePath :: FilePath -> FilePath
mkBasePath base = base </> "wallets"
{-# INLINE mkBasePath #-}

mkWalletPath :: FilePath -> WalletName -> FilePath
mkWalletPath base walletName = mkBasePath base </> "wallet_" <> getWalletName walletName
{-# INLINE mkWalletPath #-}

checkWalletExists :: FilePath -> WalletName -> IO Bool
checkWalletExists base walletName = doesDirectoryExist (mkWalletPath base walletName)
{-# INLINE checkWalletExists #-}

createWalletDirectory :: FilePath -> WalletName -> IO ()
createWalletDirectory base walletName = do
  let path = mkWalletPath base walletName
  createDirectoryIfMissing True path
{-# INLINE createWalletDirectory #-}

loadAllWalletsNames :: FilePath -> IO [WalletName]
loadAllWalletsNames base = do
  createDirectoryIfMissing True (mkBasePath base)
  let path = mkBasePath base
  wallets <- getDirectoryContents path
  pure
    $ map (mkWalletName . drop 7)
      . filter (isPrefixOf "wallet_")
    $ wallets

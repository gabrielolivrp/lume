{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Wallet.Types where

import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Crypto.Signature qualified as Sig

newtype WalletName = WalletName {getWalletName :: String}
  deriving (Show, Eq)

mkWalletName :: String -> WalletName
mkWalletName = WalletName

data Wallet = Wallet
  { wName :: WalletName
  , wPrivKey :: Sig.PrivateKey
  , wPubKey :: Sig.PublicKey
  , wAddr :: Addr.Address
  }
  deriving (Show, Eq)

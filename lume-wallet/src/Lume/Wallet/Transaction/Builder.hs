{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Transaction.Builder (
  -- * Types
  BuildUnsignedTxParams (..),

  -- * Errors
  TxBuilderError (..),

  -- * Functions
  buildUnsignedTx,
)
where

import Control.Lens
import Control.Monad

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Lume.Core.Crypto.Address (Address)
import Lume.Core.Transaction
import Lume.Wallet.Transaction.CoinSelection (coinSelection)

data TxBuilderError
  = WalletInsufficientFundsError
  | WalletNoUnspentOutputsError
  | WalletInvalidTransactionValueError Text
  | WalletInvalidTransactionFeeError Text
  deriving (Eq)

instance Show TxBuilderError where
  show WalletInsufficientFundsError = "Insufficient funds to complete the transaction."
  show WalletNoUnspentOutputsError = "No unspent outputs available to create a transaction."
  show (WalletInvalidTransactionValueError msg) = "Invalid transaction value: " ++ show msg
  show (WalletInvalidTransactionFeeError msg) = "Invalid transaction fee: " ++ show msg

data BuildUnsignedTxParams = BuildUnsignedTxParams
  { _sender :: Address
  , _recipient :: Address
  , _value :: Coin
  , _fee :: Coin
  }

buildUnsignedTx :: [UTXO] -> BuildUnsignedTxParams -> Either TxBuilderError Tx
buildUnsignedTx utxos (BuildUnsignedTxParams sender recipient value fee) = do
  when (value == 0) $ Left (WalletInvalidTransactionValueError "Value must be greater than 0")
  when (fee == 0) $ Left (WalletInvalidTransactionFeeError "Fee must be greater than 0")
  when (null utxos) $ Left WalletNoUnspentOutputsError
  let txTotal = value + fee
  case coinSelection utxos txTotal of
    Just (chosens, sumChosens) -> do
      let outpoints = makeOutpoints chosens
          outputs = makeOutputs sender recipient value fee sumChosens
      pure $ buildTx outpoints outputs
    Nothing -> Left WalletInsufficientFundsError
 where
  makeOutpoints :: NE.NonEmpty UTXO -> NE.NonEmpty Outpoint
  makeOutpoints = NE.map (\utxo -> Outpoint (utxo ^. utxoTxId) (utxo ^. utxoIdx))

  makeOutputs :: Address -> Address -> Coin -> Coin -> Coin -> NE.NonEmpty (Address, Coin)
  makeOutputs sender' recipient' value' fee' sumChosens
    | sender' == recipient' = NE.singleton (recipient', sumChosens - fee')
    | otherwise =
        let change = sumChosens - value' - fee'
         in if change > 0
              then NE.fromList [(recipient', value'), (sender', change)]
              else NE.singleton (recipient', value')

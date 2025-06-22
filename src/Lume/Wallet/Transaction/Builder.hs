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
import Control.Monad.Except (MonadError, throwError)
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
  deriving (Show, Eq)

data BuildUnsignedTxParams = BuildUnsignedTxParams
  { _sender :: Address
  , _recipient :: Address
  , _value :: Coin
  , _fee :: Coin
  }

buildUnsignedTx ::
  (Monad m, MonadError TxBuilderError m) =>
  [UTXO] ->
  BuildUnsignedTxParams ->
  m Tx
buildUnsignedTx utxos (BuildUnsignedTxParams sender recipient value fee) = do
  when (value == 0) $ throwError (WalletInvalidTransactionValueError "Value must be greater than 0")
  when (fee == 0) $ throwError (WalletInvalidTransactionFeeError "Fee must be greater than 0")

  when (null utxos) $ throwError WalletNoUnspentOutputsError

  let txTotal = value + fee
  case coinSelection utxos txTotal of
    Just (chosens, sumChosens) -> do
      let outpoints = makeOutpoints chosens
          outputs = makeOutputs sender recipient value fee sumChosens
      pure $ buildTx outpoints outputs
    Nothing -> throwError WalletInsufficientFundsError
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

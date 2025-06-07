{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Internal (
  -- * Types
  BuildUnsignedTxParams (..),

  -- * Errors
  WalletError (..),

  -- * Functions
  buildUnsignedTx,
)
where

import Control.Lens
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Lume.Crypto.Address (Address)
import Lume.Transaction

data WalletError
  = WalletInsufficientFundsError
  | WalletNoUnspentOutputsError
  | WalletInvalidTransactionValueError Text
  | WalletInvalidTransactionFeeError Text
  | WalletTransactionError TransactionError
  deriving (Show, Eq)

data BuildUnsignedTxParams = BuildUnsignedTxParams
  { _sender :: Address
  , _recipient :: Address
  , _value :: Coin
  , _fee :: Coin
  }

buildUnsignedTx ::
  (MonadError WalletError m) =>
  UtxoSet ->
  BuildUnsignedTxParams ->
  m Tx
buildUnsignedTx utxoSet (BuildUnsignedTxParams sender recipient value fee) = do
  when (value == 0) $ throwError (WalletInvalidTransactionValueError "Value must be greater than 0")
  when (fee == 0) $ throwError (WalletInvalidTransactionFeeError "Fee must be greater than 0")

  let utxos = unspentTransactions sender utxoSet
  when (null utxos) $ throwError WalletNoUnspentOutputsError

  let txTotal = value + fee
  case coinSelection utxos txTotal of
    Just (chosens, sumChosens) -> do
      let outpoints = makeOutpoints chosens
          outputs = makeOutputs sender recipient value fee sumChosens
      case buildTx outpoints outputs of
        Left err -> throwError $ WalletTransactionError err
        Right tx -> pure tx
    Nothing -> throwError WalletInsufficientFundsError
 where
  makeOutpoints :: NE.NonEmpty UTXO -> NE.NonEmpty Outpoint
  makeOutpoints = NE.map (\utxo -> Outpoint (utxo ^. utxoId) (utxo ^. utxoIdx))

  makeOutputs :: Address -> Address -> Coin -> Coin -> Coin -> NE.NonEmpty (Address, Coin)
  makeOutputs sender' recipient' value' fee' sumChosens
    | sender' == recipient' = NE.singleton (recipient', sumChosens - fee')
    | otherwise =
        let change = sumChosens - value' - fee'
         in if change > 0
              then NE.fromList [(recipient', value'), (sender', change)]
              else NE.singleton (recipient', value')

-- | smallest-first coin selection algorithm
coinSelection :: [UTXO] -> Coin -> Maybe (NE.NonEmpty UTXO, Coin)
coinSelection utxos target = go (sortOn (^. utxoValue) utxos) 0 []
 where
  go [] total acc
    | total >= target = Just (NE.fromList $ reverse acc, total)
    | otherwise = Nothing
  go (u : us) total acc
    | total >= target = Just (NE.fromList $ reverse acc, total)
    | otherwise = go us (total + u ^. utxoValue) (u : acc)

unspentTransactions :: Address -> UtxoSet -> [UTXO]
unspentTransactions addr (UtxoSet setMap) =
  [ utxo
  | utxo <- M.elems setMap
  , utxo ^. utxoOwner == addr
  ]

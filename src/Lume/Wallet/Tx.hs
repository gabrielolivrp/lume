{-# LANGUAGE DerivingStrategies #-}

module Lume.Wallet.Tx where

import Control.Lens
import Control.Monad
import Data.List (sortOn)
import qualified Data.Map as M
import Lume.Crypto.Address (Address)
import Lume.Transaction.Amount (Amount)
import Lume.Transaction.Builder (buildTx)
import Lume.Transaction.Types

newtype WalletException = WalletException String
  deriving stock (Show)

data BuildUnsignedTxParams = BuildUnsignedTxParams
  { _sender :: Address
  , _recipient :: Address
  , _value :: Amount
  , _fee :: Amount
  }

buildUnsignedTx :: UtxoSet -> BuildUnsignedTxParams -> Either WalletException Tx
buildUnsignedTx utxoSet (BuildUnsignedTxParams sender recipient value fee) = do
  when (value <= 0) $ Left (WalletException "Transaction value must be > 0")
  when (fee < 0) $ Left (WalletException "Transaction fee cannot be negative")

  let utxos = unspentTransactions sender utxoSet
  when (null utxos) $ Left (WalletException "No unspent UTXOs available")

  let targetTotal = value + fee
  case coinSelection utxos targetTotal of
    Just (chosens, sumChosen) -> do
      let outpoints = makeOutpoint chosens
          outputs = makeOutputs sender recipient value fee sumChosen
      case buildTx outpoints outputs of
        Left err -> Left $ WalletException $ "Failed to build transaction: " ++ show err
        Right tx -> Right tx
    Nothing -> Left (WalletException "Insufficient funds")
 where
  makeOutpoint :: [UTXO] -> [Outpoint]
  makeOutpoint = map (\utxo -> Outpoint (utxo ^. utxoId) (utxo ^. utxoIdx))

  makeOutputs :: Address -> Address -> Amount -> Amount -> Amount -> [(Address, Amount)]
  makeOutputs sender' recipient' value' fee' sumChosen
    | sender' == recipient' = [(recipient', sumChosen - fee')]
    | otherwise =
        let change = sumChosen - value' - fee'
         in if change > 0
              then [(recipient', value'), (sender', change)]
              else [(recipient', value')]

-- | smallest-first coin selection algorithm
coinSelection :: [UTXO] -> Amount -> Maybe ([UTXO], Amount)
coinSelection utxos target = go (sortOn (^. utxoValue) utxos) 0 []
 where
  go [] total acc
    | total >= target = Just (reverse acc, total)
    | otherwise = Nothing
  go (u : us) total acc
    | total >= target = Just (reverse acc, total)
    | otherwise = go us (total + u ^. utxoValue) (u : acc)

unspentTransactions :: Address -> UtxoSet -> [UTXO]
unspentTransactions addr (UtxoSet setMap) =
  [ utxo
  | utxo <- M.elems setMap
  , utxo ^. utxoOwner == addr
  ]

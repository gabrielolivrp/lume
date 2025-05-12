{-# LANGUAGE DerivingStrategies #-}

module Lume.Wallet.Tx where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Lume.Crypto.Address (Address)
import Lume.Transaction.Amount (Amount, safeSubtract)
import Lume.Transaction.Builder (buildTx)
import Lume.Transaction.Types (Outpoint (Outpoint), Tx, UTXO, UtxoSet (UtxoSet), utxoId, utxoIdx, utxoOwner, utxoValue)

newtype WalletException = WalletException String
  deriving (Show)

buildUnsignedTx ::
  UtxoSet ->
  Address ->
  Address ->
  Amount ->
  Either WalletException Tx
buildUnsignedTx utxoSet addr to' amount = do
  let utxosAvailable = unspentTransactions addr utxoSet
  when (null utxosAvailable) $ Left (WalletException "No unspent transactions available")

  let (utxoSelecteds, utxoAmount) = selectUTXOs addr amount utxosAvailable
  when (null utxoSelecteds) $ Left (WalletException "No UTXOs selected")
  when (utxoAmount < amount) $ Left (WalletException "Insufficient funds")

  let outpoints = toOutpoints utxoSelecteds
  outs <- toOuts addr to' amount utxoAmount

  case buildTx outpoints outs of
    Left err -> Left $ WalletException ("Failed to build transaction: " ++ show err)
    Right tx' -> Right tx'
 where
  toOuts fromAddr toAddr txAmount utxoAmount
    | fromAddr == toAddr = Right [(fromAddr, txAmount)]
    | utxoAmount > txAmount =
        case safeSubtract utxoAmount txAmount of
          Just remainingAmount -> Right [(fromAddr, remainingAmount), (toAddr, txAmount)]
          Nothing ->
            Left (WalletException "Insufficient funds after subtracting transaction amount")
    | otherwise = Right [(toAddr, txAmount)]

  toOutpoints = map (\utxo -> Outpoint (utxo ^. utxoId) (utxo ^. utxoIdx))

selectUTXOs :: Address -> Amount -> [UTXO] -> ([UTXO], Amount)
selectUTXOs addr amount =
  foldl
    ( \acc@(utxos, amt) utxo ->
        let
          isOwned = addr == (utxo ^. utxoOwner)
          isValid = (utxo ^. utxoValue) <= amount
         in
          if isOwned && isValid
            then (utxos ++ [utxo], amt + (utxo ^. utxoValue))
            else acc
    )
    ([], 0)

unspentTransactions :: Address -> UtxoSet -> [UTXO]
unspentTransactions addr (UtxoSet utxoSet) =
  let
    utxoList = M.toList utxoSet
    spendable = filter (\(_, utxo) -> utxo ^. utxoOwner == addr) utxoList
   in
    map snd spendable

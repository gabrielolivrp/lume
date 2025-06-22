{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Wallet.Transaction.CoinSelection where

import Control.Lens
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Lume.Core.Transaction

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

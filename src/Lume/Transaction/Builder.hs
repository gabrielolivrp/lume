{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Transaction.Builder (
  buildTx,
  signTx,
  tVersion,
  SignTxError (..),
  TxBuilderError (..),
  SigInput (..),
)
where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.ByteString qualified as BS

import Data.Foldable (find, toList)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32)
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash (Hash, ToHash (toHash), toRawBytes)
import Lume.Crypto.Signature (PrivateKey, emptyPublicKey, emptySignature, sign, toPublicKey)
import Lume.Transaction.Amount (Amount, maxAmount)
import Lume.Transaction.Types

data SignTxError
  = CoinbaseTxCannotBeSigned
  | MissingSignatureInput TxIn
  deriving (Show, Eq)

data TxBuilderError
  = ValueExceedsMaxAmount
  | TxInC
  deriving (Show, Eq)

data SigInput = SigInput
  { sigOutpoint :: Outpoint
  , sigPrivKey :: PrivateKey
  }

tVersion :: Word32
tVersion = 0

buildTx ::
  (MonadError TxBuilderError m) =>
  NE.NonEmpty Outpoint ->
  NE.NonEmpty (Address, Amount) ->
  m Tx
buildTx xs ys = do
  let txin = NE.map (\x -> TxIn x emptySignature emptyPublicKey) xs
  txout <- mapM buildTxOut ys
  pure (Tx (toList txin) (toList txout) tVersion)
 where
  buildTxOut (addr, value) = do
    when (value >= maxAmount) (throwError ValueExceedsMaxAmount)
    pure (TxOut addr value)

signTx :: (MonadError SignTxError m) => Tx -> NE.NonEmpty SigInput -> m Tx
signTx tx sigins = do
  when (isCoinbase tx) (throwError CoinbaseTxCannotBeSigned)
  let hash = toRawBytes . noSigTxHash $ tx
  txins' <- mapM (go hash) (tx ^. txIn)
  pure (tx & txIn .~ txins')
 where
  go txHash txin =
    case findSigInput txin sigins of
      Just siginput -> pure $ signTxIn txin txHash (sigPrivKey siginput)
      Nothing -> throwError (MissingSignatureInput txin)

  findSigInput txin = find (\siginput -> txin ^. txInPrevOut == sigOutpoint siginput)

signTxIn :: TxIn -> BS.ByteString -> PrivateKey -> TxIn
signTxIn txin hash sk =
  let signature = sign sk hash
      pk = toPublicKey sk
   in txin
        & txInPubKey .~ pk
        & txInSignature .~ signature

noSigTxHash :: Tx -> Hash
noSigTxHash = toHash . over (txIn . mapped) sanitize
 where
  sanitize txin =
    txin
      & txInPubKey .~ emptyPublicKey
      & txInSignature .~ emptySignature

{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Transaction.Builder (
  buildTx,
  signTx,
  tVersion,
  SignTxException (..),
  BuildTxException (..),
  SigInput (..),
)
where

import Control.Lens
import Control.Lens.Internal.CTypes (Word32)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.List
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash (Hash, ToHash (toHash), toRawBytes)
import Lume.Crypto.Signature (PrivateKey, emptyPublicKey, emptySignature, sign, toPublicKey)
import Lume.Transaction.Amount (Amount, maxAmount)
import Lume.Transaction.Types

newtype SignTxException = SignTxException String
  deriving (Show)

newtype BuildTxException = BuildTxException String
  deriving (Show)

data SigInput = SigInput
  { sigOutpoint :: Outpoint
  , sigPrivKey :: PrivateKey
  }

tVersion :: Word32
tVersion = 0

buildTx ::
  [Outpoint] ->
  [(Address, Amount)] ->
  Either BuildTxException Tx
buildTx xs ys = do
  let txin = map (\x -> TxIn x emptySignature emptyPublicKey) xs
  txout <- mapM buildTxOut ys
  pure (Tx txin txout tVersion)
 where
  buildTxOut (addr, amount) = do
    when (amount >= maxAmount) (Left $ BuildTxException "Invalid amount")
    pure (TxOut addr amount)

signTx :: Tx -> [SigInput] -> Either SignTxException Tx
signTx tx sigins = do
  when (isCoinbase tx) (Left $ SignTxException "Coinbase transaction cannot be signed")
  when (null sigins) (Left $ SignTxException "No signature inputs provided")
  let hash = toRawBytes . noSigTxHash $ tx
  txins' <- mapM (go hash) (tx ^. txIn)
  pure (tx & txIn .~ txins')
 where
  go txHash txin = case findSigInput txin sigins of
    Just siginput -> pure $ signTxIn txin txHash (sigPrivKey siginput)
    Nothing -> Left $ SignTxException "Missing signature input"

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

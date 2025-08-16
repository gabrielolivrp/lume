{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Wallet.Transaction.Signer where

import Control.Lens
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (find)
import Data.List.NonEmpty qualified as NE
import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Core.Transaction

data SigTxIn = SigTxIn
  { sigOutpoint :: Outpoint
  , sigPrivKey :: Sig.PrivateKey
  }

data SignTransactionError
  = SignTransactionCoinbaseNotAllowedError
  | SignTransactionMissingInputError TxIn
  deriving (Show, Eq)

signTx :: Tx -> NE.NonEmpty SigTxIn -> Either SignTransactionError Tx
signTx tx sigins = do
  when (isCoinbase tx) (Left SignTransactionCoinbaseNotAllowedError)
  let hash = Hash.toRawBytes . txHash $ tx
  txins' <- mapM (go hash) (tx ^. txIn)
  Right (tx & txIn .~ txins')
 where
  go hash txin =
    case findSig txin sigins of
      Just sig -> Right $ signTxIn txin hash (sigPrivKey sig)
      Nothing -> Left (SignTransactionMissingInputError txin)

  findSig txin = find (\sig -> txin ^. txInPrevOut == sigOutpoint sig)

signTxIn :: TxIn -> BS.ByteString -> Sig.PrivateKey -> TxIn
signTxIn txin hash sk =
  let signature = Sig.sign sk hash
      pk = Sig.toPublicKey sk
   in txin
        & txInPubKey .~ pk
        & txInSignature .~ signature

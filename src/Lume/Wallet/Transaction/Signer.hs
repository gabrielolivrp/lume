{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Wallet.Transaction.Signer where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
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

signTx :: (MonadError SignTransactionError m) => Tx -> NE.NonEmpty SigTxIn -> m Tx
signTx tx sigins = do
  when (isCoinbase tx) (throwError SignTransactionCoinbaseNotAllowedError)
  let hash = Hash.toRawBytes . hashTx $ tx
  txins' <- mapM (go hash) (tx ^. txIn)
  pure (tx & txIn .~ txins')
 where
  go txHash txin =
    case findSig txin sigins of
      Just sig -> pure $ signTxIn txin txHash (sigPrivKey sig)
      Nothing -> throwError (SignTransactionMissingInputError txin)

  findSig txin = find (\sig -> txin ^. txInPrevOut == sigOutpoint sig)

signTxIn :: TxIn -> BS.ByteString -> Sig.PrivateKey -> TxIn
signTxIn txin hash sk =
  let signature = Sig.sign sk hash
      pk = Sig.toPublicKey sk
   in txin
        & txInPubKey .~ pk
        & txInSignature .~ signature

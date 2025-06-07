{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Transaction.Internal (
  -- * Types
  Outpoint (..),
  TxIn (..),
  TxOut (..),
  Tx (..),
  Txs (..),
  UTXO (..),
  UtxoSet (..),
  SigInput (..),

  -- * Lenses
  outpId,
  outpIdx,
  txInPrevOut,
  txInSignature,
  txInPubKey,
  txOutAddress,
  txOutValue,
  txIn,
  txOut,
  txVersion,
  utxoId,
  utxoIdx,
  utxoOwner,
  utxoValue,

  -- * Functions
  transactionVersion,
  isCoinbase,
  buildTx,
  buildCoinbaseTx,
  buildTxOut,
  signTx,
  signTxIn,
  noSigTxHash,
  validateTx,

  -- * Errors
  SignTransactionError (..),
  TransactionError (..),
)
where

import Control.Lens
import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (throwError))
import Data.Binary (Binary)
import Data.ByteString qualified as BS
import Data.Foldable (find, toList)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lume.Crypto.Address (Address)
import Lume.Crypto.Hash qualified as Hash
import Lume.Crypto.Signature qualified as Sig
import Lume.Transaction.Coin (Coin, maxCoin)

data Outpoint = Outpoint
  { _outpId :: !Hash.Hash
  -- ^ Hash of the transaction
  , _outpIdx :: !Word64
  -- ^ Output index within the source transaction
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

makeLenses ''Outpoint

data TxIn = TxIn
  { _txInPrevOut :: !Outpoint
  -- ^ Previous output of the transaction (UTXO)
  , _txInSignature :: !Sig.Signature
  -- ^ Signature of the transaction
  , _txInPubKey :: !Sig.PublicKey
  -- ^ Public key of the transaction
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxIn

data TxOut = TxOut
  { _txOutAddress :: !Address
  -- ^ Address that will receive the funds
  , _txOutValue :: !Coin
  -- ^ Amount to be transferred
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''TxOut

data Tx = Tx
  { _txIn :: ![TxIn]
  -- ^ Inputs of the transaction
  , _txOut :: ![TxOut]
  -- ^ Outputs of the transaction
  , _txVersion :: !Word32
  -- ^ Version of the transaction
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, Hash.ToHash)

makeLenses ''Tx

newtype Txs = Txs {getTxs :: NE.NonEmpty Tx}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''Txs

data UTXO = UTXO
  { _utxoId :: !Hash.Hash
  -- ^ Hash of the transaction
  , _utxoIdx :: !Word64
  -- ^ Index of the output in the transaction
  , _utxoOwner :: !Address
  -- ^ Address that owns this unspent output
  , _utxoValue :: !Coin
  -- ^ Amount of money in the output
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

makeLenses ''UTXO

newtype UtxoSet = UtxoSet (M.Map Outpoint UTXO)
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

-- | A transaction is a coinbase transaction if it has no inputs.
isCoinbase :: Tx -> Bool
isCoinbase tx = null (tx ^. txIn)

transactionVersion :: Word32
transactionVersion = 0x00000001
{-# INLINE transactionVersion #-}

data SignTransactionError
  = SignTransactionCoinbaseNotAllowedError
  | SignTransactionMissingInputError TxIn
  deriving (Show, Eq)

data TransactionError
  = TransactionInvalidError
  | TransactionInvalidSignatureError
  | TransactionInsufficientFundsError
  | TransactionBuilderValueOverflowError
  deriving (Show, Eq)

data SigInput = SigInput
  { sigOutpoint :: Outpoint
  , sigPrivKey :: Sig.PrivateKey
  }

buildTx ::
  (MonadError TransactionError m) =>
  NE.NonEmpty Outpoint ->
  NE.NonEmpty (Address, Coin) ->
  m Tx
buildTx xs ys = do
  let txin = NE.map (\x -> TxIn x Sig.emptySignature Sig.emptyPublicKey) xs
  txout <- mapM buildTxOut ys
  pure (Tx (toList txin) (toList txout) transactionVersion)

buildCoinbaseTx ::
  (MonadError TransactionError m) =>
  NE.NonEmpty (Address, Coin) ->
  m Tx
buildCoinbaseTx xs = do
  txout <- mapM buildTxOut xs
  pure $
    Tx
      { _txIn = []
      , _txOut = toList txout
      , _txVersion = transactionVersion
      }

buildTxOut :: (MonadError TransactionError m) => (Address, Coin) -> m TxOut
buildTxOut (addr, value) = do
  when (value >= maxCoin) (throwError TransactionBuilderValueOverflowError)
  pure (TxOut addr value)

signTx :: (MonadError SignTransactionError m) => Tx -> NE.NonEmpty SigInput -> m Tx
signTx tx sigins = do
  when (isCoinbase tx) (throwError SignTransactionCoinbaseNotAllowedError)
  let hash = Hash.toRawBytes . noSigTxHash $ tx
  txins' <- mapM (go hash) (tx ^. txIn)
  pure (tx & txIn .~ txins')
 where
  go txHash txin =
    case findSigInput txin sigins of
      Just siginput -> pure $ signTxIn txin txHash (sigPrivKey siginput)
      Nothing -> throwError (SignTransactionMissingInputError txin)

  findSigInput txin = find (\siginput -> txin ^. txInPrevOut == sigOutpoint siginput)

signTxIn :: TxIn -> BS.ByteString -> Sig.PrivateKey -> TxIn
signTxIn txin hash sk =
  let signature = Sig.sign sk hash
      pk = Sig.toPublicKey sk
   in txin
        & txInPubKey .~ pk
        & txInSignature .~ signature

noSigTxHash :: Tx -> Hash.Hash
noSigTxHash = Hash.toHash . over (txIn . mapped) sanitize
 where
  sanitize txin =
    txin
      & txInPubKey .~ Sig.emptyPublicKey
      & txInSignature .~ Sig.emptySignature

validateTx :: (MonadError TransactionError m) => UtxoSet -> Tx -> m ()
validateTx (UtxoSet utxos) tx = do
  let inputs = tx ^. txIn
      outputs = tx ^. txOut

  inValue <- forM inputs $ \txin -> do
    let prevOut = txin ^. txInPrevOut
    txout <- case M.lookup prevOut utxos of
      Just utxo -> return utxo
      Nothing -> throwError TransactionInvalidError
    let pubKey = txin ^. txInPubKey
        sig = txin ^. txInSignature
        msg = Hash.toRawBytes $ noSigTxHash tx
    unless (Sig.verify pubKey msg sig) $ throwError TransactionInvalidSignatureError
    pure $ txout ^. utxoValue

  let outValue = map (^. txOutValue) outputs
      totalIn = sum inValue
      totalOut = sum outValue

  when (totalIn < totalOut) $ throwError TransactionInsufficientFundsError

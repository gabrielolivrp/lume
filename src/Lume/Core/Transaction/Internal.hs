{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lume.Core.Transaction.Internal (
  -- * Types
  Outpoint (..),
  TxIn (..),
  TxOut (..),
  Tx (..),
  Txs (..),
  UTXO (..),
  UtxoSet (..),

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
  utxoTxId,
  utxoIdx,
  utxoOwner,
  utxoValue,

  -- * Errors
  TransactionError (..),

  -- * Validations
  validateTx,

  -- * Functions
  transactionVersion,
  isCoinbase,
  txHash,
  buildCoinbaseTx,
  buildTx,
)
where

import Control.Lens hiding ((.=))
import Control.Monad (forM, unless, when)
import Data.Aeson
import Data.Binary (Binary (get, put))
import Data.Foldable (toList)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lume.Core.Crypto.Address (Address, fromPublicKey)
import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Core.Transaction.Coin (Coin)

data Outpoint = Outpoint
  { _outpId :: !Hash.Hash
  -- ^ Hash of the transaction
  , _outpIdx :: !Word32
  -- ^ Output index within the source transaction
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Outpoint where
  put (Outpoint txId idx) = do
    put txId
    put idx

instance ToJSON Outpoint where
  toJSON (Outpoint txId idx) =
    object
      [ "tx_id" .= txId
      , "idx" .= idx
      ]

instance FromJSON Outpoint where
  parseJSON = withObject "Outpoint" $ \v ->
    Outpoint
      <$> v .: "tx_id"
      <*> v .: "idx"

makeLenses ''Outpoint

data TxIn = TxIn
  { _txInPrevOut :: !Outpoint
  -- ^ Previous output of the transaction (UTXO)
  , _txInSignature :: !Sig.Signature
  -- ^ Signature of the transaction
  , _txInPubKey :: !Sig.PublicKey
  -- ^ Public key of the transaction
  }
  deriving (Show, Eq, Generic)

instance Binary TxIn where
  put (TxIn prevOut signature pubKey) = do
    put prevOut
    put signature
    put pubKey
  get = TxIn <$> get <*> get <*> get

makeLenses ''TxIn

instance ToJSON TxIn where
  toJSON (TxIn prevOut signature pubKey) =
    object
      [ "prev_out" .= prevOut
      , "signature" .= signature
      , "pub_key" .= pubKey
      ]

instance FromJSON TxIn where
  parseJSON = withObject "TxIn" $ \v ->
    TxIn
      <$> v .: "prev_out"
      <*> v .: "signature"
      <*> v .: "pub_key"

data TxOut = TxOut
  { _txOutAddress :: !Address
  -- ^ Address that will receive the funds
  , _txOutValue :: !Coin
  -- ^ Amount to be transferred
  }
  deriving (Show, Eq, Generic)

instance Binary TxOut where
  put (TxOut address value) = do
    put address
    put value
  get = TxOut <$> get <*> get

makeLenses ''TxOut

instance ToJSON TxOut where
  toJSON (TxOut address value) =
    object
      [ "address" .= address
      , "value" .= value
      ]

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \v ->
    TxOut
      <$> v .: "address"
      <*> v .: "value"

data Tx = Tx
  { _txIn :: ![TxIn]
  -- ^ Inputs of the transaction
  , _txOut :: ![TxOut]
  -- ^ Outputs of the transaction
  , _txVersion :: !Word32
  -- ^ Version of the transaction
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hash.ToHash)

instance Binary Tx where
  put (Tx inputs outputs version) = do
    put inputs
    put outputs
    put version
  get = Tx <$> get <*> get <*> get

instance ToJSON Tx where
  toJSON (Tx inputs outputs version) =
    object
      [ "inputs" .= inputs
      , "outputs" .= outputs
      , "version" .= version
      ]

instance FromJSON Tx where
  parseJSON = withObject "Tx" $ \v ->
    Tx
      <$> v .: "inputs"
      <*> v .: "outputs"
      <*> v .: "version"

makeLenses ''Tx

newtype Txs = Txs {getTxs :: NE.NonEmpty Tx}
  deriving (Show, Eq, Generic)

instance Binary Txs where
  put (Txs txs) = put (NE.toList txs)
  get = Txs . NE.fromList <$> get

instance ToJSON Txs where
  toJSON (Txs txs) = toJSON $ map toJSON (NE.toList txs)

instance FromJSON Txs where
  parseJSON = withArray "Txs" $ \arr -> do
    txs <- mapM parseJSON (toList arr)
    case NE.nonEmpty txs of
      Just nonEmptyTxs -> pure (Txs nonEmptyTxs)
      Nothing -> fail "Transaction list cannot be empty"

makeLenses ''Txs

data UTXO = UTXO
  { _utxoTxId :: !Hash.Hash
  -- ^ Hash of the transaction
  , _utxoIdx :: !Word32
  -- ^ Index of the output in the transaction
  , _utxoOwner :: !Address
  -- ^ Address that owns this unspent output
  , _utxoValue :: !Coin
  -- ^ Amount of money in the output
  }
  deriving (Show, Eq, Generic)

makeLenses ''UTXO

newtype UtxoSet = UtxoSet (M.Map Outpoint UTXO)
  deriving (Show, Generic)

transactionVersion :: Word32
transactionVersion = 0x00000001
{-# INLINE transactionVersion #-}

-- | A transaction is a coinbase transaction if it has no inputs.
isCoinbase :: Tx -> Bool
isCoinbase tx = null (tx ^. txIn)
{-# INLINE isCoinbase #-}

------------------------
-- Transaction builders
------------------------

buildTx :: NE.NonEmpty Outpoint -> NE.NonEmpty (Address, Coin) -> Tx
buildTx xs ys = do
  let txin = NE.map (\outpoint -> TxIn outpoint Sig.emptySignature Sig.emptyKey) xs
      txout = NE.map (uncurry TxOut) ys
   in Tx (toList txin) (toList txout) transactionVersion

buildCoinbaseTx :: NE.NonEmpty (Address, Coin) -> Tx
buildCoinbaseTx xs =
  Tx
    { _txIn = []
    , _txOut = toList $ NE.map (uncurry TxOut) xs
    , _txVersion = transactionVersion
    }

txHash :: Tx -> Hash.Hash
txHash = Hash.toHash . over (txIn . mapped) sanitize
 where
  sanitize txin =
    txin
      & txInPubKey .~ Sig.emptyKey
      & txInSignature .~ Sig.emptySignature

----------------------------
-- Transaction Validations
----------------------------

data TransactionError
  = TransactionInvalidError
  | TransactionDuplicateInputError
  | TransactionInsufficientFundsError
  | TransactionBuilderValueOverflowError
  | InvalidInputOwnershipError TxIn
  | TransactionInvalidSignatureError TxIn
  | TransactionUtxoNotFoundError TxIn
  | TransactionUnableToDeriveAddressError TxIn
  deriving (Show, Eq)

validateTx :: UtxoSet -> Tx -> Either TransactionError ()
validateTx (UtxoSet utxos) tx = do
  let inputs = tx ^. txIn
      outputs = tx ^. txOut

  inValue <- forM inputs $ \txin -> do
    let prevOut = txin ^. txInPrevOut

    utxo <- case M.lookup prevOut utxos of
      Just utxo -> pure utxo
      Nothing -> Left $ TransactionUtxoNotFoundError txin

    txinAddr <- case fromPublicKey (txin ^. txInPubKey) of
      Right addr -> pure addr
      Left _ -> Left $ TransactionUnableToDeriveAddressError txin

    when (utxo ^. utxoOwner /= txinAddr) $
      Left (InvalidInputOwnershipError txin)

    let pubKey = txin ^. txInPubKey
        sig = txin ^. txInSignature
        msg = Hash.toRawBytes $ txHash tx

    unless (Sig.verify pubKey msg sig) $
      Left (TransactionInvalidSignatureError txin)

    pure $ utxo ^. utxoValue

  let outValue = map (^. txOutValue) outputs
      totalIn = sum inValue
      totalOut = sum outValue

  when (length inputs /= length (nub (map (^. txInPrevOut) inputs))) $
    Left TransactionDuplicateInputError

  when (totalIn < totalOut) $ Left TransactionInsufficientFundsError

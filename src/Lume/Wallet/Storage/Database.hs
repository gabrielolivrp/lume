{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lume.Wallet.Storage.Database (
  Database,
  DatabaseError (..),
  openDatabase,
  storeWallet,
  loadWallet,
  storeUTXO,
  markUTXOAsSpent,
  getUTXOs,
  WalletModel (..),
  UTXOModel (..),
)
where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word (Word64)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (ToField (toField))
import Lume.Core.Crypto.Hash qualified as Hash

data DatabaseError
  = DatabaseError String
  deriving (Show, Eq)

newtype Database = Database
  { dbConn :: Connection
  }

type DatabaseM m = (MonadIO m, MonadError DatabaseError m)

walletSchema :: Query
walletSchema =
  "CREATE TABLE IF NOT EXISTS wallet (\n\
  \  id INTEGER PRIMARY KEY CHECK (id = 1),\n\
  \  name TEXT NOT NULL,\n\
  \  public_key BLOB NOT NULL,\n\
  \  private_key BLOB NOT NULL,\n\
  \  address TEXT NOT NULL\n\
  \);"

utxoSchema :: Query
utxoSchema =
  "CREATE TABLE IF NOT EXISTS utxos (\n\
  \  tx_id TEXT NOT NULL,\n\
  \  output_index INTEGER NOT NULL,\n\
  \  address TEXT NOT NULL,\n\
  \  amount INTEGER NOT NULL,\n\
  \  spent BOOLEAN NOT NULL DEFAULT 0,\n\
  \  UNIQUE(tx_id, output_index)\n\
  \);"

openDatabase :: (DatabaseM m) => FilePath -> m Database
openDatabase path = do
  result <- liftIO . try $ do
    conn <- open path
    execute_ conn walletSchema
    execute_ conn utxoSchema
    pure conn
  case result of
    Left (e :: SomeException) -> throwError . DatabaseError $ show e
    Right conn -> pure $ Database conn

withQuery :: (DatabaseM m) => Database -> (Connection -> IO a) -> m a
withQuery db action = do
  result <- liftIO . try $ action (dbConn db)
  case result of
    Left (e :: SomeException) -> throwError . DatabaseError $ show e
    Right res -> pure res

-------------------
-- Database Models
-------------------

data WalletModel = WalletModel
  { wmName :: T.Text
  -- ^ Name of the wallet
  , wmPublicKey :: BS.ByteString
  -- ^ Public key of the wallet
  , wmPrivateKey :: BS.ByteString
  -- ^ Private key of the wallet
  , wmAddress :: T.Text
  -- ^ Address associated with the wallet
  }
  deriving (Show, Eq)

instance ToRow WalletModel where
  toRow (WalletModel name publicKey privateKey address) =
    [ toField name
    , toField publicKey
    , toField privateKey
    , toField address
    ]

instance FromRow WalletModel where
  fromRow =
    WalletModel
      <$> field
      <*> field
      <*> field
      <*> field

data UTXOModel = UTXOModel
  { umTxId :: BS.ByteString
  -- ^ Transaction ID
  , umOutIdx :: Word64
  -- ^ Output index in the transaction
  , umAddress :: T.Text
  -- ^ Address associated with the output
  , umAmount :: Integer
  -- ^ Amount of the output
  , umSpent :: Bool
  -- ^ Whether the output has been spent
  }
  deriving (Show, Eq)

instance ToRow UTXOModel where
  toRow (UTXOModel txid outIdx address amount spent) =
    [ toField txid
    , toField outIdx
    , toField address
    , toField amount
    , toField spent
    ]

instance FromRow UTXOModel where
  fromRow =
    UTXOModel
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

storeWallet :: (DatabaseM m) => Database -> WalletModel -> m ()
storeWallet db wm =
  let q = "INSERT OR REPLACE INTO wallet (id, name, public_key, private_key, address) VALUES (1, ?, ?, ?, ?)"
   in withQuery db $ \conn -> execute conn q wm

loadWallet :: (DatabaseM m) => Database -> m (Maybe WalletModel)
loadWallet db = do
  let q = "SELECT name, public_key, private_key, address FROM wallet WHERE id = 1"
  result <- withQuery db (`query_` q)
  case result of
    [] -> pure Nothing
    (wm : _) -> pure $ Just wm

storeUTXO :: (DatabaseM m) => Database -> UTXOModel -> m ()
storeUTXO db utxo =
  let q = "INSERT OR REPLACE INTO utxos (tx_id, output_index, address, amount, spent) VALUES (?, ?, ?, ?, ?)"
   in withQuery db $ \conn -> execute conn q utxo

markUTXOAsSpent :: (DatabaseM m) => Database -> Hash.Hash -> Word64 -> m ()
markUTXOAsSpent db txid outIdx =
  let q = "UPDATE utxos SET spent = 1 WHERE tx_id = ? AND output_index = ?"
   in withQuery db $ \conn -> execute conn q (toField $ Hash.toHex txid, toField outIdx)

getUTXOs :: (DatabaseM m) => Database -> m [UTXOModel]
getUTXOs db =
  let q = "SELECT tx_id, output_index, address, amount, spent FROM utxos WHERE spent = 0"
   in withQuery db (`query_` q)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lume.Wallet.Internal (
  WalletName,
  getWalletName,
  Wallet (..),
  WalletStorage (..),
  WalletError (..),
  mkWalletName,
  mkWalletStorage,
  newWallet,
  checkWalletExists,
  loadAllWallets,
  sendTransaction,
)
where

import Control.Exception (SomeException, try)
import Control.Monad.Except (ExceptT, MonadError, MonadIO (liftIO), runExceptT, throwError, when)
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Word (Word64)
import Lume.Core.Crypto.Address (Address)
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Core.Transaction (Coin, Outpoint (Outpoint), UTXO (UTXO))
import Lume.Wallet.Storage.Database qualified as DB
import Lume.Wallet.Transaction (SigTxIn (SigTxIn), signTx)
import Lume.Wallet.Transaction.Builder (BuildUnsignedTxParams (BuildUnsignedTxParams), TxBuilderError, buildUnsignedTx)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

newtype WalletName = WalletName {getWalletName :: T.Text}
  deriving (Show, Eq)

data Wallet = Wallet
  { wName :: WalletName
  , wPrivKey :: Sig.PrivateKey
  , wPubKey :: Sig.PublicKey
  , wAddr :: Addr.Address
  }
  deriving (Show, Eq)

data WalletError
  = WalletDatabaseError DB.DatabaseError
  | WalletCryptoError T.Text
  | WalletAlreadyExistsError WalletName
  | WalletNotFoundError WalletName
  | WalletTransactionBuilderError TxBuilderError
  deriving (Show, Eq)

type WalletM m = (MonadIO m, MonadError WalletError m)

data WalletStorage = WalletStorage
  { store :: forall m. (WalletM m) => Wallet -> m ()
  -- ^ Store a wallet
  , load :: forall m. (WalletM m) => m (Maybe Wallet)
  -- ^ Load a wallet from storage
  , storeUTXO :: forall m. (WalletM m) => UTXO -> m ()
  -- ^ Store a UTXO in the wallet storage
  , markUTXOAsSpent :: forall m. (WalletM m) => Hash.Hash -> Word64 -> m ()
  -- ^ Mark a UTXO as spent in the wallet storage
  , getUTXOs :: forall m. (WalletM m) => m [UTXO]
  -- ^ Get all UTXOs from the wallet storage
  }

mkWalletName :: String -> WalletName
mkWalletName name = WalletName (T.pack name)

mkWalletPath :: FilePath -> WalletName -> FilePath
mkWalletPath basePath walletName = basePath </> "wallets" </> "wallet_" <> T.unpack (getWalletName walletName)

mkWalletStorage :: (WalletM m) => FilePath -> WalletName -> m WalletStorage
mkWalletStorage basePath walletName = do
  let walletPath = mkWalletPath basePath walletName
  -- Create the wallet directory if it doesn't exist
  liftIO $ createDirectoryIfMissing True walletPath

  database <- liftDB $ DB.openDatabase (walletPath </> "wallet.db")
  pure $
    WalletStorage
      { store = storeWallet' database
      , load = loadWallet' database
      , storeUTXO = storeUTXO' database
      , markUTXOAsSpent = markUTXOAsSpent' database
      , getUTXOs = getUTXOs' database
      }

checkWalletExists :: (WalletM m) => FilePath -> WalletName -> m Bool
checkWalletExists basePath walletName = do
  let walletPath = mkWalletPath basePath walletName
  liftIO $ doesDirectoryExist walletPath

newWallet :: (MonadIO m, MonadError WalletError m) => FilePath -> WalletName -> m Wallet
newWallet basePath walletName = do
  exists <- checkWalletExists basePath walletName
  when exists (throwError $ WalletAlreadyExistsError walletName)

  result <- liftIO . try $ Sig.generateKeyPair
  case result of
    Left (e :: SomeException) ->
      throwError . WalletCryptoError $ "Failed to generate key pair: " <> T.pack (show e)
    Right (Sig.KeyPair (pk, sk)) -> do
      address <- case Addr.fromPublicKey pk of
        Left e -> throwError . WalletCryptoError $ "Failed to generate address from public key: " <> T.pack (show e)
        Right address' -> pure address'
      pure $ Wallet walletName sk pk address

storeWallet' :: (WalletM m) => DB.Database -> Wallet -> m ()
storeWallet' database (Wallet walletName privKey pubKey (Addr.Address addr)) = do
  let wm =
        DB.WalletModel
          { DB.wmName = getWalletName walletName
          , DB.wmPublicKey = Sig.toRawBytes pubKey
          , DB.wmPrivateKey = Sig.toRawBytes privKey
          , DB.wmAddress = addr
          }
  liftDB $ DB.storeWallet database wm

loadWallet' :: (WalletM m) => DB.Database -> m (Maybe Wallet)
loadWallet' database = do
  wm <- liftDB $ DB.loadWallet database
  case wm of
    Nothing -> pure Nothing
    Just wm' -> do
      pubKey <- maybe (throwError $ WalletCryptoError "Invalid public key format") pure $ Sig.fromRawBytes (DB.wmPublicKey wm')
      privKey <- maybe (throwError $ WalletCryptoError "Invalid private key format") pure $ Sig.fromRawBytes (DB.wmPrivateKey wm')
      let address = Addr.Address (DB.wmAddress wm')
          name = WalletName $ DB.wmName wm'
          wallet = Wallet name privKey pubKey address
      pure . Just $ wallet

storeUTXO' :: (WalletM m) => DB.Database -> UTXO -> m ()
storeUTXO' database (UTXO txId idx (Addr.Address addr) value) = do
  let um =
        DB.UTXOModel
          { DB.umTxId = Hash.toHex txId
          , DB.umOutIdx = fromIntegral idx
          , DB.umAddress = addr
          , DB.umAmount = fromIntegral value
          , DB.umSpent = False
          }
  liftDB $ DB.storeUTXO database um

markUTXOAsSpent' :: (WalletM m) => DB.Database -> Hash.Hash -> Word64 -> m ()
markUTXOAsSpent' database txId outIdx = liftDB $ DB.markUTXOAsSpent database txId outIdx

getUTXOs' :: (WalletM m) => DB.Database -> m [UTXO]
getUTXOs' database = do
  utxos <- liftDB $ DB.getUTXOs database
  mapM toUTXO utxos
 where
  toUTXO um = do
    txId <-
      maybe (throwError $ WalletCryptoError "Invalid transaction ID") pure $
        Hash.fromHex (DB.umTxId um)
    let idx = fromIntegral (DB.umOutIdx um)
        addr = Addr.Address $ DB.umAddress um
        value = fromIntegral (DB.umAmount um)
    pure (UTXO txId idx addr value)

loadAllWallets :: (WalletM m) => FilePath -> m [Wallet]
loadAllWallets basePath = do
  walletFiles <- liftIO $ getDirectoryContents (basePath </> "wallets")
  let walletNames = filter (isPrefixOf "wallet_") walletFiles
  wallets <-
    mapM
      ( \name -> do
          let walletName = drop 7 name
          walletStorage <- mkWalletStorage basePath (WalletName $ T.pack walletName)
          load walletStorage
      )
      walletNames
  pure $ catMaybes wallets

sendTransaction :: (WalletM m) => Wallet -> WalletStorage -> Address -> Coin -> m ()
sendTransaction wallet walletStorage to' amount' = do
  -- Get UTXOs from the wallet storage
  utxos <- getUTXOs walletStorage

  -- TODO: Implement fee calculation
  let params = BuildUnsignedTxParams (wAddr wallet) to' amount' 10000

  -- Build the unsigned transaction
  result <- liftIO . runExceptT $ buildUnsignedTx utxos params
  case result of
    Left err -> throwError (WalletTransactionBuilderError err)
    Right unsignedTx -> do
      -- Sign the transaction with the wallet's private key
      let sigTxIns = map (\(UTXO txId idx _ _) -> SigTxIn (Outpoint txId idx) (wPrivKey wallet)) utxos
      -- Sign the transaction
      result' <- liftIO . runExceptT $ signTx unsignedTx (NE.fromList sigTxIns)
      case result' of
        Left err -> throwError . WalletCryptoError . T.pack $ show err
        Right _signedTx -> pure ()

liftDB :: (WalletM m) => ExceptT DB.DatabaseError IO a -> m a
liftDB action = do
  result <- liftIO $ runExceptT action
  case result of
    Left e -> throwError (WalletDatabaseError e)
    Right a -> pure a

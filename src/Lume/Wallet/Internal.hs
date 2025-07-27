{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lume.Wallet.Internal (
  WalletName,
  getWalletName,
  Wallet (..),
  WalletError (..),
  withWallet,
  storeWallet,
  loadWallet,
  storeUTXO,
  markUTXOAsSpent,
  getUTXOs,
  newWallet,
  checkWalletExists,
  loadAllWallets,
  sendTransaction,
  mkWalletName,
)
where

import Control.Exception (SomeException, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Reader
import Data.Either (partitionEithers)
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
import Lume.Wallet.Config (Config (cDataDir))
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
  | WalletCryptoError String
  | WalletAlreadyExistsError WalletName
  | WalletNotFoundError WalletName
  | WalletTransactionBuilderError TxBuilderError
  deriving (Eq)

instance Show WalletError where
  show (WalletDatabaseError dbErr) =
    "Wallet database error:\n→ " ++ show dbErr
  show (WalletCryptoError msg) =
    "Cryptographic failure in wallet operations:\n→ " ++ msg
  show (WalletAlreadyExistsError name) =
    "A wallet named \"" ++ T.unpack (getWalletName name) ++ "\" already exists."
  show (WalletNotFoundError name) =
    "Wallet not found: \"" ++ T.unpack (getWalletName name) ++ "\"."
  show (WalletTransactionBuilderError builderErr) =
    "Failed to construct transaction:\n→ " ++ show builderErr

data WalletContext = WalletContext
  { wcWalletName :: WalletName
  , wcDatabase :: DB.Database
  }

type WalletM m a = (MonadIO m) => ReaderT WalletContext (ExceptT WalletError m) a

mkWalletName :: String -> WalletName
mkWalletName name = WalletName (T.pack name)

mkWalletPath :: FilePath -> WalletName -> FilePath
mkWalletPath basePath walletName = basePath </> "wallets" </> "wallet_" <> T.unpack (getWalletName walletName)

mkContext :: (MonadIO m) => Config -> WalletName -> ExceptT WalletError m WalletContext
mkContext config walletName = do
  let walletPath = mkWalletPath (cDataDir config) walletName
  database <-
    withExceptT WalletDatabaseError $
      DB.openDatabase (walletPath </> "wallet.db")
  pure $ WalletContext walletName database

withWallet :: (MonadIO m) => Config -> WalletName -> WalletM m a -> m (Either WalletError a)
withWallet config name action = do
  runExceptT $ do
    ctx <- mkContext config name
    runReaderT action ctx

liftDB :: (MonadTrans t, Monad m) => ExceptT DB.DatabaseError m a -> t (ExceptT WalletError m) a
liftDB action = lift $ withExceptT WalletDatabaseError action

storeWallet :: Wallet -> WalletM m ()
storeWallet (Wallet walletName privKey pubKey (Addr.Address addr)) = do
  database <- asks wcDatabase
  liftDB $
    DB.storeWallet database $
      DB.WalletModel
        { DB.wmName = getWalletName walletName
        , DB.wmPublicKey = Sig.toRawBytes pubKey
        , DB.wmPrivateKey = Sig.toRawBytes privKey
        , DB.wmAddress = addr
        }

loadWallet :: WalletM m (Maybe Wallet)
loadWallet = do
  database <- asks wcDatabase
  walletModel <- liftDB $ DB.loadWallet database
  case walletModel of
    Nothing -> pure Nothing
    Just walletModel' -> do
      privKey <- parseSk (DB.wmPrivateKey walletModel')
      pubKey <- parsePk (DB.wmPublicKey walletModel')
      pure . Just $
        Wallet
          { wName = WalletName (DB.wmName walletModel')
          , wAddr = Addr.Address (DB.wmAddress walletModel')
          , wPrivKey = privKey
          , wPubKey = pubKey
          }
 where
  parsePk pk = maybe (throwError $ WalletCryptoError "Invalid public key format") pure $ Sig.fromRawBytes pk
  parseSk sk = maybe (throwError $ WalletCryptoError "Invalid private key format") pure $ Sig.fromRawBytes sk

storeUTXO :: UTXO -> WalletM m ()
storeUTXO (UTXO txId idx (Addr.Address addr) value) = do
  database <- asks wcDatabase
  lift $
    withExceptT WalletDatabaseError $
      DB.storeUTXO
        database
        DB.UTXOModel
          { DB.umTxId = Hash.toHex txId
          , DB.umOutIdx = fromIntegral idx
          , DB.umAddress = addr
          , DB.umAmount = fromIntegral value
          , DB.umSpent = False
          }

markUTXOAsSpent :: Hash.Hash -> Word64 -> WalletM m ()
markUTXOAsSpent txId outIdx = do
  database <- asks wcDatabase
  liftDB $ DB.markUTXOAsSpent database txId outIdx

getUTXOs :: WalletM m [UTXO]
getUTXOs = do
  database <- asks wcDatabase
  utxos <- liftDB $ DB.getUTXOs database
  mapM toUTXO utxos
 where
  toUTXO um = do
    txId <-
      maybe (throwError $ WalletCryptoError "Invalid transaction ID") pure $
        Hash.fromHex (DB.umTxId um)
    let idx = fromIntegral (DB.umOutIdx um)
        addr = Addr.Address (DB.umAddress um)
        value = fromIntegral (DB.umAmount um)
    pure (UTXO txId idx addr value)

sendTransaction :: Address -> Coin -> WalletM m ()
sendTransaction to' amount' = do
  wallet <-
    loadWallet >>= \case
      Nothing -> asks wcWalletName >>= throwError . WalletNotFoundError
      Just w -> pure w
  utxos <- getUTXOs
  -- TODO: Implement fee calculation
  let params = BuildUnsignedTxParams (wAddr wallet) to' amount' 10000
  let result = buildUnsignedTx utxos params
  case result of
    Left err -> throwError (WalletTransactionBuilderError err)
    Right unsignedTx -> do
      let sigTxIns = map (\(UTXO txId idx _ _) -> SigTxIn (Outpoint txId idx) (wPrivKey wallet)) utxos
      let signedTx = signTx unsignedTx (NE.fromList sigTxIns)
      case signedTx of
        Left err -> throwError . WalletCryptoError $ show err
        Right _signedTx -> pure ()

newWallet :: Config -> WalletName -> IO (Either WalletError Wallet)
newWallet config walletName = do
  exists <- checkWalletExists (cDataDir config) walletName
  if exists
    then pure $ Left (WalletAlreadyExistsError walletName)
    else do
      let walletPath = mkWalletPath (cDataDir config) walletName
      createDirectoryIfMissing True walletPath
      result <- try Sig.generateKeyPair
      case result of
        Left (e :: SomeException) -> pure . Left . WalletCryptoError $ "Failed to generate key pair: " <> show e
        Right (Sig.KeyPair (pk, sk)) ->
          case Addr.fromPublicKey pk of
            Left e -> pure . Left . WalletCryptoError $ "Failed to generate address from public key: " <> show e
            Right address' -> pure . Right $ Wallet walletName sk pk address'

checkWalletExists :: FilePath -> WalletName -> IO Bool
checkWalletExists basePath walletName = doesDirectoryExist (mkWalletPath basePath walletName)

loadAllWallets :: Config -> IO (Either [WalletError] [Wallet])
loadAllWallets config = do
  let path = cDataDir config </> "wallets"
  walletFiles <- getDirectoryContents path
  let walletNames =
        map (mkWalletName . drop 7)
          . filter (isPrefixOf "wallet_")
          $ walletFiles
  results <- forM walletNames $ \name -> withWallet config name loadWallet
  let (errors, mWallets) = partitionEithers results
  if null errors
    then pure $ Right (catMaybes mWallets)
    else pure $ Left errors

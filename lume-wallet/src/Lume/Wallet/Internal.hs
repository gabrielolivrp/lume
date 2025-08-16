{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
  spendUTXO,
  storeUTXO,
  getUTXOs,
  newWallet,
  checkWalletExists,
  loadAllWallets,
  scanFullUTXO,
  sendTransaction,
  mkWalletName,
)
where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Reader
import Data.Aeson (ToJSON (toJSON), encode)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Either (partitionEithers)
import Data.List (find, isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Lume.Core
import Lume.Core.Crypto.Address (Address)
import Lume.Core.Crypto.Address qualified as Addr
import Lume.Core.Crypto.Hash qualified as Hash
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Wallet.Config (Config (cDataDir), mkRPCUrl)
import Lume.Wallet.Network.Node (fetchGetBlockCount, handleRequest', mkGetBlockHashRequest, mkGetBlockRequest)
import Lume.Wallet.Storage.Database qualified as DB
import Lume.Wallet.Transaction (SigTxIn (SigTxIn), signTx)
import Lume.Wallet.Transaction.Builder (BuildUnsignedTxParams (BuildUnsignedTxParams), TxBuilderError, buildUnsignedTx)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

newtype WalletName = WalletName {getWalletName :: String}
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
  | WalletAlreadyExistsError String
  | WalletNotFoundError String
  | WalletTransactionBuilderError TxBuilderError
  | WalletRpcError String
  deriving (Eq)

instance Show WalletError where
  show (WalletDatabaseError dbErr) =
    "Wallet database error:\n→ " ++ show dbErr
  show (WalletCryptoError msg) =
    "Cryptographic failure in wallet operations:\n→ " ++ msg
  show (WalletAlreadyExistsError name) =
    "A wallet named \"" ++ name ++ "\" already exists."
  show (WalletNotFoundError name) =
    "Wallet not found: \"" ++ name ++ "\"."
  show (WalletTransactionBuilderError builderErr) =
    "Failed to construct transaction:\n→ " ++ show builderErr
  show (WalletRpcError msg) =
    "RPC error in wallet operations:\n→ " ++ msg

data WalletContext = WalletContext
  { wcWalletName :: WalletName
  , wcDatabase :: DB.Database
  }

type WalletM m a = (MonadIO m) => ReaderT WalletContext (ExceptT WalletError m) a

mkWalletName :: String -> WalletName
mkWalletName name = WalletName name

mkWalletPath :: FilePath -> WalletName -> FilePath
mkWalletPath basePath walletName = basePath </> "wallets" </> "wallet_" <> getWalletName walletName

mkContext :: (MonadIO m) => Config -> WalletName -> ExceptT WalletError m WalletContext
mkContext config walletName = do
  let path = mkWalletPath (cDataDir config) walletName
  database <-
    withExceptT WalletDatabaseError $
      DB.openDatabase (path </> "wallet.db")
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
  walletModel <- liftDB (DB.loadWallet database)
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
  parsePk pk = maybe (throwError $ WalletCryptoError "Invalid public key format") pure (Sig.fromRawBytes pk)
  parseSk sk = maybe (throwError $ WalletCryptoError "Invalid private key format") pure (Sig.fromRawBytes sk)

dropUTXOs :: WalletM m ()
dropUTXOs = do
  database <- asks wcDatabase
  liftDB (DB.dropUTXOs database)

spendUTXO :: UTXO -> WalletM m ()
spendUTXO (UTXO txId idx _ _) = do
  database <- asks wcDatabase
  lift $
    withExceptT WalletDatabaseError $
      DB.spendUTXO database txId idx

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

getUTXOs :: WalletM m [UTXO]
getUTXOs = do
  database <- asks wcDatabase
  utxos <- liftDB (DB.getUTXOs database)
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
      Nothing -> asks wcWalletName >>= throwError . WalletNotFoundError . getWalletName
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
      forM_ utxos spendUTXO
      case signedTx of
        Left err -> throwError . WalletCryptoError $ show err
        Right signedTx' -> liftIO $ LC8.putStrLn (encode $ toJSON signedTx')

newWallet :: Config -> WalletName -> IO (Either WalletError Wallet)
newWallet config walletName = do
  exists <- checkWalletExists (cDataDir config) walletName
  if exists
    then pure $ Left (WalletAlreadyExistsError . getWalletName $ walletName)
    else do
      let path = mkWalletPath (cDataDir config) walletName
      createDirectoryIfMissing True path
      keyPair <- generateWalletKeyPair
      case keyPair of
        Left err -> pure (Left err)
        Right keyPair' -> pure (mkWallet walletName keyPair')
 where
  mkWallet :: WalletName -> Sig.KeyPair -> Either WalletError Wallet
  mkWallet walletName' (Sig.KeyPair (pk, sk)) =
    case Addr.fromPublicKey pk of
      Left err -> Left $ WalletCryptoError $ "Failed to generate address: " <> show err
      Right addr -> Right (Wallet walletName' sk pk addr)

generateWalletKeyPair :: IO (Either WalletError Sig.KeyPair)
generateWalletKeyPair = do
  result <- try Sig.generateKeyPair
  case result of
    Left (e :: SomeException) -> pure . Left . WalletCryptoError $ "Failed to generate key pair: " <> show e
    Right keyPair -> pure (Right keyPair)

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
    else pure (Left errors)

scanFullUTXO :: Config -> IO [WalletError]
scanFullUTXO config = do
  walletsResult <- loadAllWallets config
  case walletsResult of
    Left errs -> pure errs
    Right wallets -> do
      clearAllWalletUTXOs config wallets
      bestBlockResult <- getBestBlockHeight rpcUrl
      case bestBlockResult of
        Left err -> pure [err]
        Right bestBlock -> do
          let batches = blockHeightBatches batchSize bestBlock
          concat <$> mapM (processBlockBatch config rpcUrl wallets) batches
 where
  rpcUrl :: String
  rpcUrl = mkRPCUrl config
  {-# INLINE rpcUrl #-}

  batchSize :: Int
  batchSize = 100
  {-# INLINE batchSize #-}

clearAllWalletUTXOs :: Config -> [Wallet] -> IO ()
clearAllWalletUTXOs config wallets = do
  forM_ wallets $ \wallet -> do
    let walletName = wName wallet
    withWallet config walletName dropUTXOs

processBlockBatch :: Config -> String -> [Wallet] -> [Int] -> IO [WalletError]
processBlockBatch config url wallets heights = do
  blocksResult <-
    fetchBlockHashes url heights
      >>= either (pure . Left) (fetchBlock url)
  case blocksResult of
    Left err -> pure [err]
    Right blocks -> processBlocks config wallets blocks

processBlocks :: Config -> [Wallet] -> [Block] -> IO [WalletError]
processBlocks config wallets blocks = do
  let addresses = S.fromList (map wAddr wallets)
      pks = S.fromList (map wPubKey wallets)
      txs = map (splitWalletTxs addresses pks) blocks
  concat
    <$> mapM
      ( \(spentTxs, receivedTxs) -> do
          spentErrs <- processTxs config wallets addresses spendUTXO spentTxs
          receivedErrs <- processTxs config wallets addresses storeUTXO receivedTxs
          pure (spentErrs ++ receivedErrs)
      )
      txs

processTxs :: (Monad m, MonadIO m) => Config -> [Wallet] -> S.Set Address -> (UTXO -> WalletM m a) -> [Tx] -> m [WalletError]
processTxs config' wallets addresses action txs = do
  concat <$> mapM handleTx txs
 where
  handleTx tx = do
    let utxos = findUTXO addresses tx
    results <- forM utxos $ \utxo ->
      case find (\w -> wAddr w == utxo ^. utxoOwner) wallets of
        Nothing -> pure $ Left (WalletNotFoundError $ "No wallet found for address: " ++ show (utxo ^. utxoOwner))
        Just w -> withWallet config' (wName w) (action utxo)
    let (errors, _) = partitionEithers results
    pure errors

findUTXO :: S.Set Address -> Tx -> [UTXO]
findUTXO addresses tx =
  let hash = txHash tx
      outs = filter (\out -> S.member (out ^. txOutAddress) addresses) (tx ^. txOut)
   in zipWith (mkUTXO hash) outs [0 ..]

splitWalletTxs :: S.Set Address -> S.Set Sig.PublicKey -> Block -> ([Tx], [Tx])
splitWalletTxs addresses pubKeys block =
  let txs = getTxs (block ^. bTxs)
      receivedTx = NE.filter (txToWalletAddress addresses) txs
      spentTx = NE.filter (txFromWalletKey pubKeys) txs
   in (spentTx, receivedTx)

txToWalletAddress :: S.Set Address -> Tx -> Bool
txToWalletAddress addresses tx =
  let outputs = S.fromList (tx ^.. txOut . traverse . txOutAddress)
   in not $ S.null (S.intersection addresses outputs)

txFromWalletKey :: S.Set Sig.PublicKey -> Tx -> Bool
txFromWalletKey keys tx =
  let inputs = S.fromList (tx ^.. txIn . traverse . txInPubKey)
   in not $ S.null (S.intersection keys inputs)

blockHeightBatches :: Int -> Int -> [[Int]]
blockHeightBatches size bestBlock =
  [ [start .. end]
  | start <- [0, size .. bestBlock]
  , let end = min (start + size - 1) bestBlock
  , start <= bestBlock
  ]

getBestBlockHeight :: String -> IO (Either WalletError Int)
getBestBlockHeight url = do
  result <- fetchGetBlockCount url
  pure $ case result of
    Left err -> Left (WalletRpcError err)
    Right height -> Right height

fetchBlock :: String -> [Hash.Hash] -> IO (Either WalletError [Block])
fetchBlock url hashes = do
  let requests = zipWith mkGetBlockRequest [0 ..] hashes
  getBlockResponse <- handleRequest' url requests
  pure $ case getBlockResponse of
    Left err -> Left (WalletRpcError $ concat err)
    Right blocks -> Right blocks

fetchBlockHashes :: String -> [Int] -> IO (Either WalletError [Hash.Hash])
fetchBlockHashes url heights = do
  let requests = zipWith mkGetBlockHashRequest [0 ..] heights
  getBlockHashResponse <- handleRequest' url requests
  pure $ case getBlockHashResponse of
    Left err -> Left (WalletRpcError $ concat err)
    Right hashs -> Right hashs

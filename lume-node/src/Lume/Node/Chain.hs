{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Lume.Node.Chain (
  ChainError (..),
  ChainContext (..),
  ChainM,
  runChainM,
  createBlockchain,
  validateBock,
  applyBlock,
  getBestBlock,
  getBlockByHash,
  getBlockByHeight,
)
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word64)
import Lume.Core
import Lume.Core.Crypto.Hash (Hash)
import Lume.Node.Config (Config, cDataDir)
import Lume.Node.Miner qualified as Miner
import Lume.Node.Storage.Database qualified as DB
import Lume.Node.Storage.FileStorage qualified as FS

data ChainError
  = ChainDatabaseError String
  | ChainValidationError String
  | ChainBlockNotFoundError String
  | ChainMiningError String
  | ChainAlreadyCreatedError

data ChainContext = ChainContext
  { chainConfig :: Config
  , chainBlockDB :: DB.BlockDatabase
  , chainChainStateDB :: DB.ChainStateDatabase
  }

type ChainM m a =
  (MonadIO m) =>
  ReaderT ChainContext (ExceptT ChainError (ResourceT m)) a

instance Show ChainError where
  show (ChainDatabaseError msg) =
    "A database error occurred while interacting with the blockchain:\n-> " ++ msg
  show (ChainValidationError msg) =
    "The blockchain contains invalid data or an operation failed validation:\n-> " ++ msg
  show (ChainBlockNotFoundError msg) =
    "Requested block could not be found in the chain:\n-> " ++ msg
  show (ChainMiningError msg) =
    "An error occurred during the mining process:\n-> " ++ msg
  show ChainAlreadyCreatedError =
    "The blockchain has already been initialized. No need to recreate it."

runChainM :: (MonadUnliftIO m) => Config -> ChainM m a -> m (Either ChainError a)
runChainM config action = DB.runDatabaseM $ runExceptT $ do
  blockDB <- DB.openBlockDB (cDataDir config)
  chainStateDB <- DB.openChainStateDB (cDataDir config)
  let ctx = ChainContext config blockDB chainStateDB
  runReaderT action ctx

createBlockchain :: ChainM m ()
createBlockchain = do
  chainExists <- isChainInitialized
  when chainExists (throwError ChainAlreadyCreatedError)
  applyBlock genesisBlock

applyBlock :: Block -> ChainM m ()
applyBlock block = do
  difficulty <- getNextDifficulty
  minedBlock <- mineBlock difficulty block
  storeBlock minedBlock

readBlock :: FS.FileIndex -> FS.BlockPosition -> ChainM m Block
readBlock fileIdx blockPos = do
  ChainContext{chainConfig} <- ask
  block <- liftIO $ FS.readBlock (cDataDir chainConfig) fileIdx blockPos
  case block of
    Left err -> throwError $ ChainBlockNotFoundError $ "Failed to read block from file: " ++ show err
    Right b -> pure b

getBlockByHash :: Hash -> ChainM m (Maybe Block)
getBlockByHash blockHash' = do
  ChainContext{chainBlockDB} <- ask
  result <- DB.getBlock chainBlockDB blockHash'
  case result of
    Right (Just blockModel) ->
      Just <$> readBlock (DB.bmFile blockModel) (fromIntegral $ DB.bmDataPos blockModel)
    Right Nothing -> pure Nothing
    Left err -> error $ "Failed to get block: " ++ show err

getBlockByHeight :: Word64 -> ChainM m (Maybe Block)
getBlockByHeight height = do
  ChainContext{chainBlockDB} <- ask
  result <- DB.getHeightToHash chainBlockDB height
  case result of
    Left err -> throwError $ ChainDatabaseError $ "Failed to get block difficulty: " ++ show err
    Right Nothing -> pure Nothing
    Right (Just bh) -> getBlockByHash (DB.hhHash bh)

getBestBlock :: ChainM m (Maybe Block)
getBestBlock = do
  ChainContext{chainBlockDB, chainChainStateDB} <- ask
  result <- DB.getBestBlock chainChainStateDB
  case result of
    Right (Just bestBlockHash) -> do
      blockResult <- DB.getBlock chainBlockDB bestBlockHash
      case blockResult of
        Left err -> throwError $ ChainDatabaseError $ "Failed to get block by hash: " ++ show err
        Right Nothing -> pure Nothing
        Right (Just block) -> Just <$> readBlock (DB.bmFile block) (fromIntegral $ DB.bmDataPos block)
    Right Nothing -> pure Nothing
    Left err -> throwError $ ChainDatabaseError $ "Failed to get best block: " ++ show err

validateBock :: Block -> ChainM m ()
validateBock block = do
  utxoSet <- getUtxoSet block
  bestBlock <- getBestBlock
  case bestBlock of
    Nothing -> throwError $ ChainValidationError "No best block found to validate against."
    Just prevBlock -> do
      case validateBlock utxoSet prevBlock block of
        Left err -> throwError $ ChainValidationError $ "Block validation failed: " ++ show err
        Right _ -> pure ()

mineBlock :: Bits -> Block -> ChainM m Block
mineBlock difficulty block = do
  let target = toTarget difficulty
  let result = Miner.mineBlock block target
  case result of
    Right minedBlock -> pure minedBlock
    Left err -> throwError $ ChainMiningError $ "Mining failed: " ++ show err

getNextDifficulty :: ChainM m Bits
getNextDifficulty = do
  bestBlock <- getBestBlock
  case bestBlock of
    Nothing -> pure initialBits
    Just bestBlock' -> do
      blockResult <- getBlockByHeight (bestBlock' ^. bHeader . bHeight - fromIntegral blocksPerRetarget)
      case blockResult of
        Nothing -> pure initialBits
        Just block -> do
          let actualTimestamp = (bestBlock' ^. bHeader . bTimestamp) - (block ^. bHeader . bTimestamp)
              actualBits = bestBlock' ^. bHeader . bBits
          pure $ adjustDifficulty actualBits actualTimestamp

getFileInfo :: Word32 -> ChainM m (Either DB.DatabaseError DB.FileInfoModel)
getFileInfo fileIndex = do
  ChainContext{chainBlockDB} <- ask
  result <- DB.getFileInfo chainBlockDB fileIndex
  case result of
    Right (Just info) -> pure (Right info)
    Right Nothing -> pure (Right DB.FileInfoModel{DB.fiBlockCount = 0, DB.fiFileSize = 0})
    Left err -> pure (Left err)

getLastBlockFile :: ChainM m (Either DB.DatabaseError Word32)
getLastBlockFile = do
  ChainContext{chainBlockDB} <- ask
  result <- DB.getLastBlockFile chainBlockDB
  case result of
    Right Nothing -> pure (Right 0)
    Right (Just fileIndex) -> pure (Right fileIndex)
    Left err -> pure (Left err)

storeBlock :: Block -> ChainM m ()
storeBlock block = do
  ChainContext{chainConfig, chainBlockDB, chainChainStateDB} <- ask

  let blockHash' = blockHash block
  fileIndex <-
    getLastBlockFile
      >>= \case
        Right idx -> pure idx
        Left err -> throwError $ ChainDatabaseError $ "Failed to get last block file: " ++ show err

  position <- liftIO $ FS.writeBlock (cDataDir chainConfig) fileIndex block

  let blockModel = DB.toBlockModel block DB.BlockStatusUnknown position fileIndex
  DB.putBlock chainBlockDB blockHash' blockModel

  DB.putBestBlock chainChainStateDB blockHash'

  DB.putHeightToHash
    chainBlockDB
    (block ^. bHeader . bHeight)
    (DB.HeightToHashModel blockHash')

  fileSize <- liftIO $ FS.getFileSize (cDataDir chainConfig) fileIndex
  when (fileSize >= FS.maxFileSize) $
    DB.putLastBlockFile chainBlockDB (fileIndex + 1)

  fileInfo <-
    getFileInfo fileIndex
      >>= \case
        Right idx -> pure idx
        Left err -> throwError $ ChainDatabaseError $ "Failed to get file info: " ++ show err

  let fileInfo' =
        fileInfo
          { DB.fiBlockCount = DB.fiBlockCount fileInfo + 1
          , DB.fiFileSize = fromIntegral fileSize
          }
  DB.putFileInfo chainBlockDB fileIndex fileInfo'

  forM_ (getTxs $ block ^. bTxs) $ storeTx (block ^. bHeader . bHeight)

storeTx :: Word64 -> Tx -> ChainM m ()
storeTx height tx = do
  ChainContext{chainChainStateDB} <- ask
  forM_ (zip [0 ..] (tx ^. txOut)) $ \(idx, out) ->
    DB.putUtxo chainChainStateDB (txHash tx) idx $
      DB.UTXOModel
        { DB.umIsCoinbase = isCoinbase tx
        , DB.umValue = out ^. txOutValue
        , DB.umIdx = idx
        , DB.umOwner = out ^. txOutAddress
        , DB.umHeight = height
        }

getUtxoSet :: Block -> ChainM m UtxoSet
getUtxoSet block = do
  ChainContext{chainChainStateDB} <- ask
  let inputs = concatMap (^. txIn) (getTxs $ block ^. bTxs)
  utxos <- forM inputs $ \txin -> do
    let prevOut = txin ^. txInPrevOut
    utxo <- DB.getUtxo chainChainStateDB (prevOut ^. outpId) (prevOut ^. outpIdx)
    case utxo of
      Right (Just utxoModel) ->
        pure $
          Just
            ( prevOut
            , UTXO
                { _utxoTxId = prevOut ^. outpId
                , _utxoIdx = DB.umIdx utxoModel
                , _utxoOwner = DB.umOwner utxoModel
                , _utxoValue = DB.umValue utxoModel
                }
            )
      Right Nothing -> pure Nothing
      Left err -> throwError $ ChainDatabaseError $ "Failed to get UTXO: " ++ show err

  pure . UtxoSet $ M.fromList (catMaybes utxos)

isChainInitialized :: ChainM m Bool
isChainInitialized = do
  ChainContext{chainChainStateDB} <- ask
  result <- DB.getBestBlock chainChainStateDB
  case result of
    Right (Just _) -> pure True
    Right Nothing -> pure False
    Left err -> throwError $ ChainDatabaseError $ "Failed to check if chain is initialized: " ++ show err

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Lume.Node.Chain where

import Control.Lens
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word64)
import Lume.Core
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

createBlockchain ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.BlockDatabase ->
  DB.ChainStateDatabase ->
  Config ->
  m ()
createBlockchain blockDB chainstateDB config = do
  chainExists <- chainIsCreated chainstateDB
  when chainExists $ throwError ChainAlreadyCreatedError
  applyBlock blockDB chainstateDB config genesisBlock

validateBock ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.BlockDatabase ->
  DB.ChainStateDatabase ->
  Config ->
  Block ->
  m ()
validateBock blockDB chainstateDB config block = do
  utxoSet <- getUtxoSet chainstateDB block
  prevBlockModel <- getBestBlock chainstateDB blockDB
  case prevBlockModel of
    Nothing -> throwError $ ChainBlockNotFoundError "No previous block found"
    Just prevBlockModel' -> do
      prevBlock <- liftIO $ FS.readBlock (cDataDir config) (DB.bmFile prevBlockModel') (fromIntegral $ DB.bmDataPos prevBlockModel')
      case prevBlock of
        Left err -> throwError $ ChainBlockNotFoundError $ "Failed to read previous block: " ++ show err
        Right prevBlock' -> do
          case validateBlock utxoSet prevBlock' block of
            Left err -> throwError $ ChainValidationError $ "Block validation failed: " ++ show err
            Right _ -> pure ()

applyBlock ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.BlockDatabase ->
  DB.ChainStateDatabase ->
  Config ->
  Block ->
  m ()
applyBlock blockDB chainstateDB config block = do
  difficulty <- getNextDifficulty chainstateDB blockDB
  minedBlock <- mineBlock difficulty block
  storeBlock config blockDB chainstateDB minedBlock

mineBlock :: (MonadError ChainError m) => Bits -> Block -> m Block
mineBlock difficulty block = do
  let target = toTarget difficulty
  let result = Miner.mineBlock block target
  case result of
    Right minedBlock -> pure minedBlock
    Left err -> throwError $ ChainMiningError $ "Mining failed: " ++ show err

getNextDifficulty ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.ChainStateDatabase ->
  DB.BlockDatabase ->
  m Bits
getNextDifficulty chainstateDB blockDB = do
  bestBlock <- getBestBlock chainstateDB blockDB
  case bestBlock of
    Nothing -> pure initialBits
    Just bestBlock' -> do
      retargetDifficulty <- getBlockDifficultyByHeight blockDB (DB.bmHeight bestBlock' - fromIntegral blocksPerRetarget)
      case retargetDifficulty of
        Nothing -> pure initialBits
        Just retargetDifficulty' -> do
          let actualTimestamp = DB.bmTimestamp bestBlock' - DB.biTimestamp retargetDifficulty'
              actualBits = DB.bmBits bestBlock'
          pure $ adjustDifficulty actualBits actualTimestamp

getBestBlock ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.ChainStateDatabase ->
  DB.BlockDatabase ->
  m (Maybe DB.BlockModel)
getBestBlock chainstateDB blockDB = do
  result <- DB.getBestBlock chainstateDB
  case result of
    Right (Just bestBlockHash) -> do
      blockResult <- DB.getBlock blockDB bestBlockHash
      case blockResult of
        Left err -> throwError $ ChainDatabaseError $ "Failed to get block by hash: " ++ show err
        Right Nothing -> pure Nothing
        Right (Just block) -> pure (Just block)
    Right Nothing -> pure Nothing
    Left err -> throwError $ ChainDatabaseError $ "Failed to get best block: " ++ show err

getBlockDifficultyByHeight ::
  (MonadError ChainError m, DB.DatabaseM m) =>
  DB.BlockDatabase ->
  Word64 ->
  m (Maybe DB.BlockDifficultyModel)
getBlockDifficultyByHeight blockDB height = do
  result <- DB.getBlockDifficulty blockDB height
  case result of
    Left err -> throwError $ ChainDatabaseError $ "Failed to get block difficulty: " ++ show err
    Right Nothing -> pure Nothing
    Right (Just difficultyModel) -> pure (Just difficultyModel)

getFileInfo ::
  (DB.DatabaseM m) =>
  DB.BlockDatabase ->
  Word32 ->
  m (Either DB.DatabaseError DB.FileInfoModel)
getFileInfo database fileIndex = do
  result <- DB.getFileInfo database fileIndex
  case result of
    Right (Just info) -> pure (Right info)
    Right Nothing -> pure $ Right DB.FileInfoModel{DB.fiBlockCount = 0, DB.fiFileSize = 0}
    Left err -> pure $ Left err

getLastBlockFile ::
  (DB.DatabaseM m) =>
  DB.BlockDatabase ->
  m (Either DB.DatabaseError Word32)
getLastBlockFile database = do
  result <- DB.getLastBlockFile database
  case result of
    Right Nothing -> pure (Right 0)
    Right (Just fileIndex) -> pure (Right fileIndex)
    Left err -> pure (Left err)

storeBlock ::
  (DB.DatabaseM m, MonadError ChainError m) =>
  Config ->
  DB.BlockDatabase ->
  DB.ChainStateDatabase ->
  Block ->
  m ()
storeBlock config blockDB chainstateDB block = do
  let blockHash' = blockHash block
  fileIndex <-
    getLastBlockFile blockDB
      >>= \case
        Right idx -> pure idx
        Left err -> throwError $ ChainDatabaseError $ "Failed to get last block file: " ++ show err

  position <- liftIO $ FS.writeBlock (cDataDir config) fileIndex block

  let blockModel = DB.toBlockModel block DB.BlockStatusUnknown position fileIndex
  DB.putBlock blockDB blockHash' blockModel

  DB.putBestBlock chainstateDB blockHash'

  DB.putBlockDifficulty
    blockDB
    (block ^. bHeight)
    (DB.BlockDifficultyModel blockHash' (block ^. bHeader . bBits) (block ^. bHeader . bTimestamp))

  fileSize <- liftIO $ FS.getFileSize (cDataDir config) fileIndex
  when (fileSize >= FS.maxFileSize) $
    DB.putLastBlockFile blockDB (fileIndex + 1)

  fileInfo <-
    getFileInfo blockDB fileIndex
      >>= \case
        Right idx -> pure idx
        Left err -> throwError $ ChainDatabaseError $ "Failed to get file info: " ++ show err

  let fileInfo' =
        fileInfo
          { DB.fiBlockCount = DB.fiBlockCount fileInfo + 1
          , DB.fiFileSize = fromIntegral fileSize
          }
  DB.putFileInfo blockDB fileIndex fileInfo'

  forM_ (getTxs $ block ^. bTxs) $ storeTx chainstateDB (block ^. bHeight)

storeTx :: (DB.DatabaseM m) => DB.ChainStateDatabase -> Word64 -> Tx -> m ()
storeTx chainstateDB height tx =
  forM_ (zip [0 ..] (tx ^. txOut)) $ \(idx, out) ->
    DB.putUtxo chainstateDB (txHash tx) idx $
      DB.UTXOModel
        { DB.umIsCoinbase = isCoinbase tx
        , DB.umValue = out ^. txOutValue
        , DB.umIdx = idx
        , DB.umOwner = out ^. txOutAddress
        , DB.umHeight = height
        }

getUtxoSet :: (DB.DatabaseM m, MonadError ChainError m) => DB.ChainStateDatabase -> Block -> m UtxoSet
getUtxoSet chainstateDB block = do
  let inputs = concatMap (^. txIn) (getTxs $ block ^. bTxs)
  utxos <- forM inputs $ \txin -> do
    let prevOut = txin ^. txInPrevOut
    utxo <- DB.getUtxo chainstateDB (prevOut ^. outpId) (prevOut ^. outpIdx)
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

chainIsCreated :: (DB.DatabaseM m, MonadError ChainError m) => DB.ChainStateDatabase -> m Bool
chainIsCreated chainstateDB = do
  result <- DB.getBestBlock chainstateDB
  case result of
    Right (Just _) -> pure True
    Right Nothing -> pure False
    Left err -> throwError $ ChainDatabaseError $ "Failed to check if chain is created: " ++ show err

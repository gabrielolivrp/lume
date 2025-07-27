{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.DatabaseTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Control.Monad.Except (MonadIO (liftIO))
import Lume.Core.Block (blockHash, genesisBlock, initialBits)
import Lume.Core.Time.Timestamp (Timestamp (..))
import Lume.Core.Transaction (Coin (..))
import Lume.Mocks (mockAddress1)
import Lume.Node.Storage
import System.IO.Temp (withSystemTempDirectory)

nodeStorageDatabaseTests :: TestTree
nodeStorageDatabaseTests =
  testGroup
    "Storage.Database"
    [ testCase "should store and get BlockModel" $
        withSystemTempDirectory "database-test" $ \dir ->
          runDatabaseM $ do
            let blockHash' = blockHash genesisBlock
                blockModel =
                  BlockModel
                    { bmVersion = 1
                    , bmHeight = 42
                    , bmStatus = BlockStatusValid
                    , bmTxCount = 3
                    , bmFile = 0
                    , bmDataPos = 100
                    , bmMerkleRoot = blockHash'
                    , bmTimestamp = Timestamp 999
                    , bmBits = initialBits
                    , bmNonce = 7
                    }
            db <- openBlockDB dir
            putBlock db blockHash' blockModel
            result <- getBlock db blockHash'
            case result of
              Left err ->
                liftIO $ assertFailure $ "getBlock failed unexpectedly: " <> show err
              Right (Just retrievedModel) ->
                liftIO $ assertEqual "BlockModel" blockModel retrievedModel
              Right Nothing -> liftIO $ assertFailure "Expected to find BlockModel, but got Nothing"
    , testCase "should store and get FileInfoModel" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runDatabaseM $ do
            let idx = 123
                fileInfoModel = FileInfoModel{fiBlockCount = 10, fiFileSize = 2048}
            db <- openBlockDB dir
            putFileInfo db idx fileInfoModel
            result <- getFileInfo db idx
            case result of
              Left err ->
                liftIO $ assertFailure $ "getFileInfo failed unexpectedly: " <> show err
              Right Nothing -> liftIO $ assertFailure "Expected to find FileInfoModel, but got Nothing"
              Right (Just retrievedModel) ->
                liftIO $ assertEqual "FileInfoModel" fileInfoModel retrievedModel
    , testCase "should store and get last block file index" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runDatabaseM $ do
            db <- openBlockDB dir
            putLastBlockFile db 123
            result <- getLastBlockFile db
            case result of
              Left err ->
                liftIO $ assertFailure $ "getLastBlockFile failed unexpectedly: " <> show err
              Right Nothing -> liftIO $ assertFailure "Expected to find last block file index, but got Nothing"
              Right (Just retrievedIndex) ->
                liftIO $ assertEqual "last block file index" 123 retrievedIndex
    , testCase "should store, get and delete UTXOModel" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runDatabaseM $ do
            let txHash = blockHash genesisBlock
                utxo =
                  UTXOModel
                    { umIsCoinbase = False
                    , umValue = Coin 5000
                    , umIdx = 1
                    , umOwner = mockAddress1
                    , umHeight = 7
                    }
            db <- openChainStateDB dir
            putUtxo db txHash 1 utxo
            getResult <- getUtxo db txHash 1
            case getResult of
              Left err ->
                liftIO $ assertFailure $ "getUtxo failed unexpectedly: " <> show err
              Right (Just retrievedUtxo) ->
                liftIO $ assertEqual "UTXOModel" utxo retrievedUtxo
              Right Nothing -> liftIO $ assertFailure "Expected to find UTXOModel, but got Nothing"
            deleteUtxo db txHash 1
            deleteResult <- getUtxo db txHash 1
            case deleteResult of
              Left err ->
                liftIO $ assertFailure $ "getUtxo after delete failed unexpectedly: " <> show err
              Right Nothing -> pure ()
              Right (Just _) -> liftIO $ assertFailure "Expected to not find UTXOModel after deletion"
    ]

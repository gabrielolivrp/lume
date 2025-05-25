{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lume.Storage.DatabaseTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Control.Monad.Except (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either (isLeft)
import System.IO.Temp (withSystemTempDirectory)

import Lume.Block.Builder (hashBlock)
import Lume.Consensus.Difficulty (initialBits)
import Lume.Mocks (mockAddress1, mockGenesisBlock)
import Lume.Storage.Database
import Lume.Time.Timestamp (Timestamp (..))
import Lume.Transaction.Amount (Amount (..))

databaseTests :: TestTree
databaseTests =
  testGroup
    "Storage.Database"
    [ testCase "should store and get BlockRecord" $
        withSystemTempDirectory "database-test" $ \dir ->
          runResourceT $ do
            let config = defaultDatabaseConfig{_dbBasePath = dir}
                blockHash = hashBlock mockGenesisBlock
                blockRecord =
                  BlockRecord
                    { _brVersion = 1
                    , _brHeight = 42
                    , _brStatus = BlockStatusValid
                    , _brTxCount = 3
                    , _brFile = 0
                    , _brDataPos = 100
                    , _brMerkleRoot = blockHash
                    , _brTimestamp = Timestamp 999
                    , _brBits = initialBits
                    , _brNonce = 7
                    }
            db <- openDatabase @'BlockContext config "blocks"
            putBlock db blockHash blockRecord
            result <- getBlock db blockHash
            case result of
              Left err ->
                liftIO $ assertFailure $ "getBlock failed unexpectedly: " <> show err
              Right retrievedRecord ->
                liftIO $ assertEqual "BlockRecord" blockRecord retrievedRecord
    , testCase "should store and get FileInfoRecord" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runResourceT $ do
            let config = defaultDatabaseConfig{_dbBasePath = dir}
                idx = 123
                fileInfoRecord = FileInfoRecord{_fiBlockCount = 10, _fiFileSize = 2048}
            db <- openDatabase @'BlockContext config "blocks"
            putFileInfo db idx fileInfoRecord
            result <- getFileInfo db idx
            case result of
              Left err ->
                liftIO $ assertFailure $ "getFileInfo failed unexpectedly: " <> show err
              Right retrievedRecord ->
                liftIO $ assertEqual "FileInfoRecord" fileInfoRecord retrievedRecord
    , testCase "should store and get last block file index" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runResourceT $ do
            let config = defaultDatabaseConfig{_dbBasePath = dir}
            db <- openDatabase @'BlockContext config "blocks"
            putLastBlockFile db 123
            result <- getLastBlockFile db
            case result of
              Left err ->
                liftIO $ assertFailure $ "getLastBlockFile failed unexpectedly: " <> show err
              Right retrievedIndex ->
                liftIO $ assertEqual "last block file index" 123 retrievedIndex
    , testCase "should store, get and delete UTXORecord" $
        withSystemTempDirectory "dbtest" $ \dir ->
          runResourceT $ do
            let config = defaultDatabaseConfig{_dbBasePath = dir}
                txHash = hashBlock mockGenesisBlock
                utxo =
                  UTXORecord
                    { _urIsCoinbase = False
                    , _urValue = Amount 5000
                    , _uIdx = 1
                    , _urOwner = mockAddress1
                    , _urHeight = 7
                    }
            db <- openDatabase @'ChainStateContext config "chainstate"
            putUtxo db txHash 1 utxo
            getResult <- getUtxo db txHash 1
            case getResult of
              Left err ->
                liftIO $ assertFailure $ "getUtxo failed unexpectedly: " <> show err
              Right retrievedUtxo ->
                liftIO $ assertEqual "UTXORecord" utxo retrievedUtxo
            deleteUtxo db txHash 1
            deleteResult <- getUtxo db txHash 1
            liftIO $ assertBool "UTXO should not exist after deletion" (isLeft deleteResult)
    ]

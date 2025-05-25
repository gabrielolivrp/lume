{-# LANGUAGE OverloadedStrings #-}

module Lume.Storage.FileStorageTest where

import Lume.Mocks (mockBlock1, mockGenesisBlock)
import Lume.Storage.FileStorage
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

fileStorageTests :: TestTree
fileStorageTests =
  testGroup
    "Storage.FileStorage"
    [ testCase "should create storage file path with correct format" $ do
        let basePath = "/tmp/blocks"
            fileIdx = 42
            expected = "/tmp/blocks/blk_00042.dat"
        mkStorageFilePath basePath fileIdx @?= expected
    , testCase "should write block to new file and return position" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "test_block.dat"
          position <- writeBlock filePath mockGenesisBlock
          assertEqual "write position should be 0 for new file" 0 position
          exists <- doesFileExist filePath
          assertBool "file should exist after write" exists
    , testCase "should write multiple blocks and return correct positions" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "multi_blocks.dat"
          pos1 <- writeBlock filePath mockGenesisBlock
          pos2 <- writeBlock filePath mockBlock1
          assertEqual "first block position" 0 pos1
          assertBool "second block position should be greater than first" (pos2 > pos1)
    , testCase "should read block successfully after writing" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "read_test.dat"
          pos <- writeBlock filePath mockGenesisBlock
          result <- readBlock filePath pos
          case result of
            Left err ->
              assertFailure $ "readBlock failed unexpectedly: " <> show err
            Right retrievedBlock ->
              assertEqual "retrieved block" mockGenesisBlock retrievedBlock
    , testCase "should read correct block when multiple blocks exist" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "multi_read.dat"
          pos1 <- writeBlock filePath mockGenesisBlock
          pos2 <- writeBlock filePath mockBlock1
          result1 <- readBlock filePath pos1
          result2 <- readBlock filePath pos2
          case (result1, result2) of
            (Right block1, Right block2) -> do
              assertEqual "first block" mockGenesisBlock block1
              assertEqual "second block" mockBlock1 block2
            (Left err, _) ->
              assertFailure $ "reading first block failed: " <> show err
            (_, Left err) ->
              assertFailure $ "reading second block failed: " <> show err
    , testCase "should fail to read block with invalid position" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "invalid_pos.dat"
          _ <- writeBlock filePath mockGenesisBlock
          result <- readBlock filePath 999999
          case result of
            Left InvalidPositionError -> pure ()
            Left err -> assertFailure $ "unexpected error type: " <> show err
            Right _ -> assertFailure "readBlock should have failed with invalid position"
    , testCase "should return zero size for non-existent file" $ do
        let nonExistentPath = "/tmp/non_existent_file.dat"
        size <- getFileSize nonExistentPath
        assertEqual "non-existent file size" 0 size
    , testCase "should return correct size for existing file" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "size_test.dat"
          _ <- writeBlock filePath mockGenesisBlock
          size <- getFileSize filePath
          assertBool "file size should be greater than 0" (size > 0)
    , testCase "should increase file size when writing multiple blocks" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          let filePath = tmpDir </> "growing_file.dat"
          _ <- writeBlock filePath mockGenesisBlock
          size1 <- getFileSize filePath
          _ <- writeBlock filePath mockBlock1
          size2 <- getFileSize filePath
          assertBool "file size should increase after second write" (size2 > size1)
    ]

{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.FileStorageTest where

import Lume.Core.Block (genesisBlock)
import Lume.Mocks (mockBlock1)
import Lume.Node.Storage
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

nodeStorageFileStorageTests :: TestTree
nodeStorageFileStorageTests =
  testGroup
    "Storage.FileStorage"
    [ testCase "should write block to new file and return position" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          position <- writeBlock tmpDir 0 genesisBlock
          assertEqual "write position should be 0 for new file" 0 position
          let filePath = mkDirectoryPath tmpDir </> mkFileName 0
          exists <- doesFileExist filePath
          assertBool "file should exist after write" exists
    , testCase "should write multiple blocks and return correct positions" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          pos1 <- writeBlock tmpDir 0 genesisBlock
          pos2 <- writeBlock tmpDir 0 mockBlock1
          assertEqual "first block position" 0 pos1
          assertBool "second block position should be greater than first" (pos2 > pos1)
    , testCase "should read block successfully after writing" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          pos <- writeBlock tmpDir 0 genesisBlock
          result <- readBlock tmpDir 0 pos
          case result of
            Left err ->
              assertFailure $ "readBlock failed unexpectedly: " <> show err
            Right retrievedBlock ->
              assertEqual "retrieved block" genesisBlock retrievedBlock
    , testCase "should read correct block when multiple blocks exist" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          pos1 <- writeBlock tmpDir 0 genesisBlock
          pos2 <- writeBlock tmpDir 1 mockBlock1
          result1 <- readBlock tmpDir 0 pos1
          result2 <- readBlock tmpDir 1 pos2
          case (result1, result2) of
            (Right block1, Right block2) -> do
              assertEqual "first block" genesisBlock block1
              assertEqual "second block" mockBlock1 block2
            (Left err, _) ->
              assertFailure $ "reading first block failed: " <> show err
            (_, Left err) ->
              assertFailure $ "reading second block failed: " <> show err
    , testCase "should fail to read block with invalid position" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          _ <- writeBlock tmpDir 0 genesisBlock
          result <- readBlock tmpDir 0 999999
          case result of
            Left FileStorageInvalidPositionError -> pure ()
            Left err -> assertFailure $ "unexpected error type: " <> show err
            Right _ -> assertFailure "readBlock should have failed with invalid position"
    , testCase "should return zero size for non-existent file" $ do
        let nonExistentPath = "/non/existent/path"
        size <- getFileSize nonExistentPath 0
        assertEqual "non-existent file size" 0 size
    , testCase "should return correct size for existing file" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          _ <- writeBlock tmpDir 0 genesisBlock
          size <- getFileSize tmpDir 0
          assertBool "file size should be greater than 0" (size > 0)
    , testCase "should increase file size when writing multiple blocks" $
        withSystemTempDirectory "filestorage-test" $ \tmpDir -> do
          _ <- writeBlock tmpDir 0 genesisBlock
          size1 <- getFileSize tmpDir 0
          _ <- writeBlock tmpDir 0 mockBlock1
          size2 <- getFileSize tmpDir 0
          assertBool "file size should increase after second write" (size2 > size1)
    ]

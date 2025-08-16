{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Storage.FileStorageSpec where

import Data.List.NonEmpty qualified as NE
import Lume.Core
import Lume.Core.Crypto.Address (Address (..))
import Lume.Core.Crypto.Hash (hash')
import Lume.Core.Crypto.Signature qualified as Sig
import Lume.Node
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

mockBlock1 :: Block
mockBlock1 =
  Block
    { _bHeader =
        BlockHeader
          { _bVersion = 1
          , _bNonce = 0
          , _bMerkleRoot = hash' "hash_block"
          , _bTimestamp = Timestamp 1234567890
          , _bBits = initialBits
          , _bHashPrevBlock = blockHash genesisBlock
          , _bHeight = 1
          }
    , _bTxs = Txs $ NE.fromList [tx1, tx2]
    }
 where
  tx1 :: Tx
  tx1 = Tx [] [txOut1] 1

  tx2 :: Tx
  tx2 = Tx [txIn1] [txOut2] 1

  txIn1 :: TxIn
  txIn1 =
    TxIn
      { _txInPrevOut = Outpoint (hash' "a") 0
      , _txInSignature = Sig.emptySignature
      , _txInPubKey = Sig.emptyKey
      }
  txOut1 :: TxOut
  txOut1 = TxOut (Address "lume_addr_1") 1000

  txOut2 :: TxOut
  txOut2 = TxOut (Address "lume_addr_2") 2000

fileStorageSpec :: TestTree
fileStorageSpec =
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

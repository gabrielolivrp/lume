{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Crypto.HashTest where

import Data.Binary
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Lume.Crypto.Hash qualified as Hash
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

data TestData = TestData String Int
  deriving (Show, Eq, Generic)

instance Binary TestData
instance Hash.ToHash TestData

hashTests :: TestTree
hashTests =
  testGroup
    "Hash Tests"
    [ testCase "Hash.hash' produces consistent values for the same input" $ do
        let input = "foo" :: BS.ByteString
            hash1 = Hash.hash' input
            hash2 = Hash.hash' input
        assertEqual "Hashes should be equal for the same input" hash1 hash2
    , testCase "Different inputs produce different hashes" $ do
        let input1 = "foo 1" :: BS.ByteString
            input2 = "foo 2" :: BS.ByteString
            hash1 = Hash.hash' input1
            hash2 = Hash.hash' input2
        assertBool "Hashes should be different for different inputs" (hash1 /= hash2)
    , testCase "toHex produces a valid hexadecimal representation" $ do
        let input = "foo" :: BS.ByteString
            hash = Hash.hash' input
            hexString = Hash.toHex hash
        assertEqual "Hex string should be 64 characters (32 bytes)" 64 (BS.length hexString)
    , testCase "toRawBytes and fromRawBytes perform correct round-trip conversion" $ do
        let input = "foo" :: BS.ByteString
            hash = Hash.hash' input
            rawBytes = Hash.toRawBytes hash
            hashAgain = Hash.fromRawBytes rawBytes
        case hashAgain of
          Just h -> assertEqual "Hash should be the same after roundtrip conversion" hash h
          Nothing -> assertFailure "fromRawBytes failed to parse the raw bytes"
    , testCase "fromRawBytes returns Nothing for invalid bytes" $ do
        let invalidBytes = BS.pack [1, 2, 3] -- Too short to be a valid SHA256 hash
            result = Hash.fromRawBytes invalidBytes
        case result of
          Just h -> assertFailure $ "Expected failure for invalid bytes, but got: " ++ show h
          Nothing -> pure ()
    , testCase "Default toHash implementation works for types with Binary instance" $ do
        let testData1 = TestData "foo" 1
            testData2 = TestData "foo" 1
            testData3 = TestData "bar" 2
            hash1 = Hash.toHash testData1
            hash2 = Hash.toHash testData2
            hash3 = Hash.toHash testData3
        assertEqual "Hash should be the same for equal data" hash1 hash2
        assertBool "Hash should be different for different data" (hash1 /= hash3)
    , testCase "ToHash instance for Hash returns the hash itself" $ do
        let input = "reflexive test" :: BS.ByteString
            hash = Hash.hash' input
            hashOfHash = Hash.toHash hash
        assertEqual "toHash of a Hash should return the same Hash" hash hashOfHash
    ]

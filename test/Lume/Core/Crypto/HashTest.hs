{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Crypto.HashTest where

import Data.Binary
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Lume.Core.Crypto.Hash qualified as Hash
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

newtype Foo = Foo String
  deriving (Show, Eq, Generic)

instance Binary Foo
instance Hash.ToHash Foo

coreHashTests :: TestTree
coreHashTests =
  testGroup
    "Crypto.Hash"
    [ testCase "should produce consistent hashes for same input" $ do
        let input = "foo"
            hash1 = Hash.hash' input
            hash2 = Hash.hash' input
        assertEqual "identical input hash" hash1 hash2
    , testCase "should produce different hashes for different inputs" $ do
        let input1 = "foo 1"
            input2 = "foo 2"
            hash1 = Hash.hash' input1
            hash2 = Hash.hash' input2
        assertBool "different inputs should produce different hashes" $
          hash1 /= hash2
    , testCase "should produce valid hexadecimal representation" $ do
        let input = "foo"
            hash = Hash.hash' input
            hexString = Hash.toHex hash
        assertEqual "hex string length" 64 (BS.length hexString)
    , testCase "should preserve hash through raw bytes round-trip" $ do
        let input = "foo"
            originalHash = Hash.hash' input
            rawBytes = Hash.toRawBytes originalHash
            reconstructedHash = Hash.fromRawBytes rawBytes
        case reconstructedHash of
          Just h ->
            assertEqual "round-trip hash" originalHash h
          Nothing ->
            assertFailure "fromRawBytes failed to parse valid raw bytes"
    , testCase "should return Nothing for invalid raw bytes" $ do
        let invalidBytes = BS.pack [1, 2, 3]
            result = Hash.fromRawBytes invalidBytes
        case result of
          Just h ->
            assertFailure $ "fromRawBytes should have failed for invalid bytes, got: " <> show h
          Nothing -> pure ()
    , testCase "should work with default Binary-based toHash implementation" $ do
        let foo1 = Foo "a"
            foo2 = Foo "a"
            foo3 = Foo "c"
            hash1 = Hash.toHash foo1
            hash2 = Hash.toHash foo2
            hash3 = Hash.toHash foo3
        assertEqual "equal data hash" hash1 hash2
        assertBool "different data should produce different hashes" $
          hash1 /= hash3
    ]

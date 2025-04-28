{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Crypto.MerkleTreeTest where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Lume.Crypto.Hash (toHash)
import Lume.Crypto.MerkleTree qualified as MT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

merkleTreeTests :: TestTree
merkleTreeTests =
  testGroup
    "MerkleTree Tests"
    [ testCase "single leaf root is hash of element" $ do
        let xs = ["hello" :: BS.ByteString]
            tree = MT.fromList xs
            root = MT.getMerkleHash tree
            expected = toHash ("hello" :: BS.ByteString)
        assertEqual "root should equal hash of the single leaf" expected root
    , testCase "fromListWithRoot returns same root as getMerkleHash" $ do
        let xs = ["a", "b", "c", "d"] :: [BS.ByteString]
            (tree, root) = MT.fromListWithRoot xs
        assertEqual "fromListWithRoot root should match getMerkleHash" root (MT.getMerkleHash tree)
    , testCase "proof verification succeeds for each leaf" $ do
        let xs = ["foo", "bar", "baz"] :: [BS.ByteString]
            (tree, root) = MT.fromListWithRoot xs
        forM_ xs $ \x ->
          let h = toHash x
              proof = MT.generateProof tree h
           in assertBool ("proof should verify for element " ++ show x ++ " - ") (MT.verifyProof proof root h)
    , testCase "proof verification fails for non-member" $ do
        let xs = ["1", "2", "3"] :: [BS.ByteString]
            (tree, root) = MT.fromListWithRoot xs
            fake = toHash ("notpresent" :: BS.ByteString)
            proof = MT.generateProof tree fake
        assertBool "verify should fail for non-member leaf" (not $ MT.verifyProof proof root fake)
    ]

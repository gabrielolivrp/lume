{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Crypto.MerkleTreeTest where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Lume.Core.Crypto.Hash (toHash)
import Lume.Core.Crypto.MerkleTree qualified as MT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

coreMerkleTreeTests :: TestTree
coreMerkleTreeTests =
  testGroup
    "Crypto.MerkleTree"
    [ testCase "should use leaf hash as root for single element" $ do
        let xs = ["hello" :: BS.ByteString]
            tree = MT.fromList xs
            actualRoot = MT.getMerkleHash tree
            expectedRoot = toHash ("hello" :: BS.ByteString)
        assertEqual "single element root" expectedRoot actualRoot
    , testCase "should return consistent root from fromListWithRoot and getMerkleHash" $ do
        let xs = ["a", "b", "c", "d"] :: [BS.ByteString]
            (tree, rootFromWith) = MT.fromListWithRoot xs
            rootFromGet = MT.getMerkleHash tree
        assertEqual "consistent merkle root" rootFromWith rootFromGet
    , testCase "should verify proofs for all tree members" $ do
        let xs = ["foo", "bar", "baz"] :: [BS.ByteString]
            (tree, root) = MT.fromListWithRoot xs
        forM_ xs $ \x -> do
          let leafHash = toHash x
              proof = MT.generateProof tree leafHash
          assertBool ("proof should verify for member: " <> show x) $
            MT.verifyProof proof root leafHash
    , testCase "should reject proofs for non-members" $ do
        let xs = ["1", "2", "3"] :: [BS.ByteString]
            (tree, root) = MT.fromListWithRoot xs
            nonMemberHash = toHash ("notpresent" :: BS.ByteString)
            proof = MT.generateProof tree nonMemberHash
        assertBool "proof should fail for non-member" $
          not $
            MT.verifyProof proof root nonMemberHash
    ]

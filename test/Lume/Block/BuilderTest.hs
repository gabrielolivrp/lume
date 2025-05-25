{-# LANGUAGE OverloadedStrings #-}

module Lume.Block.BuilderTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Control.Lens
import Lume.Block.Builder (BlockBuilderError (..), buildBlock)
import Lume.Block.Types (Block (..), bHeight, bTxs)
import Lume.Mocks
import Lume.Time.Timestamp (Timestamp (..))
import Lume.Transaction.Types (Txs (..))

blockBuilderTests :: TestTree
blockBuilderTests =
  testGroup
    "Block.Builder"
    [ testCase "should build new block with incremented height" $
        let timestamp = Timestamp 123456
            txs = Txs [mockTx2]
            result = buildBlock mockGenesisBlock timestamp txs
         in case result of
              Left err ->
                assertFailure $ "buildBlock failed unexpectedly: " <> show err
              Right newBlock -> do
                assertEqual "new block height" 1 (newBlock ^. bHeight)
                assertEqual "new block transactions" txs (newBlock ^. bTxs)
    , testCase "should fail when block height would overflow Word32" $
        let maxHeightBlock = mockBlock1{_bHeight = 0xFFFFFFFF}
            timestamp = Timestamp 123456
            txs = Txs [mockTx1]
            result = buildBlock maxHeightBlock timestamp txs
         in case result of
              Left InvalidHeight -> pure ()
              Left err -> assertFailure $ "buildBlock should have failed: " <> show err
              Right _ -> assertFailure "buildBlock should have failed due to height overflow"
    ]

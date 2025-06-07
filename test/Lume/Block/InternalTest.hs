{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Block.InternalTest where

import Control.Lens
import Data.List.NonEmpty qualified as NE
import Lume.Block (bHeight, bTxs, buildBlock)
import Lume.Block.Genesis (genesisBlock)
import Lume.Mocks
import Lume.Time.Timestamp (Timestamp (..))
import Lume.Transaction (Txs (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

blockInternalTests :: TestTree
blockInternalTests =
  testGroup
    "Block.Internal"
    [ testCase "should build new block with incremented height" $
        let timestamp = Timestamp 123456
            txs = Txs $ NE.singleton mockTx1
            result = buildBlock genesisBlock timestamp txs
         in case result of
              Left err ->
                assertFailure $ "buildBlock failed unexpectedly: " <> show err
              Right newBlock -> do
                assertEqual "new block height" 1 (newBlock ^. bHeight)
                assertEqual "new block transactions" txs (newBlock ^. bTxs)
    ]

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Block.InternalTest where

import Control.Lens
import Data.List.NonEmpty qualified as NE
import Lume.Core.Block (bHeight, bTxs, buildBlock, genesisBlock, initialBits)
import Lume.Core.Time.Timestamp (Timestamp (..))
import Lume.Core.Transaction (Txs (..))
import Lume.Mocks
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

nodeBlockInternalTests :: TestTree
nodeBlockInternalTests =
  testGroup
    "Block.Internal"
    [ testCase "should build new block with incremented height" $ do
        let timestamp = Timestamp 123456
            txs = Txs $ NE.singleton mockTx1
            newBlock = buildBlock genesisBlock initialBits timestamp txs
        assertEqual "new block height" 1 (newBlock ^. bHeight)
        assertEqual "new block transactions" txs (newBlock ^. bTxs)
    ]

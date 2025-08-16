{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Block.InternalSpec where

import Control.Lens
import Data.List.NonEmpty qualified as NE
import Lume.Core
import Lume.Core.Crypto.Address (Address (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tx :: Tx
tx =
  Tx
    []
    [ TxOut (Address "lume_addr_1") 1000
    ]
    1

blockInternalSpec :: TestTree
blockInternalSpec =
  testGroup
    "Block.Internal"
    [ testCase "should build new block with incremented height" $ do
        let timestamp = Timestamp 123456
            txs = Txs (NE.singleton tx)
            newBlock = buildBlock genesisBlock initialBits timestamp txs
        assertEqual "new block height" 1 (newBlock ^. bHeader . bHeight)
        assertEqual "new block transactions" txs (newBlock ^. bTxs)
    ]

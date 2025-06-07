{-# LANGUAGE OverloadedStrings #-}

module Lume.Consensus.PoWTest where

import Control.Lens
import Lume.Block
import Lume.Block.Genesis (genesisBlock)
import Lume.Consensus.Difficulty (Target (Target))
import Lume.Consensus.PoW
import Lume.Crypto.Hash (hash2Integer, toHash)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

powTests :: TestTree
powTests =
  testGroup
    "Consensus.PoW"
    [ testCase "should return True for valid proof of work" $ do
        let header = genesisBlock ^. bHeader
            target = hash2Integer (toHash header) + 1
        checkNonce header (fromIntegral target) @?= True
    , testCase "should return False for invalid proof of work" $ do
        let header = genesisBlock ^. bHeader
            target = hash2Integer (toHash header) - 1
        checkNonce header (fromIntegral target) @?= False
    , testCase "should find valid nonce when mining block" $ do
        let target = Target ((2 :: Integer) ^ (255 :: Integer))
            result = mineBlock genesisBlock target
        case result of
          Left err ->
            assertFailure $ "mineBlock failed unexpectedly: " <> show err
          Right minedBlock ->
            assertBool "mined block should satisfy proof of work" $
              checkNonce (minedBlock ^. bHeader) target
    , testCase "should fail when nonce would overflow" $ do
        let blockWithMaxNonce = genesisBlock & bHeader . bNonce .~ maxBound
            strictestTarget = Target 0
            result = mineBlock blockWithMaxNonce strictestTarget
        case result of
          Left _ ->
            pure ()
          Right _ ->
            assertFailure "mineBlock should have failed due to nonce overflow"
    ]

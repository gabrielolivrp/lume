{-# LANGUAGE OverloadedStrings #-}

module Lume.Consensus.DifficultyTest where

import Lume.Consensus.Difficulty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

difficultyTests :: TestTree
difficultyTests =
  testGroup
    "Difficulty Tests"
    [ testCase "toTarget: Genesis block bits (0x1d00ffff)" $
        let bits = Bits 0x1d00ffff
            expectedTarget = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
         in assertEqual "Should decode bits to correct target" expectedTarget (toTarget bits)
    , testCase "fromTarget: Genesis block target" $
        let target = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
            expectedBits = Bits 0x1d00ffff
         in assertEqual "Should encode target to correct bits" expectedBits (fromTarget target)
    , testCase "Roundtrip: bits -> target -> bits" $
        let bits = Bits 0x1c0ae493
         in assertEqual "Roundtrip bits -> target -> bits" bits (fromTarget (toTarget bits))
    , testCase "Roundtrip: target -> bits -> target" $
        let target = Target $ 0x123456 * 2 ^ (8 * (0x1b - 3) :: Integer)
         in assertEqual "Roundtrip target -> bits -> target" target (toTarget (fromTarget target))
    ]

{-# LANGUAGE OverloadedStrings #-}

module Lume.Core.Block.DifficultySpec where

import Lume.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

difficultySpec :: TestTree
difficultySpec =
  testGroup
    "Block.Difficulty"
    [ testCase "should convert genesis block bits to target" $ do
        let bits = Bits 0x1d00ffff
            expected = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
        toTarget bits @?= expected
    , testCase "should convert target to genesis block bits" $ do
        let target = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
            expected = Bits 0x1d00ffff
        fromTarget target @?= expected
    , testCase "should preserve bits through bits->target->bits roundtrip" $ do
        let originalBits = Bits 0x1c0ae493
        fromTarget (toTarget originalBits) @?= originalBits
    , testCase "should preserve target through target->bits->target roundtrip" $ do
        let originalTarget = Target $ 0x123456 * 2 ^ (8 * (0x1b - 3) :: Integer)
        toTarget (fromTarget originalTarget) @?= originalTarget
    , testCase "should maintain difficulty when timeframes match exactly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
        adjustDifficulty bits expectedRetargetInterval @?= bits
    , testCase "should increase difficulty when blocks are mined too quickly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 29600
            adjustedBits = adjustDifficulty bits actualTime
        assertBool "adjusted target should be lower than original target" $
          toTarget adjustedBits < toTarget bits
    , testCase "should decrease difficulty when blocks are mined too slowly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 2419200
            adjustedBits = adjustDifficulty bits actualTime
        assertBool "adjusted target should be higher than original target" $
          toTarget adjustedBits > toTarget bits
    , testCase "should limit difficulty decrease to 4.0x factor" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 6400000
            adjusted = adjustDifficulty bits actualTime
            originalTarget = toTarget bits
            expectedTarget =
              min
                (round $ fromIntegral originalTarget * (4.0 :: Double))
                maximumTarget
        toTarget adjusted @?= expectedTarget
    ]

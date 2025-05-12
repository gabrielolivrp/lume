{-# LANGUAGE OverloadedStrings #-}

module Lume.Consensus.DifficultyTest where

import Lume.Consensus.Difficulty
import Lume.Time.Timestamp (Timestamp (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

difficultyTests :: TestTree
difficultyTests =
  testGroup
    "Difficulty Tests"
    [ testCase "Convert genesis block bits to target" $ do
        let bits = Bits 0x1d00ffff
            expected = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
        toTarget bits @?= expected
    , testCase "Convert target to genesis block bits" $ do
        let target = Target $ 0x00ffff * 2 ^ (8 * (0x1d - 3) :: Integer)
            expected = Bits 0x1d00ffff
        fromTarget target @?= expected
    , testCase "Preserve bits through bits->target->bits roundtrip" $ do
        let originalBits = Bits 0x1c0ae493
        fromTarget (toTarget originalBits) @?= originalBits
    , testCase "Preserve target through target->bits->target roundtrip" $ do
        let originalTarget = Target $ 0x123456 * 2 ^ (8 * (0x1b - 3) :: Integer)
        toTarget (fromTarget originalTarget) @?= originalTarget
    , testCase "Maintain difficulty when timeframes match exactly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 1209600
            expectedTime = Timestamp 1209600
        adjustDifficulty bits actualTime expectedTime @?= bits
    , testCase "Increase difficulty when blocks are mined too quickly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 604800
            expectedTime = Timestamp 1209600
            adjusted = adjustDifficulty bits actualTime expectedTime
        assertBool "Target should decrease when blocks are mined faster" $
          toTarget adjusted < toTarget bits
    , testCase "Decrease difficulty when blocks are mined too slowly" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 1209600
            expectedTime = Timestamp 304800
            adjusted = adjustDifficulty bits actualTime expectedTime
        assertBool "Target should increase when blocks are mined slower" $
          toTarget adjusted > toTarget bits
    , testCase "Limit difficulty decrease to 4.0x factor" $ do
        let bits = fromTarget (maximumTarget `div` 2)
            actualTime = Timestamp 6400000
            expectedTime = Timestamp 1600000
            adjusted = adjustDifficulty bits actualTime expectedTime

            originalTarget = toTarget bits
            expectedTarget =
              min
                (round $ fromIntegral originalTarget * (4.0 :: Double))
                maximumTarget

        toTarget adjusted @?= expectedTarget
    ]

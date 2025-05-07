import Test.Tasty (TestTree, defaultMain, testGroup)

import Lume.Consensus.DifficultyTest (difficultyTests)
import Lume.Crypto.AddressTest (addressTests)
import Lume.Crypto.HashTest (hashTests)
import Lume.Crypto.MerkleTreeTest (merkleTreeTests)
import Lume.Crypto.SignatureTest (signatureTests)

cryptoTests :: TestTree
cryptoTests =
  testGroup
    "Crypto Tests"
    [ hashTests
    , addressTests
    , signatureTests
    , merkleTreeTests
    ]

consensusTests :: TestTree
consensusTests =
  testGroup
    "Consensus Tests"
    [ difficultyTests
    ]

tests :: TestTree
tests =
  testGroup
    "Lume"
    [ cryptoTests
    , consensusTests
    ]

main :: IO ()
main = defaultMain tests

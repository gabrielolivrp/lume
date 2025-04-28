import Test.Tasty (TestTree, defaultMain, testGroup)

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

tests :: TestTree
tests = testGroup "Lume" [cryptoTests]

main :: IO ()
main = defaultMain tests

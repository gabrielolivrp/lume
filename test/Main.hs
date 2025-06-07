import Test.Tasty (TestTree, defaultMain, testGroup)

import Lume.Block.InternalTest (blockInternalTests)
import Lume.Consensus.DifficultyTest (difficultyTests)
import Lume.Consensus.PoWTest (powTests)
import Lume.Crypto.AddressTest (addressTests)
import Lume.Crypto.HashTest (hashTests)
import Lume.Crypto.MerkleTreeTest (merkleTreeTests)
import Lume.Crypto.SignatureTest (signatureTests)
import Lume.Storage.DatabaseTest (databaseTests)
import Lume.Storage.FileStorageTest (fileStorageTests)
import Lume.Transaction.InternalTest (transactionInternalTests)
import Lume.Wallet.InternalTest (walletInternalTests)

cryptoTests :: TestTree
cryptoTests =
  testGroup
    "Crypto"
    [ hashTests
    , addressTests
    , signatureTests
    , merkleTreeTests
    ]

consensusTests :: TestTree
consensusTests =
  testGroup
    "Consensus"
    [ difficultyTests
    , powTests
    ]

blockTests :: TestTree
blockTests =
  testGroup
    "Block"
    [ blockInternalTests
    ]

transactionTests :: TestTree
transactionTests =
  testGroup
    "Transaction"
    [ transactionInternalTests
    ]

walletTests :: TestTree
walletTests =
  testGroup
    "Wallet"
    [ walletInternalTests
    ]

storageTests :: TestTree
storageTests =
  testGroup
    "Storage"
    [ fileStorageTests
    , databaseTests
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lume"
      [ cryptoTests
      , consensusTests
      , blockTests
      , transactionTests
      , walletTests
      , storageTests
      ]

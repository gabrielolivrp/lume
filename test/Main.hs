import Test.Tasty (TestTree, defaultMain, testGroup)

import Lume.Block.BuilderTest (blockBuilderTests)
import Lume.Consensus.DifficultyTest (difficultyTests)
import Lume.Consensus.PoWTest (powTests)
import Lume.Crypto.AddressTest (addressTests)
import Lume.Crypto.HashTest (hashTests)
import Lume.Crypto.MerkleTreeTest (merkleTreeTests)
import Lume.Crypto.SignatureTest (signatureTests)
import Lume.Storage.DatabaseTest (databaseTests)
import Lume.Storage.FileStorageTest (fileStorageTests)
import Lume.Transaction.BuilderTest (transactionBuilderTests)
import Lume.Wallet.TxTest (walletTxTests)

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
    [ blockBuilderTests
    ]

transactionTests :: TestTree
transactionTests =
  testGroup
    "Transaction"
    [ transactionBuilderTests
    ]

walletTests :: TestTree
walletTests =
  testGroup
    "Wallet"
    [ walletTxTests
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

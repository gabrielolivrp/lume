import Test.Tasty (TestTree, defaultMain, testGroup)

import Lume.Core.Block.DifficultyTest (coreDifficultyTests)
import Lume.Core.Block.InternalTest (nodeBlockInternalTests)
import Lume.Core.Crypto.AddressTest (coreAddressTests)
import Lume.Core.Crypto.HashTest (coreHashTests)
import Lume.Core.Crypto.MerkleTreeTest (coreMerkleTreeTests)
import Lume.Core.Crypto.SignatureTest (coreSignatureTests)
import Lume.Node.ChainTest (nodeChainTests)
import Lume.Node.Miner.PoWTest (nodeMinerPoWTests)
import Lume.Node.Storage.DatabaseTest (nodeStorageDatabaseTests)
import Lume.Node.Storage.FileStorageTest (nodeStorageFileStorageTests)
import Lume.Wallet.InternalTest (walletInternalTests)
import Lume.Wallet.Storage.DatabaseTest (walletStorageDatabaseTests)
import Lume.Wallet.Transaction.BuilderTest (walletTransactionBuilderTests)
import Lume.Wallet.Transaction.SignerTest (walletSignerTests)

coreTests :: TestTree
coreTests =
  testGroup
    "Core"
    [ nodeBlockInternalTests
    , coreDifficultyTests
    , coreAddressTests
    , coreHashTests
    , coreMerkleTreeTests
    , coreSignatureTests
    ]

nodeTests :: TestTree
nodeTests =
  testGroup
    "Node"
    [ nodeMinerPoWTests
    , nodeStorageDatabaseTests
    , nodeStorageFileStorageTests
    , nodeChainTests
    ]

walletTests :: TestTree
walletTests =
  testGroup
    "Wallet"
    [ walletStorageDatabaseTests
    , walletTransactionBuilderTests
    , walletSignerTests
    , walletInternalTests
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lume"
      [ coreTests
      , nodeTests
      , walletTests
      ]

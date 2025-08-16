import Test.Tasty (defaultMain, testGroup)

import Lume.Node.ChainSpec (chainSpec)
import Lume.Node.Miner.PoWSpec (poWSpec)
import Lume.Node.Storage.DatabaseSpec (databaseSpec)
import Lume.Node.Storage.FileStorageSpec (fileStorageSpec)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lume.Node"
      [ poWSpec
      , databaseSpec
      , fileStorageSpec
      , chainSpec
      ]

import Test.Tasty (defaultMain, testGroup)

import Lume.Core.Block.DifficultySpec (difficultySpec)
import Lume.Core.Block.InternalSpec (blockInternalSpec)
import Lume.Core.Crypto.AddressSpec (addressSpec)
import Lume.Core.Crypto.HashSpec (hashSpec)
import Lume.Core.Crypto.MerkleTreeSpec (merkleTreeSpec)
import Lume.Core.Crypto.SignatureSpec (signatureSpec)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lume.Core"
      [ blockInternalSpec
      , difficultySpec
      , addressSpec
      , hashSpec
      , merkleTreeSpec
      , signatureSpec
      ]

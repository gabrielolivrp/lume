import Test.Tasty (defaultMain, testGroup)

import Lume.Wallet.InternalSpec (internalSpec)
import Lume.Wallet.Storage.DatabaseSpec (storageDatabaseSpec)
import Lume.Wallet.Transaction.BuilderSpec (transactionBuilderSpec)
import Lume.Wallet.Transaction.SignerSpec (signerSpec)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lume.Wallet"
      [ internalSpec
      , storageDatabaseSpec
      , transactionBuilderSpec
      , signerSpec
      ]

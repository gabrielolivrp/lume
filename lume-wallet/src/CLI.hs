{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module CLI where

import GHC.Natural (Natural)
import Lume.Wallet
import Lume.Wallet.Command qualified as Command
import Options.Applicative

defaultWalletConfigPath :: FilePath
defaultWalletConfigPath = "wallet.config.json"
{-# INLINE defaultWalletConfigPath #-}

data Command
  = Wallet WalletCmd
  deriving (Eq)

data WalletCmd
  = ListAddresses
      { walletConfigPath :: FilePath
      }
  | CreateWallet
      { walletName :: String
      , walletConfigPath :: FilePath
      }
  | GetWalletInfo
      { walletName :: String
      , walletConfigPath :: FilePath
      }
  | SendTransaction
      { walletName :: String
      , to :: String
      , amount :: Natural
      , walletConfigPath :: FilePath
      }
  | ScanFullUTXO
      { walletConfigPath :: FilePath
      }
  deriving (Eq)

walletCommands :: Parser WalletCmd
walletCommands = hsubparser (createCommand <> infoCommand <> listCommand <> sendTxCommand <> fullScan)
 where
  createCommand =
    command
      "create"
      ( info
          ( CreateWallet
              <$> strArgument
                ( metavar "WALLET_NAME"
                    <> help "Name for the new wallet"
                )
              <*> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the wallet configuration file"
                    <> value defaultWalletConfigPath
                    <> showDefault
                )
          )
          (progDesc "Create a new wallet with the specified name")
      )

  infoCommand =
    command
      "info"
      ( info
          ( GetWalletInfo
              <$> strArgument
                ( metavar "WALLET_NAME"
                    <> help "Name of the wallet to retrieve information about"
                )
              <*> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the wallet configuration file"
                    <> value defaultWalletConfigPath
                    <> showDefault
                )
          )
          (progDesc "Display detailed information about a specific wallet")
      )

  listCommand =
    command
      "list"
      ( info
          ( ListAddresses
              <$> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the wallet configuration file"
                    <> value defaultWalletConfigPath
                    <> showDefault
                )
          )
          (progDesc "List all wallets and their associated addresses")
      )

  sendTxCommand =
    command
      "send"
      ( info
          ( SendTransaction
              <$> strArgument
                ( metavar "WALLET_NAME"
                    <> help "Name of the wallet to send coins from"
                )
              <*> strArgument
                ( metavar "TO_ADDRESS"
                    <> help "Destination address for the transaction"
                )
              <*> argument
                auto
                ( metavar "AMOUNT"
                    <> help "Amount of coins to send"
                )
              <*> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the wallet configuration file"
                    <> value defaultWalletConfigPath
                    <> showDefault
                )
          )
          (progDesc "Send coins from the specified wallet to a recipient address")
      )

  fullScan =
    command
      "fullscan"
      ( info
          ( ScanFullUTXO
              <$> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the wallet configuration file"
                    <> value defaultWalletConfigPath
                    <> showDefault
                )
          )
          (progDesc "Perform a full scan of UTXOs in the wallet")
      )

commands :: Parser Command
commands =
  hsubparser
    ( command
        "wallet"
        ( info
            (Wallet <$> walletCommands)
            (progDesc "Wallet management operations")
        )
    )

parseCLI :: IO Command
parseCLI = execParser opts
 where
  opts =
    info
      (commands <**> helper)
      ( fullDesc
          <> header "Lume CLI"
          <> progDesc "Command line interface for Lume blockchain"
      )

handleWallet :: WalletCmd -> IO ()
handleWallet (SendTransaction walletName' to' amount' configPath) =
  Command.sendTransactionCommand configPath (mkWalletName walletName') to' amount'
handleWallet (CreateWallet walletName' configPath) =
  Command.newWalletCommand configPath (mkWalletName walletName')
handleWallet (GetWalletInfo walletName' configPath) =
  Command.getWalletInfoCommand configPath (mkWalletName walletName')
handleWallet (ListAddresses configPath) = Command.listAddressesCommand configPath
handleWallet (ScanFullUTXO configPath) = Command.scanFullUTXOCommand configPath

run :: IO ()
run =
  parseCLI
    >>= \case
      Wallet walletCmd -> handleWallet walletCmd

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module CLI where

import GHC.Natural (Natural)
import Lume.Node qualified as Node
import Lume.Wallet qualified as Wallet
import Options.Applicative

defaultNodeConfigPath :: FilePath
defaultNodeConfigPath = "node.config.json"
{-# INLINE defaultNodeConfigPath #-}

defaultWalletConfigPath :: FilePath
defaultWalletConfigPath = "wallet.config.json"
{-# INLINE defaultWalletConfigPath #-}

data Command
  = Node NodeCmd
  | Wallet WalletCmd
  deriving (Eq)

data NodeCmd
  = CreateBlockchain {nodeConfigPath :: FilePath}
  | StartNode {nodeConfigPath :: FilePath}
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

nodeCommands :: Parser NodeCmd
nodeCommands = hsubparser (createChainCommand <> startCommand)
 where
  createChainCommand =
    command
      "createblockchain"
      ( info
          ( CreateBlockchain
              <$> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the node configuration file"
                    <> value defaultNodeConfigPath
                    <> showDefault
                )
          )
          (progDesc "Initialize a new blockchain using the specified configuration")
      )

  startCommand =
    command
      "start"
      ( info
          ( StartNode
              <$> strArgument
                ( metavar "CONFIG_PATH"
                    <> help "Path to the node configuration file"
                    <> value defaultNodeConfigPath
                    <> showDefault
                )
          )
          (progDesc "Start the blockchain node with the provided configuration")
      )

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
        "node"
        ( info
            (Node <$> nodeCommands)
            (progDesc "Blockchain node operations")
        )
        <> command
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
  Wallet.sendTransactionCommand configPath (Wallet.mkWalletName walletName') to' amount'
handleWallet (CreateWallet walletName' configPath) =
  Wallet.newWalletCommand configPath (Wallet.mkWalletName walletName')
handleWallet (GetWalletInfo walletName' configPath) =
  Wallet.getWalletInfoCommand configPath (Wallet.mkWalletName walletName')
handleWallet (ListAddresses configPath) = Wallet.listAddressesCommand configPath
handleWallet (ScanFullUTXO configPath) = Wallet.scanFullUTXOCommand configPath

handleNode :: NodeCmd -> IO ()
handleNode (CreateBlockchain configPath) = Node.createBlockchainCommand configPath
handleNode (StartNode configPath) = Node.startNodeCommand configPath

run :: IO ()
run =
  parseCLI
    >>= \case
      Node nodeCmd -> handleNode nodeCmd
      Wallet walletCmd -> handleWallet walletCmd

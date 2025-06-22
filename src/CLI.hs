{-# LANGUAGE ImportQualifiedPost #-}

module CLI where

import GHC.Natural (Natural)
import Lume.Wallet qualified as Wallet
import Options.Applicative

data Command
  = Node NodeCmd
  | Wallet WalletCmd
  deriving (Eq)

data NodeCmd
  = StartNode {privKey :: Maybe String}
  deriving (Eq)

data WalletCmd
  = ListAddresses
  | CreateWallet {walletName :: String}
  | GetWalletInfo {walletName :: String}
  | SendTransaction {walletName :: String, to :: String, amount :: Natural}
  deriving (Eq)

nodeCommands :: Parser NodeCmd
nodeCommands = hsubparser startCommand
 where
  startCommand =
    command
      "start"
      ( info
          ( StartNode
              <$> optional
                ( strOption
                    ( long "privkey"
                        <> metavar "PRIVATE_KEY"
                        <> help "Private key for the node"
                    )
                )
          )
          (progDesc "Start the blockchain node")
      )

walletCommands :: Parser WalletCmd
walletCommands = hsubparser (createCommand <> infoCommand <> listCommand <> sendTxCommand)
 where
  createCommand =
    command
      "create"
      ( info
          ( CreateWallet
              <$> strArgument
                ( metavar "WALLET_NAME" <> help "Name of the wallet to create"
                )
          )
          (progDesc "Create a new wallet")
      )

  infoCommand =
    command
      "info"
      ( info
          ( GetWalletInfo
              <$> strArgument
                ( metavar "WALLET_NAME" <> help "Name of the wallet to get info about"
                )
          )
          (progDesc "Get information about a specific wallet")
      )

  listCommand =
    command
      "list"
      ( info
          (pure ListAddresses)
          (progDesc "List all addresses in the wallet")
      )

  sendTxCommand =
    command
      "send"
      ( info
          ( SendTransaction
              <$> strArgument
                ( metavar "WALLET_NAME" <> help "Name of the wallet to send from"
                )
              <*> strArgument
                ( metavar "TO_ADDRESS" <> help "Address to send the transaction to"
                )
              <*> argument
                auto
                ( metavar "AMOUNT"
                    <> help "Amount to send in the transaction (in smallest unit)"
                )
          )
          (progDesc "Send a transaction to a specified address")
      )

commands :: Parser Command
commands =
  hsubparser
    ( command
        "node"
        ( info
            (Node <$> nodeCommands)
            (progDesc "Operations to the blockchain node")
        )
        <> command
          "wallet"
          ( info
              (Wallet <$> walletCommands)
              (progDesc "Operations to the wallet management")
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
handleWallet (SendTransaction walletName' to' amount') =
  Wallet.sendTransactionCommand (Wallet.mkWalletName walletName') to' amount'
handleWallet (CreateWallet walletName') =
  Wallet.newWalletCommand (Wallet.mkWalletName walletName')
handleWallet (GetWalletInfo walletName') =
  Wallet.getWalletInfoCommand (Wallet.mkWalletName walletName')
handleWallet ListAddresses = Wallet.listAddressesCommand

handle :: Command -> IO ()
handle (Node _) = do putStrLn "node..."
handle (Wallet walletCmd) = handleWallet walletCmd

run :: IO ()
run = do
  cmd <- parseCLI
  handle cmd

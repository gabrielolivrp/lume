{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module CLI where

import Lume.Node.Command qualified as Command
import Options.Applicative

defaultNodeConfigPath :: FilePath
defaultNodeConfigPath = "node.config.json"
{-# INLINE defaultNodeConfigPath #-}

data Command
  = Node NodeCmd
  deriving (Eq)

data NodeCmd
  = CreateBlockchain {nodeConfigPath :: FilePath}
  | StartNode {nodeConfigPath :: FilePath}
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

commands :: Parser Command
commands =
  hsubparser
    ( command
        "node"
        ( info
            (Node <$> nodeCommands)
            (progDesc "Blockchain node operations")
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

handleNode :: NodeCmd -> IO ()
handleNode (CreateBlockchain configPath) = Command.createBlockchainCommand configPath
handleNode (StartNode configPath) = Command.startNodeCommand configPath

run :: IO ()
run =
  parseCLI
    >>= \case
      Node nodeCmd -> handleNode nodeCmd

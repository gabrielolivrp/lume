module CLI where

import Options.Applicative

data Command
  = Node NodeCmd

data NodeCmd
  = StartNode {privKey :: Maybe String}

nodeCommands :: Parser NodeCmd
nodeCommands =
  hsubparser
    ( command
        "start"
        ( info
            startParser
            (progDesc "Launch the node with an optional private key")
        )
    )

startParser :: Parser NodeCmd
startParser =
  StartNode
    <$> optional
      ( strOption
          ( long "privkey"
              <> short 'k'
              <> metavar "PRIVATE_KEY"
              <> help "Private key to initialize the node (optional)"
          )
      )

commands :: Parser Command
commands =
  hsubparser
    ( command
        "node"
        ( info
            (Node <$> nodeCommands)
            (progDesc "Operations related to the blockchain node")
        )
    )

parseCLI :: IO Command
parseCLI = execParser opts
 where
  opts =
    info
      (commands <**> helper)
      ( fullDesc
          <> progDesc "Lume Command Line Interface"
          <> header "Lume CLI - A command-line tool for managing your blockchain node"
      )

handle :: Command -> IO ()
handle (Node _) = do
  putStrLn "Initializing node..."

run :: IO ()
run = do
  cmd <- parseCLI
  handle cmd

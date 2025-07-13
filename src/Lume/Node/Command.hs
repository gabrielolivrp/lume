module Lume.Node.Command where

import Control.Monad.Except (runExceptT)
import Lume.Node.Chain (createBlockchain)
import Lume.Node.Config (Config (cDataDir), parseConfig)
import Lume.Node.Network (startNode)
import Lume.Node.Storage (openBlockDB, openChainStateDB, runDatabase)

separator :: String
separator = "====================================================="

printSection :: String -> IO ()
printSection title = do
  putStrLn $ "\n" ++ title
  putStrLn separator

startNodeCommand :: FilePath -> IO ()
startNodeCommand configPath = do
  config <- parseConfig configPath
  putStrLn "🚀 Starting blockchain node..."
  startNode config

createBlockchainCommand :: FilePath -> IO ()
createBlockchainCommand configPath = do
  config <- parseConfig configPath
  result <- runDatabase $ do
    blockDB <- openBlockDB (cDataDir config)
    chainstateDB <- openChainStateDB (cDataDir config)
    runExceptT $ createBlockchain blockDB chainstateDB config
  case result of
    Left err -> putStrLn $ "❌ Failed to initialize blockchain: " ++ show err
    Right _ -> do
      putStrLn "\n✅ Blockchain created successfully!"
      putStrLn separator

printChainCommand :: FilePath -> IO ()
printChainCommand configPath = do
  _config <- parseConfig configPath
  putStrLn "🔧 printChainCommand is not implemented yet."

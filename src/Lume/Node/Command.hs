{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Command where

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BS
import Lume.Node.Config (NodeConfig)
import Lume.Node.Network (startNode)

startNodeCommand :: FilePath -> IO ()
startNodeCommand configPath = do
  configContent <- BS.readFile configPath
  case decode configContent :: Maybe NodeConfig of
    Just config -> startNode config
    Nothing -> putStrLn "Failed to parse configuration file."

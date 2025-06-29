{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Config (
  NodeConfig (..),
)
where

import Data.Aeson

data MiningConfig = MiningConfig
  { mcEnabled :: Bool
  , mcCoinbaseAddress :: String
  }
  deriving (Show, Eq)

data NodeConfig = NodeConfig
  { ncPort :: Int
  , ncHost :: String
  , ncDataDir :: FilePath
  , ncMining :: MiningConfig
  , ncPeers :: [String]
  }
  deriving (Show, Eq)

instance FromJSON MiningConfig where
  parseJSON = withObject "MiningConfig" $ \v ->
    MiningConfig
      <$> v .: "enabled"
      <*> v .: "coinbase_address"

instance FromJSON NodeConfig where
  parseJSON = withObject "NodeConfig" $ \v ->
    NodeConfig
      <$> v .: "port"
      <*> v .: "host"
      <*> v .: "data_dir"
      <*> v .: "mining"
      <*> v .: "peers"

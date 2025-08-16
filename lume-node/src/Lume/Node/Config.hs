{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Node.Config (
  Config (..),
  MiningConfig (..),
  RpcConfig (..),
  parseConfig,
)
where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS

data MiningConfig = MiningConfig
  { mEnabled :: Bool
  , mCoinbaseAddress :: String
  }
  deriving (Show, Eq)

data RpcConfig = RpcConfig
  { rpcEnabled :: Bool
  , rpcHost :: String
  , rpcPort :: Int
  }
  deriving (Show, Eq)

instance FromJSON RpcConfig where
  parseJSON = withObject "RpcConfig" $ \v ->
    RpcConfig
      <$> v .: "enabled"
      <*> v .: "host"
      <*> v .: "port"

data Config = Config
  { cPort :: Int
  , cHost :: String
  , cDataDir :: FilePath
  , cMining :: MiningConfig
  , cPeers :: [String]
  , cRpc :: RpcConfig
  }
  deriving (Show, Eq)

instance FromJSON MiningConfig where
  parseJSON = withObject "MiningConfig" $ \v ->
    MiningConfig
      <$> v .: "enabled"
      <*> v .: "coinbase_address"

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .: "port"
      <*> v .: "host"
      <*> v .: "data_dir"
      <*> v .: "mining"
      <*> v .: "peers"
      <*> v .: "rpc"

parseConfig :: FilePath -> IO Config
parseConfig fp = do
  configContent <- BS.readFile fp
  case decode configContent of
    Just config -> pure config
    Nothing -> fail $ "Failed to parse configuration file: " ++ fp

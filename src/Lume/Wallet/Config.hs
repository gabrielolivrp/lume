{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Config where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS

data WalletRpcConfig = WalletRpcConfig
  { rpcHost :: String
  , rpcPort :: Int
  }

instance FromJSON WalletRpcConfig where
  parseJSON = withObject "WalletRpcConfig" $ \o ->
    WalletRpcConfig
      <$> o .: "host"
      <*> o .: "port"

data Config = Config
  { cDataDir :: FilePath
  , cRpc :: WalletRpcConfig
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config
      <$> o .: "data_dir"
      <*> o .: "rpc"

parseConfig :: FilePath -> IO Config
parseConfig fp = do
  configContent <- BS.readFile fp
  case decode configContent of
    Just config -> pure config
    Nothing -> fail $ "Failed to parse configuration file: " ++ fp

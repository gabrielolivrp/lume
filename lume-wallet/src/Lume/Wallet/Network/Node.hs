{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lume.Wallet.Network.Node where

import Control.Applicative ((<|>))
import Data.Aeson
import Lume.Core.Crypto.Hash (Hash)
import Network.HTTP.Simple

data RpcRequest a = RpcRequest
  { rpcReqMethod :: String
  , rpcReqParams :: [a]
  , rpcReqId :: Int
  }
  deriving (Show)

instance (ToJSON a) => ToJSON (RpcRequest a) where
  toJSON (RpcRequest method params rpcId) =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "method" .= method
      , "params" .= params
      , "id" .= rpcId
      ]

instance (FromJSON a) => FromJSON (RpcRequest a) where
  parseJSON = withObject "RpcRequest" $ \v ->
    RpcRequest
      <$> v .: "method"
      <*> v .: "params"
      <*> v .: "id"

data RpcError = RpcError
  { rpcErrCode :: Int
  , rpcErrMessage :: String
  }
  deriving (Show)

instance FromJSON RpcError where
  parseJSON = withObject "RpcError" $ \v ->
    RpcError
      <$> v .: "code"
      <*> v .: "message"

instance ToJSON RpcError where
  toJSON (RpcError code message) =
    object
      [ "code" .= code
      , "message" .= message
      ]

data RpcResponse a
  = RpcResponseSuccess
      { rpcResId :: Int
      , rpcResResult :: a
      }
  | RpcResponseError
      { rpcResId :: Int
      , rpcResError :: RpcError
      }
  deriving (Show)

instance (ToJSON a) => ToJSON (RpcResponse a) where
  toJSON (RpcResponseSuccess rpcId' result) =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "id" .= rpcId'
      , "result" .= result
      ]
  toJSON (RpcResponseError rpcId' error') =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "id" .= rpcId'
      , "error" .= error'
      ]

instance (FromJSON a) => FromJSON (RpcResponse a) where
  parseJSON = withObject "RpcResponse" $ \v -> do
    successP v <|> errorP v
   where
    successP v =
      RpcResponseSuccess
        <$> v .: "id"
        <*> v .: "result"

    errorP v =
      RpcResponseError
        <$> v .: "id"
        <*> v .: "error"

handleRequest' :: (ToJSON a, FromJSON b) => String -> [RpcRequest a] -> IO (Either [String] [b])
handleRequest' rpcUrl requests = do
  response <- handleRequest rpcUrl requests
  case response of
    Left err -> pure (Left [err])
    Right response' -> do
      let (errors, successes) = partitionResults response'
      if not (null errors)
        then pure (Left errors)
        else pure (Right successes)

partitionResults :: [RpcResponse a] -> ([String], [a])
partitionResults = foldr partition ([], [])
 where
  partition (RpcResponseError _ (RpcError _ e)) (errors, successes) = (e : errors, successes)
  partition (RpcResponseSuccess _ s) (errors, successes) = (errors, s : successes)

handleRequest :: (ToJSON a, FromJSON b) => String -> a -> IO (Either String b)
handleRequest url rpcRequest = do
  initReq <- parseRequest url
  let request =
        setRequestMethod "POST" $
          setRequestBodyJSON rpcRequest initReq
  response <- httpLBS request
  let body = getResponseBody response
  case eitherDecode body of
    Right result -> pure (Right result)
    Left err -> pure (Left err)

fetchGetBlockCount :: String -> IO (Either String Int)
fetchGetBlockCount baseUrl = do
  let request = RpcRequest "getblockcount" [()] 1
  result <- handleRequest baseUrl request
  case result of
    Right (RpcResponseSuccess _ count) -> pure (Right count)
    Right (RpcResponseError _ (RpcError _ msg)) -> pure (Left msg)
    Left err -> pure (Left err)

sendRawTransaction :: String -> String -> IO (Either String String)
sendRawTransaction baseUrl rawTx = do
  let request = mkSendRawTransactionRequest 1 rawTx
  result <- handleRequest baseUrl request
  case result of
    Right (RpcResponseSuccess _ txid) -> pure (Right txid)
    Right (RpcResponseError _ (RpcError _ msg)) -> pure (Left msg)
    Left err -> pure (Left err)

mkGetBlockHashRequest :: Int -> Int -> RpcRequest Int
mkGetBlockHashRequest rpcId blockIndex = RpcRequest "getblockhash" [blockIndex] rpcId

mkGetBlockRequest :: Int -> Hash -> RpcRequest Hash
mkGetBlockRequest rpcId blockHash = RpcRequest "getblock" [blockHash] rpcId

mkSendRawTransactionRequest :: Int -> String -> RpcRequest String
mkSendRawTransactionRequest rpcId rawTx = RpcRequest "sendrawtransaction" [rawTx] rpcId

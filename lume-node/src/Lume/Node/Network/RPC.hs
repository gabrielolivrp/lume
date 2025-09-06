{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lume.Node.Network.RPC (startRPCServer) where

import Control.Applicative ((<|>))
import Control.Distributed.Process (Process, say)
import Control.Distributed.Process.Node (runProcess)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Lume.Core
import Lume.Core.Crypto.Hash (fromHex)
import Lume.Node.Chain (ChainM, getBestHeight, getBlockByHash, getBlockByHeight, runChainM, validateTransaction)
import Lume.Node.Config (Config (cRpc), RpcConfig (rpcHost, rpcPort))
import Lume.Node.Network.Message (broadcast, mkTxInv)
import Lume.Node.Network.State (NodeContext (nDatabase, nLocalNode, nState), insertMempoolTx)
import Network.HTTP.Types (status200, status405)
import Network.Wai (Request (requestMethod), getRequestBodyChunk, responseLBS)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal

data RPCRequest
  = RPCRequest
      Int -- Request ID
      String -- Method name
      Value -- Parameters
      String -- JSON-RPC version

data RPCError
  = RPCError
      Int -- Error code
      String -- Error message
      (Maybe Value) -- Additional data (optional)

data RPCResponse
  = RPCResult Int Value String -- Request ID, result value, and JSON-RPC version
  | RPCFailure Int RPCError String -- Request ID, error, and JSON-RPC version

instance ToJSON RPCRequest where
  toJSON (RPCRequest i method params v) =
    object
      [ "jsonrpc" .= v
      , "method" .= method
      , "params" .= params
      , "id" .= i
      ]

instance FromJSON RPCRequest where
  parseJSON = withObject "RPCRequest" $ \v ->
    RPCRequest
      <$> v .: "id"
      <*> v .: "method"
      <*> v .: "params"
      <*> v .: "jsonrpc"

instance ToJSON RPCError where
  toJSON (RPCError code msg mdata) =
    object
      [ "code" .= code
      , "message" .= msg
      , "data" .= mdata
      ]

instance FromJSON RPCError where
  parseJSON = withObject "RPCError" $ \v ->
    RPCError
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"

instance ToJSON RPCResponse where
  toJSON (RPCResult i result v) =
    object
      [ "jsonrpc" .= v
      , "result" .= result
      , "id" .= i
      ]
  toJSON (RPCFailure i err v) =
    object
      [ "jsonrpc" .= v
      , "error" .= err
      , "id" .= i
      ]
data RPCWrapper a
  = Single a
  | Batch [a]
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (RPCWrapper a) where
  toJSON (Single x) = toJSON x
  toJSON (Batch xs) = toJSON xs

instance (FromJSON a) => FromJSON (RPCWrapper a) where
  parseJSON v = (Single <$> parseJSON v) <|> (Batch <$> parseJSON v)

rpcVersion :: String
rpcVersion = "2.0"

mkErrorInvalidRequest :: Int -> String -> Maybe Value -> RPCResponse
mkErrorInvalidRequest reqId msg mdata = RPCFailure reqId (RPCError (-32600) msg mdata) rpcVersion

mkInternalRPCError :: Int -> String -> Maybe Value -> RPCResponse
mkInternalRPCError reqId msg mdata = RPCFailure reqId (RPCError (-32603) msg mdata) rpcVersion

mkItemNotFound :: Int -> String -> Maybe Value -> RPCResponse
mkItemNotFound reqId item mdata = RPCFailure reqId (RPCError (-32004) (item ++ " not found") mdata) rpcVersion

mkErrorMethodNotFound :: Int -> String -> Maybe Value -> RPCResponse
mkErrorMethodNotFound reqId method mdata = RPCFailure reqId (RPCError (-32601) ("Method not found: " ++ method) mdata) rpcVersion

mkErrorInvalidParams :: Int -> String -> Maybe Value -> RPCResponse
mkErrorInvalidParams reqId msg mdata = RPCFailure reqId (RPCError (-32602) msg mdata) rpcVersion

mkSucess :: Int -> Value -> RPCResponse
mkSucess reqId result = RPCResult reqId result rpcVersion

startRPCServer :: Config -> NodeContext -> Process ()
startRPCServer config context = do
  let host = rpcHost (cRpc config)
      port = rpcPort (cRpc config)

  say $ "Starting RPC server on " ++ host ++ ":" ++ show port

  let settings =
        defaultSettings
          { settingsHost = fromString host
          , settingsPort = port
          }

  liftIO . runSettings settings $ \req respond -> do
    case requestMethod req of
      "POST" -> do
        result <-
          runChainM config (nDatabase context) (handleRequest context req)
            >>= \case
              Left err -> pure . Single $ mkInternalRPCError 0 (show err) Nothing
              Right resp -> pure resp
        respond $ responseLBS status200 [("Content-Type", "application/json")] (encode result)
      _ -> respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method Not Allowed"

handleRequest :: NodeContext -> Request -> ChainM m (RPCWrapper RPCResponse)
handleRequest context req = do
  body <- liftIO $ getRequestBodyChunk req
  let jsonBody = decode (BSL.fromStrict body) :: Maybe (RPCWrapper RPCRequest)
  case jsonBody of
    Just (Single (RPCRequest reqId method params _)) ->
      Single <$> dispatch context reqId method params
    Just (Batch requests) ->
      -- TODO: handle each request in parallel
      Batch <$> mapM (\(RPCRequest reqId method params _) -> dispatch context reqId method params) requests
    Nothing -> pure . Single $ mkErrorInvalidRequest 0 "Invalid JSON-RPC request" Nothing

withParsedParams :: (FromJSON a) => Value -> (a -> ChainM m RPCResponse) -> ChainM m RPCResponse
withParsedParams params handler = case fromJSON params of
  Success values -> handler values
  Error err -> pure $ mkErrorInvalidParams 0 err Nothing

dispatch :: NodeContext -> Int -> [Char] -> Value -> ChainM m RPCResponse
dispatch context reqId method params =
  case method of
    "getblockcount" -> withParsedParams params $ \() -> handleGetBlockCount reqId
    "getblockhash" -> withParsedParams params $ \case
      [Number height] -> handleGetBlockHash reqId (round height)
      _ -> pure $ mkErrorInvalidParams reqId "Expected a single numeric parameter for block height" Nothing
    "getblock" -> withParsedParams params $ \case
      [hash] -> handleGetBlock reqId hash
      _ -> pure $ mkErrorInvalidParams reqId "Expected a single string parameter for block hash" Nothing
    "sendrawtransaction" -> withParsedParams params $ \case
      [String rawTx] -> handleSendRawTransaction context reqId rawTx
      _ -> pure $ mkErrorInvalidParams reqId "Expected a single string parameter for raw transaction" Nothing
    _ -> pure $ mkErrorMethodNotFound reqId method Nothing

handleSendRawTransaction :: NodeContext -> Int -> Text.Text -> ChainM m RPCResponse
handleSendRawTransaction context reqId rawTx = do
  let rawTx' = BSL.fromStrict (encodeUtf8 rawTx)
  case txFromBase64 rawTx' of
    Left err -> pure $ mkErrorInvalidParams reqId ("Failed to decode base64 transaction: " ++ err) Nothing
    Right decoded ->
      case decode decoded of
        Nothing -> pure $ mkErrorInvalidParams reqId "Failed to decode transaction JSON" Nothing
        Just tx -> do
          validateTransaction tx
          let txid = txHash tx
              msg = mkTxInv txid
          liftIO $
            insertMempoolTx (nState context) tx
              >> runProcess (nLocalNode context) (broadcast context msg)
          pure $ mkSucess reqId (toJSON txid)

handleGetBlockCount :: Int -> ChainM m RPCResponse
handleGetBlockCount reqId = do
  bestHeight <- getBestHeight
  pure $ mkSucess reqId (Number . fromIntegral $ bestHeight)

handleGetBlockHash :: Int -> Int -> ChainM m RPCResponse
handleGetBlockHash reqId height = do
  block <- getBlockByHeight (fromIntegral height)
  pure $ case block of
    Nothing -> mkItemNotFound reqId "block" Nothing
    Just b -> mkSucess reqId (toJSON (blockHash b))

handleGetBlock :: Int -> String -> ChainM m RPCResponse
handleGetBlock reqId bHash =
  case fromHex (C8.pack bHash) of
    Nothing -> pure $ mkErrorInvalidParams reqId "Invalid block hash" Nothing
    Just bHash' -> do
      block <- getBlockByHash bHash'
      pure $ case block of
        Nothing -> mkItemNotFound reqId "block" Nothing
        Just b -> mkSucess reqId (toJSON b)

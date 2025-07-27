{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lume.Node.Network.RPC (startServer) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader (MonadIO (liftIO))
import Data.Aeson
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.String (IsString (fromString))
import Lume.Core
import Lume.Core.Crypto.Hash (fromHex)
import Lume.Node.Chain (ChainM, getBestBlock, getBlockByHash, getBlockByHeight, runChainM)
import Lume.Node.Config (Config (cRpc), RpcConfig (rpcHost, rpcPort))
import Lume.Node.Network.State (State)
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

startServer :: Config -> State -> IO ()
startServer config _state = do
  let settings =
        defaultSettings
          { settingsPort = rpcPort . cRpc $ config
          , settingsHost = fromString . rpcHost . cRpc $ config
          }

  runSettings settings $ \req respond ->
    case requestMethod req of
      "POST" -> do
        result <-
          runChainM config (handleRequest req) >>= \case
            Left err -> pure $ mkInternalRPCError 0 (show err) Nothing
            Right resp -> pure resp
        respond $ responseLBS status200 [("Content-Type", "application/json")] (encode result)
      _ -> respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method Not Allowed"

handleRequest :: Request -> ChainM m RPCResponse
handleRequest req = do
  body <- liftIO $ getRequestBodyChunk req
  let jsonBody = decode (BSL.fromStrict body) :: Maybe RPCRequest
  case jsonBody of
    Just (RPCRequest reqId method params _) -> do
      response <- dispatch reqId method params
      pure $ case response of
        Just res -> res
        Nothing -> mkErrorMethodNotFound reqId method Nothing
    Nothing -> pure $ mkErrorInvalidRequest 0 "Invalid JSON-RPC request" Nothing

withParsedParams :: (FromJSON a) => Value -> (a -> ChainM m (Maybe RPCResponse)) -> ChainM m (Maybe RPCResponse)
withParsedParams params handler = case fromJSON params of
  Success values -> handler values
  Error err -> pure . Just $ mkErrorInvalidParams 0 err Nothing

dispatch :: Int -> String -> Value -> ChainM m (Maybe RPCResponse)
dispatch reqId method params =
  case method of
    "getblockcount" -> withParsedParams params $ \() -> handleGetBlockCount reqId
    "getblockhash" -> withParsedParams params $ \case
      [Number height] -> handleGetBlockHash reqId (round height)
      _ -> pure . Just $ mkErrorInvalidParams reqId "Expected a single numeric parameter for block height" Nothing
    "getblock" -> withParsedParams params $ \case
      [hash] -> handleGetBlock reqId hash
      _ -> pure . Just $ mkErrorInvalidParams reqId "Expected a single string parameter for block hash" Nothing
    _ -> pure Nothing

handleGetBlockCount :: Int -> ChainM m (Maybe RPCResponse)
handleGetBlockCount reqId = do
  bestBlock <- getBestBlock
  pure . Just $ case bestBlock of
    Nothing -> mkItemNotFound reqId "best block" Nothing
    Just blockModel -> mkSucess reqId (Number . fromIntegral $ blockModel ^. bHeader . bHeight)

handleGetBlockHash :: Int -> Int -> ChainM m (Maybe RPCResponse)
handleGetBlockHash reqId height = do
  block <- getBlockByHeight (fromIntegral height)
  pure . Just $ case block of
    Nothing -> mkItemNotFound reqId "block" Nothing
    Just b -> mkSucess reqId (toJSON (blockHash b))

handleGetBlock :: Int -> String -> ChainM m (Maybe RPCResponse)
handleGetBlock reqId bHash = do
  case fromHex (Char8.pack bHash) of
    Nothing -> pure . Just $ mkErrorInvalidParams reqId "Invalid block hash" Nothing
    Just bHash' -> do
      block <- getBlockByHash bHash'
      pure . Just $ case block of
        Nothing -> mkItemNotFound reqId "block" Nothing
        Just b -> mkSucess reqId (toJSON b)

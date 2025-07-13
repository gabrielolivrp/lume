{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lume.Node.Network.RPC (startServer) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.String (IsString (fromString))
import Lume.Node.Config (Config (cRpc), RpcConfig (rpcHost, rpcPort))
import Lume.Node.Network.State (State)
import Network.HTTP.Types (status200, status405)
import Network.Wai (Request (requestMethod), Response, getRequestBodyChunk, responseLBS)
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
mkErrorInvalidRequest id' msg mdata = RPCFailure id' (RPCError (-32600) msg mdata) rpcVersion

mkErrorMethodNotFound :: Int -> String -> Maybe Value -> RPCResponse
mkErrorMethodNotFound id' method mdata = RPCFailure id' (RPCError (-32601) ("Method not found: " ++ method) mdata) rpcVersion

mkErrorInvalidParams :: Int -> String -> Maybe Value -> RPCResponse
mkErrorInvalidParams id' msg mdata = RPCFailure id' (RPCError (-32602) msg mdata) rpcVersion

mkSucess :: Int -> Value -> RPCResponse
mkSucess id' result = RPCResult id' result rpcVersion

handleMethod :: Int -> String -> Value -> IO (Maybe RPCResponse)
handleMethod id' "ping" params = do
  case fromJSON params of
    Success () -> pure . Just $ mkSucess id' (String "pong")
    Error _ -> pure . Just $ mkErrorInvalidParams id' "Invalid params" Nothing
handleMethod _ _ _ = pure Nothing

handleRequest :: Request -> (Response -> IO b) -> IO b
handleRequest req respond = do
  body <- getRequestBodyChunk req
  let jsonBody = decode (BS.fromStrict body) :: Maybe RPCRequest
  case jsonBody of
    Just (RPCRequest id' method params _) -> do
      response <- handleMethod id' method params
      case response of
        Just res -> respond $ responseLBS status200 [] (encode res)
        Nothing -> do
          let errorResponse = mkErrorMethodNotFound id' method Nothing
          respond $ responseLBS status200 [] (encode errorResponse)
    Nothing -> do
      let errorResponse = mkErrorInvalidRequest 0 "Invalid JSON" Nothing
      respond $ responseLBS status200 [] (encode errorResponse)

startServer :: Config -> State -> IO ()
startServer config _state = do
  let settings =
        defaultSettings
          { settingsPort = rpcPort . cRpc $ config
          , settingsHost = fromString $ rpcHost . cRpc $ config
          }
  runSettings settings $ \req respond ->
    case requestMethod req of
      "POST" -> handleRequest req respond
      _ -> respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method Not Allowed"

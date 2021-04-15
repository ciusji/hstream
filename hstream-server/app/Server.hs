{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent ( threadDelay )
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Vector as Vec
import Hstream.Server.HStreamApi
    ( CommandQueryResultSet(CommandQueryResultSet),
      CommandQueryResponseKind(CommandQueryResponseKindResultSet),
      CommandQueryResponse(CommandQueryResponse),
      CommandQuery(..),
      CommandPushQuery(..),
      CommandConnected(..),
      CommandConnect(..),
      HStreamApi(..),
      hstreamApiServer )
import Network.GRPC.HighLevel.Generated
    ( defaultServiceOptions,
      ServiceOptions,
      ServerRequest(ServerNormalRequest, ServerWriterRequest),
      ServerResponse(ServerNormalResponse, ServerWriterResponse),
      GRPCMethodType(Normal, ServerStreaming),
      StatusCode(StatusOk, StatusAborted) )
import ThirdParty.Google.Protobuf.Struct
    ( ValueKind(ValueKindNumberValue), Value(Value), Struct(Struct) )

handlers :: HStreamApi ServerRequest ServerResponse
handlers =
  HStreamApi
    { hstreamApiConnect = connectHandler,
      hstreamApiExecutePushQuery = executePushQueryHandler,
      hstreamApiExecuteQuery = executeQueryHandler
    }

connectHandler ::
  ServerRequest 'Normal CommandConnect CommandConnected ->
  IO (ServerResponse 'Normal CommandConnected)
connectHandler (ServerNormalRequest _metadata CommandConnect {..}) = do
  putStrLn "one client get connected"

  let connected =
        CommandConnected
          { commandConnectedServerVersion = "0.0.1.0",
            commandConnectedProtocolVersion = 1
          }
  return
    ( ServerNormalResponse
        connected
        []
        StatusOk
        "connect successfully!"
    )

executePushQueryHandler ::
  ServerRequest 'ServerStreaming CommandPushQuery Struct ->
  IO (ServerResponse 'ServerStreaming Struct)
executePushQueryHandler (ServerWriterRequest _metadata CommandPushQuery {..} streamSend) = loop 0.0
  where
    loop !i = do
      sendResult <- streamSend $ Struct $ Map.singleton "id" $ Just $ Value $ Just $ ValueKindNumberValue i
      case sendResult of
        Left err -> do
          putStrLn ("sendResult error: " ++ show err)
          return
            ( ServerWriterResponse
                []
                StatusAborted
                "send result aborted!"
            )
        Right _ -> putStrLn ("sendResult " ++ show i ++ " successfully!") >> threadDelay 1000000 >> loop (i + 1)

executeQueryHandler ::
  ServerRequest 'Normal CommandQuery CommandQueryResponse ->
  IO (ServerResponse 'Normal CommandQueryResponse)
executeQueryHandler (ServerNormalRequest _metadata CommandQuery {..}) = do
  putStrLn ("query stmt: " ++ show commandQueryStmtText)

  let resultSet = Vec.generate 5 (Struct . Map.singleton "id" . Just . Value . Just . ValueKindNumberValue . fromIntegral)
  return
    ( ServerNormalResponse
        (CommandQueryResponse (Just $ CommandQueryResponseKindResultSet $ CommandQueryResultSet resultSet))
        []
        StatusOk
        ""
    )

-- runningSumHandler :: ServerRequest 'ClientStreaming OneInt OneInt
--                      -> IO (ServerResponse 'ClientStreaming OneInt)
-- runningSumHandler (ServerReaderRequest _metadata recv) =
--   loop 0
--     where loop !i =
--             do msg <- recv
--                case msg of
--                  Left err -> return (ServerReaderResponse
--                                       Nothing
--                                       []
--                                       StatusUnknown
--                                       (fromString (show err)))
--                  Right (Just (OneInt x)) -> loop (i + x)
--                  Right Nothing -> return (ServerReaderResponse
--                                            (Just (OneInt i))
--                                            []
--                                            StatusOk
--                                            "")
--
options :: ServiceOptions
options = defaultServiceOptions

main :: IO ()
main = hstreamApiServer handlers options

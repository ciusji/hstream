{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Hstream.Server.HStreamApi
    ( CommandQueryResponseKind(CommandQueryResponseKindResultSet),
      CommandQueryResponse(CommandQueryResponse),
      CommandQuery(CommandQuery),
      CommandPushQuery(CommandPushQuery),
      CommandConnected(CommandConnected, commandConnectedProtocolVersion,
                       commandConnectedServerVersion),
      CommandConnect(CommandConnect, commandConnectClientVersion,
                     commandConnectProtocolVersion),
      HStreamApi(HStreamApi, hstreamApiExecuteQuery,
                 hstreamApiExecutePushQuery, hstreamApiConnect),
      hstreamApiClient )
import Network.GRPC.HighLevel.Generated
    ( withGRPCClient,
      ClientRequest(ClientReaderRequest, ClientNormalRequest),
      ClientResult(ClientReaderResponse, ClientNormalResponse,
                   ClientErrorResponse),
      ClientConfig(..) )

clientConfig :: ClientConfig
clientConfig =
  ClientConfig
    { clientServerHost = "localhost",
      clientServerPort = 50051,
      clientArgs = [],
      clientSSLConfig = Nothing,
      clientAuthority = Nothing
    }

main :: IO ()
main = withGRPCClient clientConfig $ \client -> do
  HStreamApi {..} <- hstreamApiClient client

  putStrLn "hstream client ready to connect to hstream server .... "
  -- Request for the Connect RPC
  let commandConnect =
        CommandConnect
          { commandConnectClientVersion = "0.0.0.1",
            commandConnectProtocolVersion = 1
          }
  resp <- hstreamApiConnect (ClientNormalRequest commandConnect 1 [])
  case resp of
    ClientNormalResponse CommandConnected {..} _meta1 _meta2 _status _details -> do
      putStrLn ("connect status: " ++ show _status)
      putStrLn ("connect details: " ++ show _details)
    ClientErrorResponse clientError -> putStrLn ("clientError: " ++ show clientError)

  -- Request for the executeQuery
  resp <- hstreamApiExecuteQuery (ClientNormalRequest (CommandQuery "select id from demoTable") 1 [])
  case resp of
    ClientNormalResponse (CommandQueryResponse (Just (CommandQueryResponseKindResultSet resultSet))) _meta1 _meta2 _status _details -> do
      putStrLn ("pull query results: " ++ show resultSet)
    ClientErrorResponse clientError -> putStrLn ("clientError: " ++ show clientError)

  -- Request for the executePushQuery
  let queryText = "select id from demo emit changes;"
  ClientReaderResponse _meta _stauts _details <-
    hstreamApiExecutePushQuery (ClientReaderRequest (CommandPushQuery queryText) 600 [] f)
  return ()
  where
    f clientCall metaData streamRecv = loop
      where
        loop = do
          msg <- streamRecv
          case msg of
            Left err -> putStrLn ("streaming recv error: " ++ show err)
            Right (Just result) -> putStrLn ("recv query result: " ++ show result) >> loop
            Right Nothing -> putStrLn ("streaming recv terminate")

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           HStreamApi
import           Network.GRPC.HighLevel.Generated

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 50051
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }

main :: IO ()
main = withGRPCClient clientConfig $ \client -> do
  HStreamApi{..} <- hstreamApiClient client

  putStrLn "hstream client ready to connect to hstream server .... "
  -- Request for the Connect RPC
  let commandConnect = CommandConnect{commandConnectClientVersion = "0.0.0.1", 
                                     commandConnectProtocolVersion = 1}
  ClientNormalResponse CommandConnected{..} _meta1 _meta2 _status _details
    <- hstreamApiConnect (ClientNormalRequest commandConnect 1 [])
  putStrLn "get connected!"

  -- Request for the RunningSum RPC
  -- ClientWriterResponse reply _streamMeta1 _streamMeta2 streamStatus streamDtls
  --   <- arithmeticRunningSum $ ClientWriterRequest 1 [] $ \send -> do
  --       eithers <- mapM send [OneInt 1, OneInt 2, OneInt 3]
  --                    :: IO [Either GRPCIOError ()]
  --       case sequence eithers of
  --         Left err -> error ("Error while streaming: " ++ show err)
  --         Right _  -> return ()

  -- case reply of
  --   Just (OneInt y) -> print ("1 + 2 + 3 = " ++ show y)
  --   Nothing -> putStrLn ("Client stream failed with status "
  --                        ++ show streamStatus
  --                        ++ " and details "
  --                        ++ show streamDtls)
  return ()

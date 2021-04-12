{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import HStreamApi
import Network.GRPC.HighLevel.Generated

import Data.String (fromString)

handlers :: HStreamApi ServerRequest ServerResponse
handlers = HStreamApi { hstreamApiConnect = connectHandler
                      }

connectHandler :: ServerRequest 'Normal CommandConnect CommandConnected 
              -> IO (ServerResponse 'Normal CommandConnected)
connectHandler (ServerNormalRequest _metadata CommandConnect{..}) = do

  putStrLn "one client get connected"

  let connected = CommandConnected {
                    commandConnectedServerVersion = "0.0.1.0"
                  , commandConnectedProtocolVersion = 1
                  }
  return (ServerNormalResponse connected 
                               []
                               StatusOk
                               "connect successfully!")


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

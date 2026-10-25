-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Collect native client traffic and its actual exchanges with the product image.
module Ecluse.E2E.Harness.CargoGateway (
    ObservedCargoProxy (..),
    withObservedCargoProxy,
    cargoTraffic,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status413, status503, statusCode)
import Network.Wai qualified as Wai

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.Container (withBridgeApplication)
import Ecluse.E2E.Harness.Docker (stubReadsNow, withE2EWith)
import Ecluse.E2E.Harness.Stub (StubRead (..), stubUrl)
import Ecluse.E2E.Harness.Types

-- | The incoming listener and the product connection keep independent request inventories.
data ObservedCargoProxy = ObservedCargoProxy
    { observedProxy :: E2E
    , observedClients :: TVar [CargoRequest]
    , observedForwards :: TVar [CargoRequest]
    }

-- | Advertise the collected listener so native discovery and archive requests use the same recorder.
withObservedCargoProxy :: E2EConfig -> GlobalDataPlane -> (ObservedCargoProxy -> IO a) -> IO a
withObservedCargoProxy config plane action = do
    destination <- newTVarIO Nothing
    incoming <- newTVarIO []
    forwarded <- newTVarIO []
    manager <- Http.newManager Http.defaultManagerSettings
    withBridgeApplication (gateway manager destination incoming forwarded) $ \port -> do
        let base = "http://127.0.0.1:" <> show port
            configured = config{ecExtraEnv = ecExtraEnv config <> [("ECLUSE_SERVER__PUBLIC_URL", base)]}
        withE2EWith
            configured
            ( \proxy -> do
                atomically (writeTVar destination (Just (e2eBaseUrl proxy)))
                let clientProxy = proxy{e2eBaseUrl = base, e2eClientEndpoints = Map.insert Cargo (base <> "/cargo/") (e2eClientEndpoints proxy)}
                action (ObservedCargoProxy clientProxy incoming forwarded)
            )
            plane

-- | Collect actual upstream access logs beside the two native command request inventories.
cargoTraffic :: ObservedCargoProxy -> IO CargoTraffic
cargoTraffic proxy = do
    clients <- readTVarIO (observedClients proxy)
    forwards <- readTVarIO (observedForwards proxy)
    upstream <- stubReadsNow (e2ePlane (observedProxy proxy))
    pure (CargoTraffic clients forwards [CargoRequest (T.dropWhileEnd (== '/') (T.drop 8 (stubUrl (srRoute request)))) (srMethod request) (srPath request) (srStatus request) | request <- upstream])

gateway :: Http.Manager -> TVar (Maybe Text) -> TVar [CargoRequest] -> TVar [CargoRequest] -> Wai.Application
gateway manager destination incoming forwarded request respond = do
    target <- readTVarIO destination
    case target of
        Nothing -> respond (Wai.responseLBS status503 [] "product listener is not ready")
        Just base ->
            boundedRead (PublishRequestBodyLimit (16 * 1024 * 1024)) (Wai.getRequestBodyChunk request) >>= \case
                Left _ -> respond (Wai.responseLBS status413 [] "native request exceeds fixture bound")
                Right (_, body) -> do
                    initial <- Http.parseRequest (toString (base <> decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request)))
                    let outgoing =
                            initial
                                { Http.method = Wai.requestMethod request
                                , Http.requestBody = Http.RequestBodyBS body
                                , Http.requestHeaders = filter (\(name, _) -> name `notElem` ["Host", "Content-Length"]) (Wai.requestHeaders request)
                                , Http.decompress = const False
                                , Http.redirectCount = 0
                                }
                    response <- Http.httpLbs outgoing manager
                    let status = statusCode (Http.responseStatus response)
                        path = decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request)
                        incomingHost = decodeUtf8 (fromMaybe "" (lookup "Host" (Wai.requestHeaders request)))
                        received = CargoRequest incomingHost (decodeUtf8 (Wai.requestMethod request)) path status
                        sent = CargoRequest (decodeUtf8 (Http.host outgoing)) (decodeUtf8 (Http.method outgoing)) path status
                    atomically $ do
                        modifyTVar' incoming (<> [received])
                        modifyTVar' forwarded (<> [sent])
                    respond (Wai.responseLBS (Http.responseStatus response) (filter ((/= "Transfer-Encoding") . fst) (Http.responseHeaders response)) (Http.responseBody response))

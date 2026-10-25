-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Observe native TLS traffic and its actual exchanges with the product image.
module Ecluse.E2E.Harness.CargoGateway (
    ObservedCargoProxy (..),
    withObservedCargoProxy,
    cargoTraffic,
) where

import Data.Text qualified as T
import Data.X509.CertificateStore (readCertificateStore)
import Network.Connection (TLSSettings (TLSSettings))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (status413, status503, statusCode)
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.Container
import Ecluse.E2E.Harness.Docker (stubReadsNow, withE2EWith)
import Ecluse.E2E.Harness.Stub (StubRead (..), startedWorkers, stubUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Container.Image (nginxImage)
import Ecluse.Test.Containers (dockerLabelArgs)
import Ecluse.Test.Poll (pollUntil)

-- | TLS access logs and product exchanges keep independent request inventories.
data ObservedCargoProxy = ObservedCargoProxy
    { observedProxy :: E2E
    , observedEdge :: String
    , observedForwards :: TVar [CargoRequest]
    , observedUploads :: TVar [ByteString]
    }

-- | The operator TLS edge advertises HTTPS and retains the ordinary product forwarding path.
withObservedCargoProxy :: E2EConfig -> GlobalDataPlane -> (ObservedCargoProxy -> IO a) -> IO a
withObservedCargoProxy config plane action = do
    destination <- newTVarIO Nothing
    forwarded <- newTVarIO []
    uploads <- newTVarIO []
    manager <- Http.newManager Http.defaultManagerSettings
    withBridgeApplication (gateway manager destination forwarded uploads) $ \port ->
        withCargoTlsEdge plane port $ \edge base -> do
            trusted <- cargoTlsManager plane
            let configured = config{ecExtraEnv = ecExtraEnv config <> [("ECLUSE_SERVER__PUBLIC_URL", base)]}
            withE2EWith
                configured
                ( \proxy -> do
                    atomically (writeTVar destination (Just (e2eBaseUrl proxy)))
                    endpoints <- traverse (maybe (fail "the client endpoint does not belong to the product listener") (pure . (base <>)) . T.stripPrefix (e2eBaseUrl proxy)) (e2eClientEndpoints proxy)
                    let clientProxy = proxy{e2eBaseUrl = base, e2eClientEndpoints = endpoints, e2eManager = trusted}
                    action (ObservedCargoProxy clientProxy edge forwarded uploads)
                )
                plane

withCargoTlsEdge :: GlobalDataPlane -> Int -> (String -> Text -> IO a) -> IO a
withCargoTlsEdge plane port action = do
    suffix <- uniqueSuffix
    labels <- dockerLabelArgs "e2e"
    image <- pinnedExternal nginxImage
    let name = "ecluse-e2e-cargo-edge-" <> suffix
        directory = gdpWorkDir plane </> name
    createDirectoryIfMissing True directory
    writeFileText (directory </> "nginx.conf") (cargoEdgeConfig port)
    let container =
            (dockerRun name (gdpNet plane) image)
                { drPorts = ["127.0.0.1:0:443"]
                , drMounts = [(directory </> "nginx.conf", "/etc/nginx/conf.d/default.conf:ro"), (gdpWorkDir plane </> "certs", "/certs:ro")]
                }
    withDockerContainer labels container $ \_ -> do
        ready <- awaitContainerLog name ((> 0) . startedWorkers) 40
        unless ready (fail "the Cargo TLS edge did not start within the timeout")
        published <- publishedPort name "443/tcp"
        action name ("https://127.0.0.1:" <> show published)

cargoEdgeConfig :: Int -> Text
cargoEdgeConfig port =
    T.unlines
        [ "log_format cargo_client 'cargo-client $scheme $http_host $request_method $status $request_uri';"
        , "server {"
        , "    listen 443 ssl;"
        , "    ssl_certificate /certs/server.crt;"
        , "    ssl_certificate_key /certs/server.key;"
        , "    access_log /dev/stdout cargo_client;"
        , "    client_max_body_size 16m;"
        , "    location / {"
        , "        proxy_set_header Host $http_host;"
        , "        proxy_http_version 1.1;"
        , "        proxy_set_header Connection \"\";"
        , "        proxy_request_buffering off;"
        , "        proxy_buffering off;"
        , "        proxy_pass http://203.0.113.1:" <> show port <> ";"
        , "    }"
        , "}"
        ]

cargoTlsManager :: GlobalDataPlane -> IO Http.Manager
cargoTlsManager plane = do
    store <- readCertificateStore (gdpWorkDir plane </> "certs/bundle.pem") >>= maybe (fail "the Cargo fixture CA bundle is unreadable") pure
    let parameters = TLS.defaultParamsClient "127.0.0.1" ""
        trusted = parameters{TLS.clientShared = (TLS.clientShared parameters){TLS.sharedCAStore = store}}
    Http.newManager (mkManagerSettings (TLSSettings trusted) Nothing)

-- | Collect TLS access logs beside product forwards and actual upstream access logs.
cargoTraffic :: ObservedCargoProxy -> IO CargoTraffic
cargoTraffic proxy = do
    forwards <- readTVarIO (observedForwards proxy)
    clients <- pollUntil 40 25000 ((>= length forwards) . length) (edgeRequests <$> containerLogs (observedEdge proxy))
    unless (length clients == length forwards) (fail "Cargo TLS access logs do not match the product request inventory")
    upstream <- stubReadsNow (e2ePlane (observedProxy proxy))
    pure (CargoTraffic clients forwards [CargoRequest "https" (T.dropWhileEnd (== '/') (T.drop 8 (stubUrl (srRoute request)))) (srMethod request) (srPath request) (srStatus request) | request <- upstream])

edgeRequests :: Text -> [CargoRequest]
edgeRequests = mapMaybe (request . words) . lines
  where
    request = \case
        ["cargo-client", scheme, host, method, status, path] -> CargoRequest scheme host method path <$> readMaybe (toString status)
        _ -> Nothing

gateway :: Http.Manager -> TVar (Maybe Text) -> TVar [CargoRequest] -> TVar [ByteString] -> Wai.Application
gateway manager destination forwarded uploads request respond = do
    target <- readTVarIO destination
    case target of
        Nothing -> respond (Wai.responseLBS status503 [] "product listener is not ready")
        Just base ->
            boundedRead (PublishRequestBodyLimit (16 * 1024 * 1024)) (Wai.getRequestBodyChunk request) >>= \case
                Left _ -> respond (Wai.responseLBS status413 [] "native request exceeds fixture bound")
                Right (_, body) -> do
                    when (Wai.requestMethod request == "PUT") (atomically (modifyTVar' uploads (<> [body])))
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
                        sent = CargoRequest (if Http.secure outgoing then "https" else "http") (decodeUtf8 (Http.host outgoing)) (decodeUtf8 (Http.method outgoing)) path status
                    atomically (modifyTVar' forwarded (<> [sent]))
                    respond (Wai.responseLBS (Http.responseStatus response) (filter ((/= "Transfer-Encoding") . fst) (Http.responseHeaders response)) (Http.responseBody response))

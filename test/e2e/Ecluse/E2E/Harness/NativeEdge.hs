-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A recording TLS edge in front of the product listener, shared by every native-client contract.
module Ecluse.E2E.Harness.NativeEdge (
    ObservedNativeProxy (..),
    withObservedNativeProxy,
    NativeRequest (..),
    NativeTraffic (..),
    nativeTraffic,
    nativeTrafficSince,
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
import Ecluse.E2E.Harness.Container
import Ecluse.E2E.Harness.Docker (stubReadsNow, withE2EWith)
import Ecluse.E2E.Harness.Stub (StubRead (..), startedWorkers, stubUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Container.Image (nginxImage)
import Ecluse.Test.Containers (dockerLabelArgs)
import Ecluse.Test.Poll (pollUntil)

-- | One answered request with its scheme, authority, method, full path and status.
data NativeRequest = NativeRequest
    { nativeRequestScheme :: Text
    , nativeRequestHost :: Text
    , nativeRequestMethod :: Text
    , nativeRequestPath :: Text
    , nativeRequestStatus :: Int
    }
    deriving stock (Eq, Show)

-- | Client, product-forward and upstream inventories, each observed independently.
data NativeTraffic = NativeTraffic
    { nativeClientRequests :: [NativeRequest]
    , nativeProxyRequests :: [NativeRequest]
    , nativeUpstreamRequests :: [NativeRequest]
    }
    deriving stock (Eq, Show)

-- | TLS access logs and product exchanges keep independent request inventories.
data ObservedNativeProxy = ObservedNativeProxy
    { observedProxy :: E2E
    , observedEdge :: String
    , observedForwards :: TVar [NativeRequest]
    , observedUploads :: TVar [ByteString]
    }

-- | The edge advertises HTTPS to native clients and forwards over the ordinary product listener.
withObservedNativeProxy :: E2EConfig -> GlobalDataPlane -> (ObservedNativeProxy -> IO a) -> IO a
withObservedNativeProxy config plane action = do
    destination <- newTVarIO Nothing
    forwarded <- newTVarIO []
    uploads <- newTVarIO []
    manager <- Http.newManager Http.defaultManagerSettings
    withBridgeApplication (gateway manager destination forwarded uploads) $ \port ->
        withTlsEdge plane port $ \edge base -> do
            trusted <- edgeTlsManager plane
            let configured = config{ecExtraEnv = ecExtraEnv config <> [("ECLUSE_SERVER__PUBLIC_URL", base)]}
            withE2EWith
                configured
                ( \proxy -> do
                    atomically (writeTVar destination (Just (e2eBaseUrl proxy)))
                    endpoints <- traverse (maybe (fail "the client endpoint does not belong to the product listener") (pure . (base <>)) . T.stripPrefix (e2eBaseUrl proxy)) (e2eClientEndpoints proxy)
                    let clientProxy = proxy{e2eBaseUrl = base, e2eClientEndpoints = endpoints, e2eManager = trusted}
                    action (ObservedNativeProxy clientProxy edge forwarded uploads)
                )
                plane

-- | Collect the edge's access log beside the product forwards and the stub's upstream reads.
nativeTraffic :: ObservedNativeProxy -> IO NativeTraffic
nativeTraffic proxy = do
    forwards <- readTVarIO (observedForwards proxy)
    clients <- pollUntil 40 25000 ((>= length forwards) . length) (edgeRequests <$> containerLogs (observedEdge proxy))
    unless (length clients == length forwards) (fail "native TLS access logs do not match the product request inventory")
    upstream <- stubReadsNow (e2ePlane (observedProxy proxy))
    pure (NativeTraffic clients forwards [NativeRequest "https" (T.dropWhileEnd (== '/') (T.drop 8 (stubUrl (srRoute request)))) (srMethod request) (srPath request) (srStatus request) | request <- upstream])

-- | The requests one interval added, failing if any append-only inventory shrank.
nativeTrafficSince :: NativeTraffic -> NativeTraffic -> IO NativeTraffic
nativeTrafficSince before after = do
    client <- delta (nativeClientRequests before) (nativeClientRequests after)
    proxy <- delta (nativeProxyRequests before) (nativeProxyRequests after)
    upstream <- delta (nativeUpstreamRequests before) (nativeUpstreamRequests after)
    pure (NativeTraffic client proxy upstream)
  where
    delta previous current = do
        unless (length current >= length previous) (fail "native request inventory reset during an observed interval")
        pure (drop (length previous) current)

withTlsEdge :: GlobalDataPlane -> Int -> (String -> Text -> IO a) -> IO a
withTlsEdge plane port action = do
    suffix <- uniqueSuffix
    labels <- dockerLabelArgs "e2e"
    image <- pinnedExternal nginxImage
    let name = "ecluse-e2e-native-edge-" <> suffix
        directory = gdpWorkDir plane </> name
    createDirectoryIfMissing True directory
    writeFileText (directory </> "nginx.conf") (edgeConfig port)
    let container =
            (dockerRun name (gdpNet plane) image)
                { drPorts = ["127.0.0.1:0:443"]
                , drMounts = [(directory </> "nginx.conf", "/etc/nginx/conf.d/default.conf:ro"), (gdpWorkDir plane </> "certs", "/certs:ro")]
                }
    withDockerContainer labels container $ \_ -> do
        ready <- awaitContainerLog name ((> 0) . startedWorkers) 40
        unless ready (fail "the native TLS edge did not start within the timeout")
        published <- publishedPort name "443/tcp"
        action name ("https://localhost:" <> show published)

edgeConfig :: Int -> Text
edgeConfig port =
    T.unlines
        [ "log_format native_client 'native-client $scheme $http_host $request_method $status $request_uri';"
        , "server {"
        , "    listen 443 ssl;"
        , "    ssl_certificate /certs/server.crt;"
        , "    ssl_certificate_key /certs/server.key;"
        , "    access_log /dev/stdout native_client;"
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

edgeTlsManager :: GlobalDataPlane -> IO Http.Manager
edgeTlsManager plane = do
    store <- readCertificateStore (gdpWorkDir plane </> "certs/bundle.pem") >>= maybe (fail "the native edge CA bundle is unreadable") pure
    let parameters = TLS.defaultParamsClient "localhost" ""
        trusted = parameters{TLS.clientShared = (TLS.clientShared parameters){TLS.sharedCAStore = store}}
    Http.newManager (mkManagerSettings (TLSSettings trusted) Nothing)

edgeRequests :: Text -> [NativeRequest]
edgeRequests = mapMaybe (request . words) . lines
  where
    request = \case
        ["native-client", scheme, host, method, status, path] -> NativeRequest scheme host method path <$> readMaybe (toString status)
        _ -> Nothing

gateway :: Http.Manager -> TVar (Maybe Text) -> TVar [NativeRequest] -> TVar [ByteString] -> Wai.Application
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
                        sent = NativeRequest (if Http.secure outgoing then "https" else "http") (decodeUtf8 (Http.host outgoing)) (decodeUtf8 (Http.method outgoing)) path status
                    atomically (modifyTVar' forwarded (<> [sent]))
                    respond (Wai.responseLBS (Http.responseStatus response) (filter ((/= "Transfer-Encoding") . fst) (Http.responseHeaders response)) (Http.responseBody response))

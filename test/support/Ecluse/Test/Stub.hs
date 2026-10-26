-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback HTTP responses, independent request captures, and fetch faults that carry secrets.
module Ecluse.Test.Stub (
    Captured (..),
    Stub (..),
    stubBaseUrl,
    stubLocalhostUrl,
    lastCaptured,
    allCaptured,
    withStub,
    withStubHeaders,
    withRoutedStub,
    withRoutedStubIO,
    routedApplication,
    stubConfig,
    headerValue,

    -- * Fetches that carry secrets
    redirectSecrets,
    signedRedirect,
    secretAnswer,
    redirectedStatusException,
    requestSecrets,
    closedPort,
    closedAuthority,
    signedRequest,
    refusedFetch,
    failedFetch,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List (lookup)
import Network.HTTP.Client (
    HttpException,
    ManagerSettings,
    Request (checkResponse),
    defaultManagerSettings,
    httpLbs,
    httpNoBody,
    newManager,
    parseRequest,
    throwErrorStatusCodes,
 )
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Header, Status, hLocation, status302)
import Network.Wai (
    Application,
    rawPathInfo,
    requestHeaders,
    requestMethod,
    responseLBS,
    strictRequestBody,
 )
import Network.Wai.Handler.Warp (Port, testWithApplication)
import UnliftIO.Exception (try)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Test.Deadline (untracedClient)
import Ecluse.Test.Wai (localhost)

-- | The method, path, headers and body received at the HTTP boundary.
data Captured = Captured
    { capMethod :: ByteString
    , capPath :: ByteString
    , capHeaders :: [(CI ByteString, ByteString)]
    , capBody :: ByteString
    }
    deriving stock (Eq, Show)

-- | An ephemeral socket and its captures, most recent first.
data Stub = Stub
    { stubPort :: Port
    , stubCaptured :: IORef [Captured]
    }

-- | The base URL of a running stub, by IP literal.
stubBaseUrl :: Stub -> Text
stubBaseUrl stub = "http://127.0.0.1:" <> show (stubPort stub)

-- | A DNS-name URL for cases whose egress guard refuses an IP literal.
stubLocalhostUrl :: Stub -> Text
stubLocalhostUrl stub = localhost (stubPort stub)

-- | The request the stub last captured (or fail loudly if it served none).
lastCaptured :: Stub -> IO Captured
lastCaptured stub =
    readIORef (stubCaptured stub)
        >>= maybe (fail "stub served no request") pure . listToMaybe

-- | Every request the stub captured, in arrival order.
allCaptured :: Stub -> IO [Captured]
allCaptured stub = reverse <$> readIORef (stubCaptured stub)

-- | Record requests and answer with a fixed status and body during the socket lifetime.
withStub :: Status -> LBS.ByteString -> (Stub -> IO a) -> IO a
withStub status = withStubHeaders status []

-- | Add response headers, including content encoding for decompressed-body bounds.
withStubHeaders :: Status -> [Header] -> LBS.ByteString -> (Stub -> IO a) -> IO a
withStubHeaders status extraHeaders body =
    withRoutedStub (const (status, extraHeaders, body))

-- | Choose a response from the captured request after recording it.
withRoutedStub :: (Captured -> (Status, [Header], LBS.ByteString)) -> (Stub -> IO a) -> IO a
withRoutedStub reply = withRoutedStubIO (pure . reply)

-- | Route with mutable server state while retaining the same capture and socket lifetime.
withRoutedStubIO :: (Captured -> IO (Status, [Header], LBS.ByteString)) -> (Stub -> IO a) -> IO a
withRoutedStubIO reply action = do
    captured <- newIORef []
    let recorded cap = do
            atomicModifyIORef' captured (\seen -> (cap : seen, ()))
            reply cap
    testWithApplication (pure (routedApplication recorded)) $ \port ->
        action Stub{stubPort = port, stubCaptured = captured}

-- | Build an application from the same complete request fields recorded by the loopback stubs.
routedApplication :: (Captured -> IO (Status, [Header], LBS.ByteString)) -> Application
routedApplication reply waiReq respond = do
    bodyBytes <- strictRequestBody waiReq
    let cap =
            Captured
                { capMethod = requestMethod waiReq
                , capPath = rawPathInfo waiReq
                , capHeaders = requestHeaders waiReq
                , capBody = LBS.toStrict bodyBytes
                }
    (status, extraHeaders, body) <- reply cap
    respond (responseLBS status extraHeaders body)

-- | Build an anonymous origin using the caller's development HTTP egress witness and swept deadlines.
stubConfig :: Deadlines -> (Text -> RegistryUrl) -> Stub -> IO OriginClient
stubConfig deadlines egressUrl stub = do
    client <- untracedClient <$> newManager defaultManagerSettings <*> pure deadlines
    pure (originClient defaultLimits client (egressUrl (stubBaseUrl stub)) Nothing)

-- | Look up a header (case-insensitively) in a captured request.
headerValue :: ByteString -> Captured -> Maybe ByteString
headerValue name cap = lookup (CI.mk name) (capHeaders cap)

-- | Every secret 'signedRedirect' and 'secretAnswer' plant: a path token, a query signature, a response header, a cookie and a body.
redirectSecrets :: [Text]
redirectSecrets = ["path-secret", "query-secret", "header-secret", "cookie-secret", "body-secret"]

-- | A 'withRoutedStubIO' route: redirect every other path to a signed URL, as a presigned object store does, and answer that URL with @answer@.
signedRedirect :: IO (Status, [Header], LBS.ByteString) -> Captured -> IO (Status, [Header], LBS.ByteString)
signedRedirect answer captured
    | capPath captured == signedPath = answer
    | otherwise = pure (status302, [(hLocation, signedPath <> signedQuery)], "")

-- | An answer that carries a secret in a header, a cookie and its body.
secretAnswer :: Status -> (Status, [Header], LBS.ByteString)
secretAnswer status = (status, [("Set-Cookie", "session=cookie-secret"), ("X-Amz-Request-Id", "header-secret")], "body-secret")

signedPath :: ByteString
signedPath = "/signed/path-secret/all.zip"

signedQuery :: ByteString
signedQuery = "?X-Amz-Signature=query-secret"

-- | A stub's @host:port@, and the exception a status-checked fetch from it throws once its redirect reaches a signed URL answering @status@.
redirectedStatusException :: Status -> IO (Text, HttpException)
redirectedStatusException status =
    withRoutedStubIO (signedRedirect (pure (secretAnswer status))) $ \stub -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest (toString (stubBaseUrl stub) <> "/feed")
        outcome <- try (httpLbs request{checkResponse = throwErrorStatusCodes} manager)
        case outcome of
            Left err -> pure ("127.0.0.1:" <> show (stubPort stub), err)
            Right _ -> fail ("the signed URL unexpectedly answered " <> show status)

-- | Every secret a planted request carries: a path token, a query signature and a session-token header.
requestSecrets :: [Text]
requestSecrets = ["path-secret", "query-secret", "token-secret"]

-- | A loopback port where nothing listens.
closedPort :: Int
closedPort = 1

-- | The @host:port@ of 'closedPort', as a fault renders it.
closedAuthority :: Text
closedAuthority = "127.0.0.1:" <> show closedPort

-- | A request for the signed URL at 'closedAuthority', with the session-token header an AWS signer adds.
signedRequest :: IO Request
signedRequest = do
    request <- parseRequest ("http://" <> toString closedAuthority <> decodeUtf8 (signedPath <> signedQuery))
    pure request{Client.requestHeaders = [("X-Amz-Security-Token", "token-secret")]}

-- | The exception a fetch throws when nothing listens at the request's port.
refusedFetch :: Request -> IO HttpException
refusedFetch = failedFetch defaultManagerSettings

-- | The exception a fetch through a manager with these settings throws, failing the test if it is answered.
failedFetch :: ManagerSettings -> Request -> IO HttpException
failedFetch settings request = do
    manager <- newManager settings
    outcome <- try (httpNoBody request manager)
    case outcome of
        Left err -> pure err
        Right _ -> fail "a fetch expected to fail was answered"

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback HTTP responses and independent request captures.
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
    stubConfig,
    headerValue,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List (lookup)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Status)
import Network.Wai (
    Application,
    rawPathInfo,
    requestHeaders,
    requestMethod,
    responseLBS,
    strictRequestBody,
 )
import Network.Wai.Handler.Warp (Port, testWithApplication)

import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
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
    let app :: Application
        app waiReq respond = do
            bodyBytes <- strictRequestBody waiReq
            let cap =
                    Captured
                        { capMethod = requestMethod waiReq
                        , capPath = rawPathInfo waiReq
                        , capHeaders = requestHeaders waiReq
                        , capBody = LBS.toStrict bodyBytes
                        }
            atomicModifyIORef' captured (\seen -> (cap : seen, ()))
            (status, extraHeaders, body) <- reply cap
            respond (responseLBS status extraHeaders body)
    testWithApplication (pure app) $ \port ->
        action Stub{stubPort = port, stubCaptured = captured}

-- | Build an anonymous origin using the caller's development HTTP egress witness.
stubConfig :: (Text -> RegistryUrl) -> Stub -> IO OriginClient
stubConfig egressUrl stub = do
    manager <- newManager defaultManagerSettings
    pure (originClient defaultLimits manager (egressUrl (stubBaseUrl stub)) Nothing)

-- | Look up a header (case-insensitively) in a captured request.
headerValue :: ByteString -> Captured -> Maybe ByteString
headerValue name cap = lookup (CI.mk name) (capHeaders cap)

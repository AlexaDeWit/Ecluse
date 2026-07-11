-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Stub (
    Captured (..),
    Stub (..),
    stubBaseUrl,
    lastCaptured,
    withStub,
    withStubHeaders,
    stubConfig,
    headerValue,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
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

import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Security (defaultLimits)

{- | What the stub captured from the request it last served: enough to assert the
method, path, and headers Écluse sent.
-}
data Captured = Captured
    { capMethod :: ByteString
    , capPath :: ByteString
    , capHeaders :: [(CI ByteString, ByteString)]
    , capBody :: ByteString
    }
    deriving stock (Eq, Show)

{- | A running stub: the ephemeral 'Port' it listens on and the slot holding the
most recent 'Captured' request (the handle talks to @127.0.0.1:port@).
-}
data Stub = Stub
    { stubPort :: Port
    , stubCaptured :: IORef (Maybe Captured)
    }

-- | The base URL of a running stub.
stubBaseUrl :: Stub -> Text
stubBaseUrl stub = "http://127.0.0.1:" <> show (stubPort stub)

-- | The request the stub last captured (or fail loudly if it served none).
lastCaptured :: Stub -> IO Captured
lastCaptured stub =
    readIORef (stubCaptured stub)
        >>= maybe (fail "stub served no request") pure

{- | Run an action against a stub that records each request and answers every one
with a fixed status and body. @testWithApplication@ binds a free port for the
action's duration, so the test never collides on a fixed port.
-}
withStub :: Status -> LBS.ByteString -> (Stub -> IO a) -> IO a
withStub status = withStubHeaders status []

{- | 'withStub' with extra response headers -- e.g. @Content-Encoding: gzip@ so the
@http-client@ body reader decompresses the served bytes, letting a test assert the
bounded read bounds /decompressed/ size rather than wire size.
-}
withStubHeaders :: Status -> [Header] -> LBS.ByteString -> (Stub -> IO a) -> IO a
withStubHeaders status extraHeaders body action = do
    captured <- newIORef Nothing
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
            atomicModifyIORef' captured (const (Just cap, ()))
            respond (responseLBS status extraHeaders body)
    testWithApplication (pure app) $ \port ->
        action Stub{stubPort = port, stubCaptured = captured}

-- | A config pointed at a stub, anonymous, sharing a no-TLS manager.
stubConfig :: Stub -> IO NpmClientConfig
stubConfig stub = do
    manager <- newManager defaultManagerSettings
    pure
        NpmClientConfig
            { npmBaseUrl = stubBaseUrl stub
            , npmManager = manager
            , npmToken = Nothing
            , npmLimits = defaultLimits
            }

-- | Look up a header (case-insensitively) in a captured request.
headerValue :: ByteString -> Captured -> Maybe ByteString
headerValue name cap = snd <$> find ((== CI.mk name) . fst) (capHeaders cap)

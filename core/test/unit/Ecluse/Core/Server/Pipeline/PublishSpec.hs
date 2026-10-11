-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Publish authentication and byte admission precede pathless identity checks and relay.
module Ecluse.Core.Server.Pipeline.PublishSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, status201, statusCode)
import Network.Wai (Application, Request (requestBodyLength, requestHeaders), RequestBodyLength (..), Response, defaultRequest, getRequestBodyChunk, responseLBS, setRequestBodyChunks)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), runSession, srequest)
import Test.Hspec

import Ecluse.Core.Credential (ClientCredential (..), mkSecret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (..), PublishBody (..))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterProjectName))
import Ecluse.Core.Registry.Npm.Adapter (npmPublish)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken))
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Multipart qualified as Multipart
import Ecluse.Core.Registry.PyPI.Route (pypiRouter)
import Ecluse.Core.Security (Limits (maxPublishRequestBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmissionTuned, withByteAdmission)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (MountBinding (..), PublishDeps (..), RequestCtx (RequestCtx), ServeRuntime (ServeRuntime), runHandler)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (requestsName)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.PyPI.Multipart (multipartContentType, multipartField, multipartUpload)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (inertDepsFor)
import Ecluse.Test.Support (testMemoryMeter, testServeAdmission)

-- | Pin the public contract through deterministic inputs.
spec :: Spec
spec = describe "servePublish" $ do
    it "uses route-owned methods without publish dependencies" $ do
        fixture <- newFixture Nothing
        response <- send (fixture{fixtureEnabled = False}) "" []
        statusCode (simpleStatus response) `shouldBe` 405
        lookup "Allow" (simpleHeaders response) `shouldBe` Just "POST"
        readIORef (fixtureReads fixture) `shouldReturn` 0
    it "authenticates before reading a pathless body" $ do
        fixture <- newFixture Nothing
        response <- send fixture (upload ["requests"]) []
        statusCode (simpleStatus response) `shouldBe` 401
        readIORef (fixtureReads fixture) `shouldReturn` 0
    it "refuses a declared oversized body before reading" $ do
        fixture <- newFixture Nothing
        response <- send (withCap 1 fixture) (upload ["requests"]) authenticated
        statusCode (simpleStatus response) `shouldBe` 413
        readIORef (fixtureReads fixture) `shouldReturn` 0
    it "bounds a chunked body before decoding names or relay" $ do
        fixture <- newFixture Nothing
        response <- send ((withCap 1 fixture){fixtureLength = ChunkedBody}) (upload ["requests"]) authenticated
        statusCode (simpleStatus response) `shouldBe` 413
        readIORef (fixtureRelays fixture) `shouldReturn` []
    it "sheds before reading when the aggregate byte budget is held" $ do
        fixture <- newFixture Nothing
        result <- withByteAdmission noopMetricsPort (pubBodyBudget (fixtureDeps fixture)) requestCap $ do
            response <- send fixture (upload ["requests"]) authenticated
            statusCode (simpleStatus response) `shouldBe` 503
        result `shouldBe` Just ()
        readIORef (fixtureReads fixture) `shouldReturn` 0
    it "retains the path-name scope refusal before the request-size check" $ do
        fixture <- newFixture (Just requestsName)
        let deps = (fixtureDeps fixture){pubAllowed = const False}
        response <- send (withCap 1 (fixture{fixtureDeps = deps})) (upload ["requests"]) authenticated
        statusCode (simpleStatus response) `shouldBe` 403
        readIORef (fixtureReads fixture) `shouldReturn` 0
    it "preserves a path route's absent body claims" $ do
        fixture <- newFixture (Just requestsName)
        response <- send fixture "" authenticated
        simpleStatus response `shouldBe` status201
    it "reads a pathless out-of-scope upload bounded before refusing it" $ do
        fixture <- newFixture Nothing
        response <- send fixture (upload ["public-project"]) authenticated
        statusCode (simpleStatus response) `shouldBe` 403
        readCount <- readIORef (fixtureReads fixture)
        readCount `shouldSatisfy` (> 0)
        readIORef (fixtureRelays fixture) `shouldReturn` []
    for_ [[], [""], ["requests", "public-project"], ["requests", ""]] $ \names ->
        it ("refuses empty or disagreeing names: " <> show names) $ do
            fixture <- newFixture Nothing
            response <- send fixture (upload names) authenticated
            statusCode (simpleStatus response) `shouldBe` 403
            readIORef (fixtureRelays fixture) `shouldReturn` []
    it "refuses multipart bytes without their declared Content-Type" $ do
        fixture <- newFixture Nothing
        response <- send fixture (upload ["requests"]) [("Authorization", "Basic Y2FsbGVyOnRva2Vu")]
        statusCode (simpleStatus response) `shouldBe` 403
        readIORef (fixtureRelays fixture) `shouldReturn` []
    it "canonicalises repeated names and preserves bytes, Content-Type and the caller pair" $ do
        fixture <- newFixture Nothing
        let body = upload ["Requests", "requests"]
        response <- send fixture body authenticated
        simpleStatus response `shouldBe` status201
        simpleBody response `shouldBe` "native response"
        relays <- readIORef (fixtureRelays fixture)
        relays `shouldBe` [(requestsName, Just (ClientCredential (Just "caller") (mkSecret "token")), PublishBody (Just multipartContentType) body)]

requestCap :: Int
requestCap = 65536

authenticated :: [Header]
authenticated = [("Authorization", "Basic Y2FsbGVyOnRva2Vu"), ("Content-Type", multipartContentType)]

upload :: [ByteString] -> ByteString
upload names = publishBytes (multipartUpload (map (multipartField "name") names))

data Fixture = Fixture
    { fixtureDeps :: PublishDeps
    , fixtureName :: Maybe PackageName
    , fixtureEnabled :: Bool
    , fixtureLength :: RequestBodyLength
    , fixtureReads :: IORef Int
    , fixtureRelays :: IORef [(PackageName, Maybe ClientCredential, PublishBody)]
    }

newFixture :: Maybe PackageName -> IO Fixture
newFixture name = do
    budget <- newByteAdmissionTuned requestCap 0 0
    readCount <- newIORef 0
    relays <- newIORef []
    let deps =
            PublishDeps
                { pubTargetUrl = loopbackRegistryUrl "http://unused.test"
                , pubAllowed = (== requestsName)
                , pubStaticToken = Nothing
                , pubInboundToken = Just (mkSecret "token")
                , pubLimits = defaultLimits{maxPublishRequestBytes = requestCap}
                , pubBodyBudget = budget
                , pubHelp = Nothing
                , pubProjectName = adapterProjectName pypiAdapter
                , pubAdapter =
                    npmPublish
                        { publishDeclaredNames = Multipart.declaredNames
                        , publishRelay = \origin agreed body -> do
                            modifyIORef' relays (<> [(agreed, ocToken origin, body)])
                            pure (Right (PublishRelayResponse 201 "native response"))
                        }
                }
    pure (Fixture deps name True (KnownLength 0) readCount relays)

withCap :: Int -> Fixture -> Fixture
withCap cap fixture = fixture{fixtureDeps = (fixtureDeps fixture){pubLimits = defaultLimits{maxPublishRequestBytes = cap}}}

send :: Fixture -> ByteString -> [Header] -> IO SResponse
send fixture bytes headers = do
    app <- fixtureApp fixture
    let len = case fixtureLength fixture of
            ChunkedBody -> ChunkedBody
            KnownLength _ -> KnownLength (fromIntegral (BS.length bytes))
    runSession (srequest (SRequest defaultRequest{requestHeaders = headers, requestBodyLength = len} (LBS.fromStrict bytes))) app

fixtureApp :: Fixture -> IO Application
fixtureApp fixture = do
    manager <- newManager defaultManagerSettings
    admission <- testServeAdmission
    meter <- testMemoryMeter
    cache <- newMetadataCache defaultCacheConfig
    queue <- newTestMemoryQueue
    logEnv <- newTestLogEnv
    let runtime = ServeRuntime admission meter manager manager cache queue noopMetricsPort passthroughTracingPort
        mount = MountBinding ("pypi" :| []) pypiRouter pypiCredential (inertDepsFor pypiAdapter) (if fixtureEnabled fixture then Just (fixtureDeps fixture) else Nothing)
    pure $ \request respond -> do
        let counted = setRequestBodyChunks (modifyIORef' (fixtureReads fixture) (+ 1) >> getRequestBodyChunk request) request
        runHandler logEnv mempty (RequestCtx runtime mount) (servePublish replies (fixtureName fixture) counted respond)

replies :: PublishReplies Response
replies = PublishReplies "POST" responseLBS (\status headers -> responseLBS status headers . LBS.fromStrict . encodeUtf8)

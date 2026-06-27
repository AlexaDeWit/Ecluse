module Ecluse.Server.PublishSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, hAuthorization, hContentType, methodPut, mkStatus, statusCode)
import Network.Wai (
    Application,
    Request (requestBodyLength, requestHeaders, requestMethod),
    RequestBodyLength (ChunkedBody),
    consumeRequestBodyStrict,
    responseLBS,
 )
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (
    SRequest (SRequest),
    SResponse (simpleBody, simpleStatus),
    defaultRequest,
    runSession,
    setPath,
    srequest,
 )
import Test.Hspec
import UnliftIO (throwString)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, mkSecret, staticProvider)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PublishDeps (..))
import Ecluse.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Telemetry (telemetryDisabled)

-- ── the publication-target double ─────────────────────────────────────────────

{- | An in-process publication-target double: it records the @Authorization@ header
and the body of every @PUT@ it receives (so the credential-passthrough and
body-relay invariants are assertable), and answers with a fixed status and body so the
relay-back-to-client path is assertable. The single double stands in for the
operator's first-party registry (Verdaccio / CodeArtifact / Artifact Registry).
-}
data Target = Target
    { tgApp :: Application
    , tgSeen :: IORef [(Maybe ByteString, ByteString)]
    }

-- | A publication-target double answering every publish with @code@ and @body@.
newTarget :: Int -> LByteString -> IO Target
newTarget code body = do
    seen <- newIORef []
    let app req respond = do
            received <- consumeRequestBodyStrict req
            modifyIORef' seen ((authHeader (requestHeaders req), LBS.toStrict received) :)
            respond (responseLBS (mkStatus code "OK") [(hContentType, "application/json")] body)
    pure (Target app seen)

{- | Host a publication-target double (answering @code@/@body@) on an ephemeral port,
handing the continuation the port (to point the proxy at) and the 'Target' (to inspect
what it saw) — the in-process integration harness, no Docker required.
-}
withTarget :: Int -> LByteString -> (Int -> Target -> IO a) -> IO a
withTarget code body k = do
    target <- newTarget code body
    testWithApplication (pure (tgApp target)) (`k` target)

-- The @Authorization@ header a request carried, if any.
authHeader :: [Header] -> Maybe ByteString
authHeader headers = snd <$> find ((== hAuthorization) . fst) headers

-- The (auth, body) pairs the target saw, in arrival order.
targetSaw :: Target -> IO [(Maybe ByteString, ByteString)]
targetSaw target = reverse <$> readIORef (tgSeen target)

-- ── proxy assembly ────────────────────────────────────────────────────────────

{- | A registry-handle double whose effectful fields refuse loudly: the publish path
talks to the publication target via the npm client over the shared 'Manager', not the
handle, so a handle that throws proves the publish never routes through it.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const (throwString "publish must not fetchMetadata")
        , fetchArtifact = \_ _ -> throwString "publish must not fetchArtifact"
        , publishArtifact = \_ _ _ -> throwString "publish must not use the handle publishArtifact"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }

fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "unused", authExpiresAt = Nothing}

newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The first-party publish dependencies for the tests: a @\@acme@ publish-scope
allow-list, the publication target at the given loopback port, and the given static
fallback credential (used only when a client sends none). The default model is
passthrough — the client's own token — so 'pubStaticToken' is usually 'Nothing'.
-}
publishDepsAt :: Int -> Maybe Secret -> PublishDeps
publishDepsAt targetPort staticToken =
    PublishDeps
        { pubTargetUrl = "http://127.0.0.1:" <> show targetPort
        , pubScopes = [mkScope "acme"]
        , pubStaticToken = staticToken
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits
        , pubHelp = Nothing
        }

{- | A proxy 'Application' over a single @\/npm@ mount carrying the given publish deps
('Nothing' leaves the publish path off — a @405@).
-}
proxyWith :: Maybe PublishDeps -> IO Application
proxyWith publishDeps = do
    env <- newTestEnv
    let cfg =
            mkServerConfig
                [ MountBinding
                    { bindingPrefix = "npm" :| []
                    , bindingClassifier = Npm.classify
                    , bindingPackumentDeps = Nothing
                    , bindingPublishDeps = publishDeps
                    , bindingRenderer = npmRenderer
                    }
                ]
    pure (application cfg env)

-- ── driving a publish ─────────────────────────────────────────────────────────

-- | A @PUT \/npm\/{path}@ publish carrying the given bearer (if any) and body.
putPublish :: ByteString -> Maybe Text -> LByteString -> Application -> IO SResponse
putPublish path bearer body =
    runSession (srequest (SRequest req body))
  where
    req =
        (setPath defaultRequest{requestMethod = methodPut, requestHeaders = auth} path)
            { requestBodyLength = ChunkedBody
            }
    auth = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer

status :: SResponse -> Int
status = statusCode . simpleStatus

-- A representative npm publish document body (its shape is irrelevant to the relay,
-- which forwards the bytes verbatim).
publishBody :: LByteString
publishBody = "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{}}"

spec :: Spec
spec = describe "first-party publish path → publication target (S52)" $ do
    it "relays an in-scope publish with the publisher's forwarded credential and returns the target's response" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            -- the publication target's own success status and body are relayed back
            status resp `shouldBe` 201
            simpleBody resp `shouldBe` "{\"success\":true}"
            -- the target saw the publisher's OWN token (passthrough), and the body verbatim
            seen <- targetSaw target
            seen `shouldBe` [(Just "Bearer publisher-token", LBS.toStrict publishBody)]

    it "forwards the static PUBLICATION_TARGET_TOKEN only when the client sends no token of its own" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort (Just (mkSecret "fallback-token"))))
            resp <- putPublish "/npm/@acme/widget" Nothing publishBody app
            status resp `shouldBe` 201
            seen <- targetSaw target
            -- no client token, so the configured static fallback is forwarded
            map fst seen `shouldBe` [Just "Bearer fallback-token"]

    it "refuses an out-of-scope publish with 403 BEFORE any upstream write (anti-shadowing guard)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@other/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            -- the guard fired before the relay: the publication target was never contacted
            targetSaw target `shouldReturn` []

    it "refuses an unscoped publish with 403 (an unscoped name is within no scope)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it "405s a publish when no publication target is configured (the opt-in is off)" $
        withTarget 201 "{\"success\":true}" $ \_targetPort target -> do
            app <- proxyWith Nothing
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 405
            targetSaw target `shouldReturn` []

    it "relays the publication target's own error status (e.g. a 409 the registry returns) to the client" $
        withTarget 409 "{\"error\":\"version already exists\"}" $ \targetPort _target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            -- a first-party publisher sees the registry's real 409, not a fabricated success
            status resp `shouldBe` 409
            simpleBody resp `shouldBe` "{\"error\":\"version already exists\"}"

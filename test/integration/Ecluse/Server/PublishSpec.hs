-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
import UnliftIO (throwIO)

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..), relayPublishDocument)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (PublishDeps (..))
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)

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
what it saw) -- the in-process integration harness, no Docker required.
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

{- | The typed trap the handle double throws when the publish path wrongly routes
through a handle field this suite proves it never uses.
-}
newtype PublishPathViolation = PublishPathViolation Text
    deriving stock (Eq, Show)

instance Exception PublishPathViolation

{- | A registry-handle double whose effectful fields refuse loudly: the publish path
talks to the publication target via the npm client over the shared 'Manager', not the
handle, so a handle that throws proves the publish never routes through it.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const (throwIO (PublishPathViolation "publish must not fetchMetadata"))
        , publishArtifact = \_ _ _ _ -> throwIO (PublishPathViolation "publish must not use the handle publishArtifact")
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }

newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission fakeRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The first-party publish dependencies for the tests: a @\@acme@ publish-scope
allow-list, the publication target at the given loopback port, and the given static
fallback credential (used only when a client sends none). The default model is
passthrough -- the client's own token -- so 'pubStaticToken' is usually 'Nothing'.
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
        , pubRelayPublish = \l m t s -> relayPublishDocument (NpmClientConfig t m s l)
        , pubCanonicaliseName = rightToMaybe . Project.projectName
        }

{- | A proxy 'Application' over a single @\/npm@ mount carrying the given publish deps
('Nothing' leaves the publish path off -- a @405@).
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

-- A representative npm publish document body whose declared identity (@_id@,
-- top-level @name@) agrees with the @\@acme\/widget@ URL the tests publish to.
publishBody :: LByteString
publishBody = "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{}}"

-- A populated single-version publish whose body identity -- its @_id@, top-level
-- @name@, and the one @versions[].name@ -- all agree with the @\@acme\/widget@ URL: a
-- legitimate npm client's shape, which the body-name agreement check must still relay.
matchingVersionBody :: LByteString
matchingVersionBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@acme/widget\",\"version\":\"1.0.0\"}}}"

-- Publish documents whose declared body identity disagrees with the in-scope URL name
-- @\@acme\/widget@ on exactly one field -- the anti-shadowing bypass of issue #391: a
-- crafted body names a package the scope guard never authorised.
mismatchedIdBody :: LByteString
mismatchedIdBody =
    "{\"_id\":\"@victim/target\",\"name\":\"@acme/widget\",\"versions\":{}}"

mismatchedNameBody :: LByteString
mismatchedNameBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@victim/target\",\"versions\":{}}"

mismatchedVersionNameBody :: LByteString
mismatchedVersionNameBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@victim/target\",\"version\":\"1.0.0\"}}}"

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

    it "forwards the static ECLUSE_PUBLICATION_TARGET_TOKEN only when the client sends no token of its own" $
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

    it "refuses a scope that only prefixes an allowed one (@acme-evil vs the allowed @acme) -- exact match" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            -- The guard compares scopes exactly, so a look-alike scope is not admitted by
            -- prefix; the publication target is never contacted.
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme-evil/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it "sends NO Authorization header to the target for a fully anonymous in-scope publish (no client token, no static fallback)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" Nothing publishBody app
            status resp `shouldBe` 201
            -- passthrough with no client token and no static fallback ⇒ the relay carries
            -- no credential at all
            seen <- targetSaw target
            map fst seen `shouldBe` [Nothing]

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

    -- The body-name agreement leg of the anti-shadowing guard (issue #391): the URL path
    -- is in-scope and passes 'inPublishScope', but the document body declares a DIFFERENT
    -- package name, so the relay would publish a name the scope guard never authorised.
    -- Each present declared name -- @_id@, top-level @name@, and @versions[].name@ -- is
    -- checked, and a disagreement is a 403 before any upstream write.
    it "refuses a publish whose body _id disagrees with the in-scope URL name (403 before any relay)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedIdBody app
            status resp `shouldBe` 403
            -- the agreement check fired before the relay: the target was never contacted
            targetSaw target `shouldReturn` []

    it "refuses a publish whose body top-level name disagrees with the in-scope URL name (403 before any relay)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it "refuses a publish whose body versions[].name disagrees with the in-scope URL name (403 before any relay)" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedVersionNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it "relays an in-scope publish whose body _id / name / versions[].name all agree with the URL name" $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") matchingVersionBody app
            -- a body whose every declared name matches the URL still relays (no over-refusal)
            status resp `shouldBe` 201
            seen <- targetSaw target
            map fst seen `shouldBe` [Just "Bearer publisher-token"]

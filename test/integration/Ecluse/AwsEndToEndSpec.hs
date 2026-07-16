-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.AwsEndToEndSpec (spec) where

import Crypto.Hash (Digest, SHA1, SHA512, hashlazy)
import Data.Aeson (Value (Object), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status201, status404, statusCode)
import Network.HTTP.Types.Header (hHost)
import Network.Wai (Application, Request (rawPathInfo), requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (simpleBody, simpleStatus), defaultRequest, request, runSession, setPath)
import Test.Hspec
import TestContainers (Container)
import UnliftIO (race_, timeout)
import UnliftIO.Concurrent (threadDelay)

import Ecluse (runWorker)
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), planMirrorQueue)
import Ecluse.Config (Config (configApp), loadConfig)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (HashAlg (SRI))
import Ecluse.Core.Package.Merge (DivergencePolicy (Warn))
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Registry.Publish (MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken), newMirrorPublish)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, tarballHostGate)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Integration.Ministack (
    endpointFor,
    freshQueueUrl,
    withMinistack,
 )
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsWaitSeconds), SqsEndpoint (endpointHost, endpointPort), newSqsQueue)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity, unsafeHash)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)
import Ecluse.Test.Worker (admitAllPolicies)

{- | The whole AWS-backed path through the __real composition root__, end to end: an
in-process Écluse -- the real 'Ecluse.Server.application' serve path and the real
mirror worker ('Ecluse.runWorker') -- over a __real SQS queue__ (a @ministack@ container,
shared through "Ecluse.Integration.Ministack") and WAI npm stubs for the public
upstream, the (missing) private upstream, and the mirror target.

Two flows are exercised against that one wiring:

* a __packument__ request is filtered by the rules (a too-recent version is denied
  by the @min-age@ quarantine and never appears in the served document);
* a __tarball__ request on a private-upstream miss is gated, streamed from the public
  upstream, and __enqueues a real SQS mirror job__; the worker then long-polls that
  queue, fetches the artifact, __verifies it against the re-admitted current-metadata
  digest__, and publishes it to the mirror target -- the demand-driven
  fetch → verify → publish back-fill, across a genuine SQS round-trip.

Hermetic and gating, but requires a Docker daemon (for @ministack@) and no real AWS.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "AWS-backed Écluse end to end (ministack SQS + WAI npm stubs)" $ do
            it "filters public versions by the rules on a packument request" $ \container ->
                withAwsProxy container "aws-e2e-packument" $ \proxy -> do
                    resp <- getThrough (tpApp proxy) "/npm/left-pad"
                    status resp `shouldBe` 200
                    -- The 7-day quarantine admits the 2020 version and denies the
                    -- 2-day-old one, so only the survivor is in the served document.
                    packumentVersions (simpleBody resp) `shouldMatchList` ["1.0.0"]

            it "gates a tarball, enqueues a real SQS job, and the worker mirrors it (fetch → verify → publish)" $ \container ->
                withAwsProxy container "aws-e2e-tarball" $ \proxy -> do
                    -- The tarball request misses the private upstream, is gated and
                    -- streamed from the public upstream, and enqueues a mirror job to
                    -- the real SQS queue before the response returns.
                    resp <- getThrough (tpApp proxy) "/npm/left-pad/-/left-pad-1.0.0.tgz"
                    status resp `shouldBe` 200
                    simpleBody resp `shouldBe` tarballBytes
                    -- The worker long-polls the same SQS queue, fetches the artifact,
                    -- verifies it against the re-admitted digest, and publishes it.
                    runLoopUntil (tpPolicies proxy) (tpEnv proxy) (publishedAtLeast (tpMirrorLog proxy) 1)
                    published <- readIORef (tpMirrorLog proxy)
                    length published `shouldSatisfy` (>= 1)
                    published `shouldSatisfy` all (== "/left-pad")

{- | The in-process Écluse under test: the serve 'Application', the composition-root
'Env' the worker runs over, and the mirror-target stub's publish log.
-}
data TestProxy = TestProxy
    { tpApp :: Application
    , tpEnv :: Env
    , tpPolicies :: WorkerPolicies
    , tpMirrorLog :: IORef [ByteString]
    }

{- Stand up the three WAI stubs (public upstream, missing private upstream, mirror
target), a fresh real SQS queue in the container, and the composition-root 'Env' and
serve 'Application' over them, then run the body against the assembled proxy.

The queue is built through the __config-driven composition root__
('Ecluse.Composition.planMirrorQueue' → 'Ecluse.Core.Queue.Sqs.newSqsQueue'), driven by the
AWS-SDK-standard @AWS_ENDPOINT_URL_SQS@ override pointed at the container -- the same
production path the released image runs, with no test-only code path. -}
withAwsProxy :: Container -> Text -> (TestProxy -> IO a) -> IO a
withAwsProxy container queueName body =
    withPrivateUpstream $ \privateUrl ->
        withPublicUpstream $ \publicUrl ->
            withMirrorTarget $ \mirrorUrl mirrorLog -> do
                queue <- configDrivenQueue container queueName
                env <- buildEnv queue
                policies <- workerPoliciesAt mirrorUrl
                binding <- mountBinding privateUrl publicUrl mirrorUrl
                let app = application (mkServerConfig [binding]) env
                body TestProxy{tpApp = app, tpEnv = env, tpPolicies = policies, tpMirrorLog = mirrorLog}

{- Build the SQS-backed mirror queue through the production composition root: create a
queue in the container, then resolve the backend from an environment layer carrying the
AWS-SDK-standard @AWS_ENDPOINT_URL_SQS@ override (and the standard credential keys an
emulator needs), exactly as the released image would. A short long-poll keeps the worker
loop brisk. -}
configDrivenQueue :: Container -> Text -> IO MirrorQueue
configDrivenQueue container queueName = do
    queueUrl <- freshQueueUrl container queueName
    let endpoint = endpointFor container
        endpointUrl = "http://" <> endpointHost endpoint <> ":" <> show (endpointPort endpoint)
    env <- either (fail . ("AwsEndToEndSpec fixture env: " <>) . show) (pure . configApp) (loadConfig (sqsEnvVars queueUrl endpointUrl) Nothing)
    plan <- either (fail . toString . T.unlines . map renderBootError) pure (planMirrorQueue env)
    logEnv <- newTestLogEnv
    case plan of
        -- The wire decode's egress former: the loopback dev former, since this
        -- suite's artifact URLs are in-process http servers.
        SqsBackend sqsConfig -> newSqsQueue logEnv (Right . loopbackRegistryUrl) sqsConfig{sqsWaitSeconds = 1}
        MemoryBackend _ -> fail "AwsEndToEndSpec fixture: expected the SQS backend, got the in-memory one"

-- The environment layer the released image would run with to target a ministack SQS:
-- the standard endpoint override and credential keys, plus the required upstreams.
sqsEnvVars :: Text -> Text -> [(String, String)]
sqsEnvVars queueUrl endpointUrl =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "test-token")
    , ("ECLUSE_QUEUE_URL", toString queueUrl)
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ENDPOINT_URL_SQS", toString endpointUrl)
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

-- The composition-root 'Env' over the real SQS queue. The guarded data-plane
-- manager opts loopback in so the in-process upstream/artifact fetches reach the
-- WAI stubs.
buildEnv :: MirrorQueue -> IO Env
buildEnv queue = do
    guardedManager <- newManager defaultManagerSettings
    trusted <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue guardedManager trusted metadataCache logEnv telemetryDisabled heartbeat

{- The worker's admit-everything bundles publishing through the production marriage
(npm's codec over the shared transport) at the mirror-target stub, with a static
test bearer -- the same construction the composition root performs. The resolver
carries the true digest of the bytes the public stub serves, so verification
passes and the pipeline publishes. -}
workerPoliciesAt :: Text -> IO WorkerPolicies
workerPoliciesAt mirrorUrl = do
    trusted <- newManager defaultManagerSettings
    let transport =
            MirrorTransport
                { ptManager = trusted
                , ptMintToken = pure (Just (mkSecret "e2e-publish-token"))
                , ptLimits = defaultLimits
                }
    pure (admitAllPolicies (newMirrorPublish transport mirrorUrl npmPublishCodec) (unsafeHash SRI sha512Integrity :| []))

-- The single npm mount: the public origin is the loopback upstream stub, the private
-- origin is the 404 stub (so every request misses to public), and the mirror target is
-- the publish stub. The fixed clock and the week-long quarantine make the rule gate
-- deterministic.
mountBinding :: Text -> Text -> Text -> IO MountBinding
mountBinding privateUrl publicUrl mirrorUrl = do
    prepared <- prepare inertRuleDeps admitOldEnough
    let deps =
            PackumentDeps
                { pdPrivateBaseUrl = privateUrl
                , pdPublicBaseUrl = publicUrl
                , pdMountBaseUrl = "https://proxy.test/npm"
                , pdMirrorTarget = mirrorUrl
                , pdRules = prepared
                , pdTarballHostPolicy = SameHostAsPackument
                , pdAdditionalBlockedRanges = []
                , pdTarballHostGate = tarballHostGate privateUrl publicUrl mirrorUrl
                , pdLimits = defaultLimits
                , pdInboundToken = Nothing
                , pdNow = pure fixedNow
                , pdAdvisoryEtag = pure Nothing
                , pdHelp = Nothing
                , pdMinIntegrity = defaultMinIntegrity
                , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
                , pdDivergencePolicy = Warn
                , pdNewMetadataClient = \t p u c f1 f2 f3 l m b s -> newNpmMetadataClient t p u c f1 f2 f3 (NpmClientConfig b m s l)
                , pdBuildArtifactRequestByFile = \_ _ t s -> artifactRequestByFile t s
                , pdBuildArtifactRequestByUrl = \_ _ t s -> artifactRequestByUrl t s
                , pdAssemble = assembleMergedPackument
                , pdEgressUrl = Right . loopbackRegistryUrl
                }
    pure
        MountBinding
            { bindingPrefix = "npm" :| []
            , bindingRouter = npmRouter
            , bindingPackumentDeps = deps
            , bindingPublishDeps = Nothing
            , bindingRenderer = npmRenderer
            }

{- The public upstream: it answers any @.tgz@ path with the artifact bytes and every
other path with the two-version packument, whose @dist.tarball@ names this same
loopback host and port (learned from the request's @Host@ header) so the honoured
location is reachable. -}
withPublicUpstream :: (Text -> IO a) -> IO a
withPublicUpstream k = testWithApplication (pure app) (k . loopbackUrl)
  where
    app :: Application
    app req respond =
        respond $
            if ".tgz" `BS.isSuffixOf` rawPathInfo req
                then responseLBS status200 [] tarballBytes
                else responseLBS status200 [] (encode (packument (selfPort req)))

-- The private upstream: a clean 404 miss for everything, so every request falls
-- through to the public origin.
withPrivateUpstream :: (Text -> IO a) -> IO a
withPrivateUpstream k = testWithApplication (pure app) (k . loopbackUrl)
  where
    app :: Application
    app _req respond = respond (responseLBS status404 [] "{}")

{- The mirror target: it accepts an npm publish @PUT@ (201) and records each PUT's path
into an 'IORef', so the worker's publish can be observed. -}
withMirrorTarget :: (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget body = do
    logRef <- newIORef []
    testWithApplication (pure (app logRef)) $ \port -> body (loopbackUrl port) logRef
  where
    app :: IORef [ByteString] -> Application
    app logRef req respond = do
        when (requestMethod req == "PUT") $
            atomicModifyIORef' logRef (\xs -> (rawPathInfo req : xs, ()))
        respond (responseLBS status201 [] "{}")

-- The artifact bytes the public upstream serves and the worker verifies + publishes.
tarballBytes :: LByteString
tarballBytes = "left-pad-1.0.0-artifact-bytes"

-- The true lower-cased hex SHA-1 (npm @dist.shasum@) of the served bytes.
sha1Shasum :: Text
sha1Shasum = decodeUtf8 (convertToBase Base16 (hashlazy tarballBytes :: Digest SHA1) :: ByteString)

-- The true SRI @sha512-<base64>@ (npm @dist.integrity@) of the served bytes -- the
-- strongest digest, the one the worker verifies the fetched bytes against.
sha512Integrity :: Text
sha512Integrity = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy tarballBytes :: Digest SHA512) :: ByteString)

{- A two-version packument: @1.0.0@ published in 2020 (clears the quarantine) and
@2.0.0@ published two days before the fixed clock (denied by it). Both carry a real
integrity digest, so the distinguishing factor is the rule, not integrity presence.
The @dist.tarball@ of each names the public stub at the given port. -}
packument :: Text -> Value
packument port =
    object
        [ "name" .= ("left-pad" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
        , "versions"
            .= object
                [ "1.0.0" .= versionObject "1.0.0" "left-pad-1.0.0.tgz" port
                , "2.0.0" .= versionObject "2.0.0" "left-pad-2.0.0.tgz" port
                ]
        , "time"
            .= object
                [ "1.0.0" .= ("2020-01-01T00:00:00.000Z" :: Text)
                , "2.0.0" .= ("2026-05-30T00:00:00.000Z" :: Text)
                ]
        ]

versionObject :: Text -> Text -> Text -> Value
versionObject version file port =
    object
        [ "name" .= ("left-pad" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= ("http://localhost:" <> port <> "/left-pad/-/" <> file)
                , "integrity" .= sha512Integrity
                , "shasum" .= sha1Shasum
                ]
        ]

-- A fixed clock. 2.0.0 is published two days earlier, well inside the 7-day window.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 1) 0

-- The shipped quarantine: admit a version only once it is older than a week.
admitOldEnough :: [PrecededRule]
admitOldEnough = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

-- A loopback base URL for a testWithApplication-hosted stub on the given port, addressed
-- by the "localhost" DNS name rather than a bare IP literal: the internal-range block
-- only recognises a literal, never a name, so this in-process stub never needs an
-- operator-style opt-in to let the fetch land.
loopbackUrl :: Int -> Text
loopbackUrl port = "http://localhost:" <> show port

-- The port a stub was reached on, from the request's @Host@ header, so a served
-- packument can name its own @dist.tarball@ at the same port.
selfPort :: Request -> Text
selfPort req =
    case find ((== hHost) . fst) (requestHeaders req) of
        Just (_, hostPort) -> snd (T.breakOnEnd ":" (decodeUtf8 hostPort))
        Nothing -> ""

-- A GET through the in-process proxy with no credential.
getThrough :: Application -> ByteString -> IO SResponse
getThrough app path = runSession (request (setPath defaultRequest path)) app

status :: SResponse -> Int
status = statusCode . simpleStatus

-- The version keys of a served packument body (its @versions@ object keys).
packumentVersions :: LByteString -> [Text]
packumentVersions body = case decode body of
    Just (Object o)
        | Just (Object versions) <- KeyMap.lookup "versions" o ->
            map Key.toText (KeyMap.keys versions)
    _ -> []

-- A scribe-free LogEnv (no stdout output during the integration run).
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

{- Run the supervised mirror worker ('runWorker') against the real queue until a
condition holds, then tear it down. The loop never returns on its own, so it is raced
against a condition-poller ('race_'); a hard timeout bounds the whole thing so a failing
test cannot hang. -}
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env done =
    void $ timeout loopHardTimeout $ race_ (runWorker policies env) (waitFor done)

-- A generous hard ceiling, far above a healthy fetch → verify → publish cycle even
-- under @-fhpc@ instrumentation, so it only ever fires on a genuine hang.
loopHardTimeout :: Int
loopHardTimeout = 45_000_000

-- Poll a condition until it holds, bounded so a failing test does not hang.
waitFor :: IO Bool -> IO ()
waitFor done = go (200 :: Int)
  where
    go :: Int -> IO ()
    go 0 = pure ()
    go n =
        done >>= \case
            True -> pure ()
            False -> threadDelay 200_000 >> go (n - 1)

publishedAtLeast :: IORef [a] -> Int -> IO Bool
publishedAtLeast logRef n = (>= n) . length <$> readIORef logRef

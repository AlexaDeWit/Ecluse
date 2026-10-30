-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | HTTP routing, advisory availability, and listener lifecycle regressions.
module Ecluse.Runtime.ServerSpec (spec) where

import Prelude hiding (get)

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (execute_, withConnection)
import Katip (LogEnv, SimpleLogPayload, closeScribes, runKatipContextT)
import Network.HTTP.Client (HttpException, defaultManagerSettings, httpNoBody, newManager, parseRequest)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types (Header, Method, Status, hConnection, hContentType, methodDelete, methodGet, methodHead, methodPost, methodPut, status200, status404, status500, statusCode)
import Network.Wai (
    Application,
    Response,
    ResponseReceived,
    responseLBS,
    responseStatus,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Test qualified as WaiTest
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO (timeout)
import UnliftIO.Async (link, poll, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, finally, throwIO, try)

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime)

import Ecluse.Composition.TelemetrySupport (newAdvisoryHandles)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Cve.Slot (swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataSerialise), AdapterPublish (publishRelay))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterProjectName))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter, npmPublish)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Publish qualified as NpmPublish
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Npm.Route.Internal (npmNotFound)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused))
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams),
    FailureAlignment (FailDeny, FailNoDecision),
    PrecededRule,
    Rule (AllowByIdentity, AllowIfOlderThan, DenyIfCve),
 )
import Ecluse.Core.Security (defaultLimits, maxPublishRequestBytes)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission)
import Ecluse.Core.Server.Context (MountRouter, PackumentDeps (pdMetadata), PublishDeps (..), ResponseAction (AnswerLocally, RunPipeline), RouteAction (RouteAction))
import Ecluse.Core.Server.Contract (ResponseContract, VariableResponse, variableOpaqueContract, variableResponse)
import Ecluse.Core.Server.Fault (RequestFault (rqCause))
import Ecluse.Core.Server.Readiness (Readiness (Latched))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (UnclassifiedFault))
import Ecluse.Core.Worker (Liveness (Liveness, liveHealthy, liveLastPoll), heartbeatLivenessNow, workerHeartbeatStaleAfter)
import Ecluse.Cve.Sync (CveSyncHandle (csClock, csEnv, csReady), cveRuleDepsFor, cveSyncReadiness)
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), OsvDbFetchFault (OsvDbTransport), SyncEnv (..), SyncHooks (..), SyncSchedule (..), runCveSync)
import Ecluse.Runtime.Env (envWorkerHeartbeat, recordPoll)
import Ecluse.Runtime.Server.Internal (
    DrainSignal,
    MountBinding (..),
    ServerConfig (..),
    ShutdownDrainTimeout (..),
    application,
    beginDrain,
    isDraining,
    listeningPrefix,
    mkServerConfig,
    newDrainSignal,
    perimeterGuard,
    probeOnlyApplication,
    proxyListener,
    raceServerAgainstLoop,
    runWarp,
    serveBound,
 )
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Cve (fetchServingAt, headOnlyFetch)
import Ecluse.Runtime.Test.Support (newTestEnv, newTestEnvLogging)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog, memoryLogEnv, newTestLogEnv)
import Ecluse.Test.Osv (mkDbWithWrongEpoch, mkMinimalValidDbWithMeta)
import Ecluse.Test.Package (validSha256, validSha256Sri)
import Ecluse.Test.Poll (awaitUntil, pollUntil)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort, passthroughAdvisorySyncTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity), packumentValue, publishedDaysAgo, versionSpec, versionValue)
import Ecluse.Test.Registry.Npm.Publish (npmReleaseDocument)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps, npmServeDeps, pypiServeDeps)
import Ecluse.Test.Stub (Captured (capHeaders, capPath), stubLocalhostUrl, withRoutedStub)
import Ecluse.Test.Wai (bodyContainsAll, freePort, selfBaseUrlOf, servedVersions, status, withSweptApp)

-- A mount with the given prefix and router over __inert__ serve dependencies, for routing cases alone.
mountAt :: NonEmpty Text -> MountRouter -> MountBinding
mountAt prefix router =
    publishMountAt prefix router Nothing

-- | Optional publication dependencies select the guard and relay or the disabled response.
publishMountAt :: NonEmpty Text -> MountRouter -> Maybe PublishDeps -> MountBinding
publishMountAt prefix router publishDeps =
    MountBinding
        { bindingPrefix = prefix
        , bindingRouter = router
        , bindingCredential = npmCredential
        , bindingPackumentDeps = inertPackumentDeps
        , bindingPublishDeps = publishDeps
        }

-- One npm mount over closed-port upstreams and no publish target, resolved as the composition root resolves it.
npmApp :: Deadlines -> IO Application
npmApp deadlines = application (mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))) <$> newTestEnv deadlines

-- One PyPI mount over closed-port upstreams and no publication target.
pypiApp :: Deadlines -> IO Application
pypiApp deadlines = application (mkServerConfig (maybeToList (mountBindingFor PyPI (inertDepsFor pypiAdapter) Nothing))) <$> newTestEnv deadlines

-- | An unreachable target distinguishes a refused publish from one that reached the relay.
npmMountApp :: Deadlines -> IO Application
npmMountApp deadlines = application (mkServerConfig [mountAt ("npm" :| []) npmRouter]) <$> newTestEnv deadlines

-- | An unreachable publish target distinguishes relay attempts from local guard refusals.
publishMountApp :: Deadlines -> IO Application
publishMountApp = publishAppWith basePublishDeps

-- | Allow the fixture scope while keeping the target unreachable.
basePublishDeps :: ByteAdmission -> PublishDeps
basePublishDeps bodyBudget =
    PublishDeps
        { pubReadUrl = loopbackRegistryUrl "http://127.0.0.1:1"
        , pubTargetUrl = loopbackRegistryUrl "http://127.0.0.1:1" -- an unconnectable port
        , pubAllowed = NpmPublish.npmPublishAllowed [mkScope "acme"]
        , pubStaticCredential = Nothing
        , pubStaticPresentation = nativePresentation
        , pubMaxRequestBytes = maxPublishRequestBytes defaultLimits
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits{maxPublishRequestBytes = 26214400}
        , pubBodyBudget = bodyBudget
        , pubHelp = Nothing
        , pubProjectName = adapterProjectName npmAdapter
        , pubAdapter = npmPublish
        }

-- The in-scope package name the publish cases write to.
widgetName :: Text
widgetName = "@acme/widget"

-- One @\/npm@ mount with the given publish deps and a body budget these cases never contend on.
publishAppWith :: (ByteAdmission -> PublishDeps) -> Deadlines -> IO Application
publishAppWith mkDeps deadlines = do
    bodyBudget <- newByteAdmission deadlines (128 * 1024 * 1024)
    application (mkServerConfig [publishMountAt ("npm" :| []) npmRouter (Just (mkDeps bodyBudget))]) <$> newTestEnv deadlines

-- One @\/npm@ mount under a __fake__ router, proving dispatch follows the binding's router.
fakeRouterApp :: Deadlines -> IO Application
fakeRouterApp deadlines = application (mkServerConfig [mountAt ("npm" :| []) fakeRouter]) <$> newTestEnv deadlines
  where
    -- npm's @is-odd@ would be a packument read. Under this router it is a miss, so the two
    -- routers give observably different answers.
    fakeRouter :: MountRouter
    fakeRouter _method _headers ["beep"] = RouteAction fakeContract (AnswerLocally (variableResponse status200 [] "{}"))
    fakeRouter _method _headers _ = npmNotFound

    fakeContract :: ResponseContract (VariableResponse LByteString)
    fakeContract = variableOpaqueContract "application/json" "The fake router's response."

-- No mounts: every path but the health probes is the neutral @404@.
neutralApp :: Deadlines -> IO Application
neutralApp deadlines = application (mkServerConfig []) <$> newTestEnv deadlines

-- | The probe-only front door the Dredger and Pilot worker roles run.
probeOnlyApp :: Deadlines -> IO Application
probeOnlyApp deadlines = probeOnlyApplication deadlines (mkServerConfig [])

-- | 'probeOnlyApp' with the injected liveness check failing, which only @\/livez@ reads.
deadProbeOnlyApp :: Deadlines -> IO Application
deadProbeOnlyApp deadlines = probeOnlyApplication deadlines ((mkServerConfig []){scCheckLive = pure notLive})

-- | A failing liveness verdict with no poll recorded, standing in for a stalled loop.
notLive :: Liveness
notLive = Liveness{liveHealthy = False, liveLastPoll = Nothing}

-- An npm mount whose 'DrainSignal' is __already raised__, as mid-shutdown, with no socket bound.
drainingApp :: Deadlines -> IO Application
drainingApp deadlines = do
    drain <- raisedDrain
    application (mkServerConfig [mountAt ("npm" :| []) npmRouter]){scDrain = drain} <$> newTestEnv deadlines

-- | A live 'DrainSignal' raised into the draining state.
raisedDrain :: IO DrainSignal
raisedDrain = do
    drain <- newDrainSignal
    beginDrain drain
    pure drain

-- An npm mount whose readiness is the Dredger's halt latch, which @\/readyz@ renders with no @mounts@ object.
latchedApp :: Deadlines -> IO Application
latchedApp deadlines =
    application (mkServerConfig [mountAt ("npm" :| []) npmRouter]){scCheckReady = pure Latched} <$> newTestEnv deadlines

-- An npm mount whose worker heartbeat is past 'workerHeartbeatStaleAfter', so liveness answers @503@.
stalledWorkerApp :: Deadlines -> IO Application
stalledWorkerApp deadlines = do
    env <- newTestEnv deadlines
    now <- getCurrentTime
    -- A poll older than the staleness threshold: the loop did not advance its
    -- heartbeat within the window, so liveness must read it as stalled.
    recordPoll (envWorkerHeartbeat env) (addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now)
    -- The composition root folds the heartbeat into /livez only when a worker
    -- runs. This fixture models that mirrored-deployment wiring explicitly.
    let cfg = (mkServerConfig [mountAt ("npm" :| []) npmRouter]){scCheckLive = heartbeatLivenessNow (envWorkerHeartbeat env)}
    pure (application cfg env)

-- Passes only without a @Connection@ header, which @hspec-wai@'s '<:>' cannot assert.
matchNoConnectionHeader :: MatchHeader
matchNoConnectionHeader = MatchHeader $ \headers _body ->
    if any ((== hConnection) . fst) headers
        then Just "expected no Connection header, but one was present"
        else Nothing

spec :: Spec
spec = do
    perimeterGuardSpec
    pipelineFallbackSpec
    composedNpmMountSpec
    partialAdvisorySpec
    startupAdvisorySpec
    refusedRenderSpec
    runWarpDrainWiringSpec
    listeningLineSpec
    raceServerAgainstLoopSpec
    uncodedRoutesSpec
    describe "control-plane health probes (above any mount)" $
        withSweptApp npmMountApp $ do
            it "reports a null last poll when no background loop is wired behind /livez" $
                get "/livez" `shouldRespondWith` 200{matchBody = bodyContainsAll ["\"lastPoll\":null"]}

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

    describe "readiness -- the latched arm" $
        withSweptApp latchedApp $
            it "fails /readyz with 503 and the halted body a latched Dredger answers" $
                get "/readyz" `shouldRespondWith` "{\"status\":\"halted\"}"{matchStatus = 503}

    describe "liveness -- worker-stall arm of /livez" $
        withSweptApp stalledWorkerApp $ do
            it "fails /livez with 503 once the worker heartbeat is stale" $
                -- Liveness folds in the mirror worker's consume-loop heartbeat, so a loop quiet
                -- past the threshold flips /livez to 503 even while the front door still serves.
                get "/livez" `shouldRespondWith` 503

            it "keeps /readyz at 200 (readiness ignores worker staleness; it is not draining)" $
                -- Readiness gates only on the drain signal, so a stalled worker fails /livez and
                -- never /readyz.
                get "/readyz" `shouldRespondWith` 200

    describe "graceful shutdown -- readiness flip while draining" $
        withSweptApp drainingApp $ do
            it "fails /readyz with 503 (the LB stops routing new traffic here)" $
                get "/readyz" `shouldRespondWith` 503

            it "keeps /livez at 200 (a draining instance is alive, not unhealthy)" $
                get "/livez" `shouldRespondWith` 200

    describe "graceful shutdown -- going-away header" $ do
        withSweptApp drainingApp $
            it "stamps Connection: close on a response while draining" $
                -- A keep-alive pool (a client's, or a mesh's) must not reuse a socket
                -- on a closing instance. The header is what tells it to close.
                get "/npm/-/ping"
                    `shouldRespondWith` 200{matchHeaders = ["Connection" <:> "close"]}

        withSweptApp npmMountApp $
            it "adds no Connection header when not draining" $
                get "/npm/-/ping" `shouldRespondWith` 200{matchHeaders = [matchNoConnectionHeader]}

    describe "meta-routes under a mount" $
        withSweptApp npmMountApp $ do
            it "answers /npm/-/ping locally with 200 {}" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "answers /npm/-/v1/search with 501 (search is not an install path)" $
                get "/npm/-/v1/search" `shouldRespondWith` 501

            it "answers a dist-tag read with 501, not the deny-by-default 404" $
                -- A 404 would read as the package being absent. The 501 says the operation
                -- is not implemented, which is what an operator needs to see.
                get "/npm/-/package/is-odd/dist-tags" `shouldRespondWith` 501

            it "answers a dist-tag write with 501, without reading the body" $
                request methodPut "/npm/-/package/is-odd/dist-tags/latest" [] "\"1.0.0\""
                    `shouldRespondWith` 501

            it "answers a dist-tag removal with 501, the same boundary as the write" $
                request methodDelete "/npm/-/package/is-odd/dist-tags/latest" [] ""
                    `shouldRespondWith` 501

            it "still denies a DELETE outside the dist-tag path with the 404" $
                -- The method claims one route, not a class of paths, so a package path
                -- under DELETE stays deny-by-default.
                request methodDelete "/npm/is-odd" [] "" `shouldRespondWith` 404

    describe "dispatch -- /npm mount (prefix strip + npm grammar)" $
        withSweptApp npmMountApp $ do
            it "accepts the bare mount prefix with a trailing slash (empty path → 404)" $
                get "/npm/" `shouldRespondWith` 404

            it "normalises repeated trailing slashes the same as one (empty path → 404)" $
                -- @/npm//@ collapses its run of trailing empty segments to the bare mount,
                -- exactly like @/npm/@, rather than leaving a spurious empty path component.
                get "/npm//" `shouldRespondWith` 404

            it "leaves an internal empty segment for the router to reject (404, not collapsed)" $
                -- The router drops only /trailing/ empties, so the leading empty segment stays and
                -- @/npm//is-odd@ never normalises to the @/npm/is-odd@ packument route.
                get "/npm//is-odd" `shouldRespondWith` 404

            it "404s an unknown /-/… meta-route under the mount" $
                get "/npm/-/whoami" `shouldRespondWith` 404

            it "404s a hostile traversal path rather than routing it" $
                -- @%2F@ decodes to one segment carrying a slash, so the router denies it.
                get "/npm/foo%2Fbar" `shouldRespondWith` 404

    describe "first-party publish path (PUT /{pkg})" $ do
        withSweptApp npmMountApp $
            it "405s a publish when no publication target is configured (the opt-in is off)" $
                -- This mount wires no publish dependencies, so there is no implicit write
                -- path. A PUT /{pkg} is Method Not Allowed.
                request methodPut "/npm/widget" [] "" `shouldRespondWith` 405

        withSweptApp publishMountApp $ do
            it "refuses an out-of-scope publish with 403, before any upstream write (anti-shadowing)" $
                -- Nothing contacts the unconnectable target, so a 403 rather than a 502 proves the
                -- guard fired before the relay.
                request methodPut "/npm/@other/widget" [] "" `shouldRespondWith` 403

            it "refuses an unscoped publish with 403 (an unscoped name is within no scope)" $
                request methodPut "/npm/widget" [] "" `shouldRespondWith` 403

            it "lets an in-scope publish through the guard to the relay (502 when the target is unreachable)" $
                -- The target is unconnectable, so the 502 proves the guard let the write through to
                -- the relay rather than refusing it at the scope check.
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName widgetName widgetName) `shouldRespondWith` 502

            -- The unreachable target would answer 502, so 403 proves the guard stopped the relay.
            it "refuses an in-scope publish whose body _id / name disagree with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument "@victim/target" "@victim/target" widgetName) `shouldRespondWith` 403

            it "refuses an in-scope publish whose body versions[].name disagrees with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName widgetName "@victim/target") `shouldRespondWith` 403

            -- The wire manifest is the publish path's other name. A declared name the npm grammar
            -- refuses canonicalises to nothing, which is a disagreement, so it never reaches a write.
            it "refuses an in-scope publish whose body declares a non-ASCII name with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName "@acme/wid\x3164get" widgetName) `shouldRespondWith` 403

            it "lets an in-scope publish whose body _id / name / versions[].name all agree with the URL through to the relay (502 when unreachable)" $
                -- The guard does not over-refuse a body whose every declared name matches
                -- the URL. It reaches the relay (502 to the unconnectable target), not a 403.
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName widgetName widgetName) `shouldRespondWith` 502

        withSweptApp (publishAppWith (\b -> (basePublishDeps b){pubAdapter = npmPublish{publishRelay = \_ _ _ _ -> throwIO (RelayContractEscape "simulated relay contract escape")}})) $
            it "answers a relay contract escape with the route's declared 500 (not a torn session, not a 502)" $
                -- The relay reports its failures as typed values, so a throw is an invariant break.
                -- The perimeter answers it with the neutral 500 and the session survives.
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName widgetName widgetName) `shouldRespondWith` 500

        withSweptApp (publishAppWith (\b -> (basePublishDeps b){pubTargetUrl = loopbackRegistryUrl ""})) $
            it "500s an in-scope publish when the publication target URL is unformable (misconfig)" $
                -- An empty target URL cannot form a request, a configuration fault rather
                -- than a transient outage, so the publish is a 500 (not a 502).
                request methodPut "/npm/@acme/widget" [] (npmReleaseDocument widgetName widgetName widgetName) `shouldRespondWith` 500

        withSweptApp (publishAppWith (\b -> (basePublishDeps b){pubInboundToken = Just (mkSecret "edge-token")})) $ do
            it "401s a publish that fails the edge token gate (before the scope guard)" $
                -- With an edge token configured, the edge rejects a publish that carries
                -- none. It is the same gate the read paths apply.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 401

    describe "the two response tiers (neutral above mounts, route contract within)" $
        withSweptApp npmMountApp $ do
            it "renders an UNMOUNTED path as a neutral text/plain 404" $
                -- No mount matches @/pypi/...@, so there is no ecosystem to shape it. The
                -- body is the generic plain-text Not Found, not an npm error object.
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unrecognised IN-MOUNT path through npm's fallback contract" $
                -- The body is npm's error object, the mount's own surface, rather than the neutral
                -- plain-text 404 above.
                get "/npm/is-odd/3.0.1" `shouldRespondWith` "{\"error\":\"not found\"}"{matchStatus = 404}

            it "derives a bodiless response for an unrecognised HEAD path" $
                request methodHead "/npm/is-odd/3.0.1" [] ""
                    `shouldRespondWith` ""{matchStatus = 404}

    describe "dispatch -- injected router (the routing boundary)" $
        -- Dispatch runs under a FAKE router, not npm's, so the action a request takes must
        -- follow the injected function.
        withSweptApp fakeRouterApp $ do
            it "routes the fake router's recognised path (/npm/beep → answered locally → 200 {})" $
                get "/npm/beep" `shouldRespondWith` "{}"{matchStatus = 200}

            it "denies a path npm would accept but the fake does not (/npm/is-odd → 404)" $
                -- Under npm's router @is-odd@ is a packument read, so this 404 proves dispatch
                -- followed the injected function.
                get "/npm/is-odd" `shouldRespondWith` 404

            it "denies npm's ping meta-route (the fake router does not recognise it)" $
                get "/npm/-/ping" `shouldRespondWith` 404

    describe "no mounts -- neutral by default" $
        -- With no mount wired, the web layer serves nothing but the health probes.
        -- Every other path matches no mount and is the neutral 404.
        withSweptApp neutralApp $ do
            it "404s a package-shaped path (no mount is configured)" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "still answers the control-plane health probes (above any mount)" $ do
                get "/livez" `shouldRespondWith` 200
                get "/readyz" `shouldRespondWith` 200

    describe "probeOnlyApplication (the worker roles' front door)" $ do
        withSweptApp probeOnlyApp $ do
            it "answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

            it "404s every other path (a worker role serves no mount)" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

        withSweptApp deadProbeOnlyApp $ do
            it "fails /livez with 503 when the injected liveness check fails" $
                get "/livez" `shouldRespondWith` 503

            it "keeps /readyz at 200 (readiness does not read the liveness check)" $
                get "/readyz" `shouldRespondWith` 200

    describe "mkServerConfig -- defaults" $ do
        it "listens on the conventional npm proxy port" $
            scPort (mkServerConfig []) `shouldBe` 4873

        it "defaults the graceful-drain timeout to 30 seconds" $
            scDrainTimeout (mkServerConfig []) `shouldBe` ShutdownDrainTimeout 30

-- | A typed stand-in for a relay implementation escaping its typed contract.
newtype RelayContractEscape = RelayContractEscape Text
    deriving stock (Show)

instance Exception RelayContractEscape

-- | Record statuses and classified causes alongside the guard result, including rethrows.
driveGuard :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO ([Int], [RequestFaultCause], Either SomeException ())
driveGuard handler = do
    responses <- newIORef []
    observed <- newIORef []
    let respond response = do
            modifyIORef' responses (<> [statusCode (responseStatus response)])
            pure ResponseReceived
    outcome <- try (void (perimeterGuard (\fault -> modifyIORef' observed (<> [rqCause fault])) respond fallback handler))
    (,,) <$> readIORef responses <*> readIORef observed <*> pure outcome
  where
    fallback = responseLBS status500 [] "internal server error"

-- Native metadata gzip covers admitted listings alone, so every other route ignores Accept-Encoding.
uncodedRoutesSpec :: Spec
uncodedRoutesSpec = describe "routes outside native metadata gzip" $ do
    withSweptApp npmMountApp $
        it "answer probes, ping, search, unrouted paths, and a disabled publish alike under Accept-Encoding gzip" $
            for_ [(methodGet, "/livez"), (methodGet, "/readyz"), (methodGet, "/npm/-/ping"), (methodHead, "/npm/-/ping"), (methodGet, "/npm/-/v1/search"), (methodGet, "/npm/-/nope/x"), (methodPut, "/npm/widget")] (uncoded "")
    withSweptApp publishMountApp $
        it "answer a refused and a relayed publish alike under Accept-Encoding gzip" $
            for_ [(methodPut, "/npm/@other/widget"), (methodPut, "/npm/@acme/widget")] (uncoded (npmReleaseDocument widgetName widgetName widgetName))
    withSweptApp pypiApp $
        it "answer PyPI distribution, companion, upload, and unrouted refusals alike under Accept-Encoding gzip" $
            for_ [(methodGet, "/pypi/simple/thing/thing-1.0.tar.gz"), (methodHead, "/pypi/simple/thing/thing-1.0.tar.gz"), (methodGet, "/pypi/simple/thing/thing-1.0-py3-none-any.whl.metadata"), (methodPost, "/pypi/legacy"), (methodGet, "/pypi/nope")] (uncoded "")
  where
    uncoded body (method, path) = do
        plain <- request method path [] body
        offered <- request method path [("Accept-Encoding", "gzip")] body
        liftIO $ do
            (path, WaiTest.simpleStatus offered) `shouldBe` (path, WaiTest.simpleStatus plain)
            (path, WaiTest.simpleHeaders offered) `shouldBe` (path, WaiTest.simpleHeaders plain)
            (path, WaiTest.simpleBody offered) `shouldBe` (path, WaiTest.simpleBody plain)
            (path, lookup "Content-Encoding" (WaiTest.simpleHeaders offered)) `shouldBe` (path, Nothing)

perimeterGuardSpec :: Spec
perimeterGuardSpec = describe "perimeterGuard (the typed request perimeter)" $ do
    it "passes a committed response through untouched, observing nothing" $ do
        (statuses, observed, outcome) <- driveGuard (\respond -> respond (responseLBS status200 [] "ok"))
        statuses `shouldBe` [200]
        observed `shouldBe` []
        outcome `shouldSatisfy` isRight

    it "answers an unrecognised pre-commit escape with the neutral 500, observed as UnclassifiedFault" $ do
        (statuses, observed, _) <- driveGuard (\_respond -> throwIO (RelayContractEscape "boom"))
        statuses `shouldBe` [500]
        observed `shouldBe` [UnclassifiedFault]

    it "rethrows a post-commit escape: one response, nothing observed, the fault propagates" $ do
        (statuses, observed, outcome) <- driveGuard $ \respond -> do
            received <- respond (responseLBS status200 [] "committed")
            _ <- throwIO (RelayContractEscape "post-commit teardown")
            pure received
        statuses `shouldBe` [200]
        observed `shouldBe` []
        case outcome of
            Left escape -> fmap (\(RelayContractEscape detail) -> detail) (fromException escape) `shouldBe` Just "post-commit teardown"
            Right () -> expectationFailure "expected the post-commit escape to rethrow"

-- Each pipeline route's escape before commit answers the route's own neutral 500, pinned byte for byte.
pipelineFallbackSpec :: Spec
pipelineFallbackSpec = describe "a pipeline route's neutral 500 after an escape before commit" $
    withSweptApp escapingApp $
        it "answers each npm and PyPI pipeline route with its pinned status, headers, and body" $
            for_ pipelineFallbacks $ \(method, path, expected) -> do
                response <- request method path [] ""
                liftIO ((method, path, answered response) `shouldBe` (method, path, expected))
  where
    answered response = (statusCode (WaiTest.simpleStatus response), WaiTest.simpleHeaders response, WaiTest.simpleBody response)

-- npm answers its JSON error body. PyPI's reads answer a bare status (no help message is set), its upload plain text.
pipelineFallbacks :: [(Method, ByteString, (Int, [Header], LByteString))]
pipelineFallbacks =
    [ (methodGet, "/npm/widget", (500, npmJson, npmBody))
    , (methodHead, "/npm/widget", (500, npmJson, ""))
    , (methodGet, "/npm/widget/-/widget-1.0.0.tgz", (500, npmJson, npmBody))
    , (methodHead, "/npm/widget/-/widget-1.0.0.tgz", (500, npmJson, ""))
    , (methodPut, "/npm/widget", (500, npmJson, npmBody))
    , (methodGet, "/pypi/simple/thing", (500, [], ""))
    , (methodGet, "/pypi/simple/thing/thing-1.0.tar.gz", (500, [], ""))
    , (methodHead, "/pypi/simple/thing/thing-1.0.tar.gz", (500, [], ""))
    , (methodGet, "/pypi/simple/thing/thing-1.0-py3-none-any.whl.metadata", (500, [], ""))
    , (methodPost, "/pypi/legacy", (500, [(hContentType, "text/plain")], "internal server error"))
    ]
  where
    npmJson = [(hContentType, "application/json")]
    npmBody = "{\"error\":\"internal server error\"}"

-- The shipped npm and PyPI mounts, with every pipeline handler escaping before it responds.
escapingApp :: Deadlines -> IO Application
escapingApp deadlines = application (mkServerConfig (map escaping mounts)) <$> newTestEnv deadlines
  where
    mounts = catMaybes [mountBindingFor Npm inertPackumentDeps Nothing, mountBindingFor PyPI (inertDepsFor pypiAdapter) Nothing]
    escaping binding = binding{bindingRouter = escapingBeforeCommit (bindingRouter binding)}

-- Keep the route's contract and fallback, and replace its handler with one that escapes before it responds.
escapingBeforeCommit :: MountRouter -> MountRouter
escapingBeforeCommit router method headers segments = case router method headers segments of
    RouteAction contract (RunPipeline fallback _) -> RouteAction contract (RunPipeline fallback (\_ _ -> throwIO (RelayContractEscape "escaped before commit")))
    action -> action

-- The application builder raises it on receipt of its config, so 'runWarp' unwinds before binding a socket.
data AbortLaunch = AbortLaunch
    deriving stock (Eq, Show)

instance Exception AbortLaunch

-- | Exercise the live drain signal allocated by runWarp instead of an injected fixture signal.
runWarpDrainWiringSpec :: Spec
runWarpDrainWiringSpec = describe "runWarp -- graceful-drain wiring (issue #841)" $
    it "hands the application builder the live drain, not the inert neverDraining" $ do
        captured <- newEmptyMVar
        -- The scDrain from mkServerConfig is neverDraining, so an app closed over that inert
        -- signal would never see a drain. Abort before warp binds a socket.
        let getApp cfg = putMVar captured (scDrain cfg) >> throwIO AbortLaunch
        logEnv <- newTestLogEnv
        runWarp logEnv "probe" (mkServerConfig []) getApp `shouldThrow` (== AbortLaunch)
        drain <- takeMVar captured
        -- A live, lowered signal: raising it is observable. neverDraining's raise is a
        -- no-op, so an inert signal would stay False where this one flips True.
        isDraining drain `shouldReturn` False
        beginDrain drain
        isDraining drain `shouldReturn` True

-- Pin the line 'runWarp' logs once bound, through 'serveBound', which installs no process-wide signal handlers.
listeningLineSpec :: Spec
listeningLineSpec = describe "serveBound -- the listening line" $ do
    it "logs the proxy's line with its configured port once it serves there" $ do
        port <- freePort
        (logged, answers) <- listeningOn port
        logged `shouldBe` Just port
        answers `shouldBe` True
    it "logs the port the socket took when the configured port is 0, and serves there" $ do
        (logged, answers) <- listeningOn 0
        logged `shouldSatisfy` maybe False (/= 0)
        answers `shouldBe` True

-- Serve the probes on a configured port, wait up to ten seconds for the proxy's listening line,
-- and return the port it names and whether @/livez@ answers on that port.
listeningOn :: Int -> IO (Maybe Int, Bool)
listeningOn configured = withSweeper $ \deadlines -> do
    app <- probeOnlyApplication deadlines (mkServerConfig [])
    bracket memoryLogEnv (void . closeScribes . fst) $ \(logEnv, messages) ->
        withAsync (serveBound logEnv proxyListener (Warp.setPort configured Warp.defaultSettings) app) $ \server -> do
            link server
            logged <- pollUntil 200 50_000 isJust (loggedPort <$> messages)
            answers <- maybe (pure False) livezAnswers logged
            pure (logged, answers)
  where
    loggedPort = listToMaybe . mapMaybe (readMaybe . toString <=< T.stripPrefix (listeningPrefix proxyListener))

livezAnswers :: Int -> IO Bool
livezAnswers port = do
    manager <- newManager defaultManagerSettings
    livez <- parseRequest ("http://127.0.0.1:" <> show port <> "/livez")
    either (const False) ((== status200) . HttpClient.responseStatus) <$> (try (httpNoBody livez manager) :: IO (Either HttpException (HttpClient.Response ())))

-- A fault from one arm of 'raceServerAgainstLoop', which the race must re-raise rather than swallow.
newtype RaceBoom = RaceBoom Text
    deriving stock (Eq, Show)

instance Exception RaceBoom

-- The server arm's return cancels the never-returning loop, and a fault from either arm re-raises.
raceServerAgainstLoopSpec :: Spec
raceServerAgainstLoopSpec = describe "raceServerAgainstLoop -- shutdown-race invariant (issue #842)" $ do
    it "returns when the server arm returns, cancelling the never-returning loop" $ do
        -- A `concurrently_` would keep waiting on the never-returning loop and time out. Under
        -- `race_` the server's return cancels it, so the loop's `finally` cleanup runs.
        cancelled <- newIORef False
        let server = pass
            loop = forever (threadDelay 1_000_000) `finally` writeIORef cancelled True
        outcome <- timeout 2_000_000 (raceServerAgainstLoop server loop)
        outcome `shouldBe` Just ()
        readIORef cancelled `shouldReturn` True

    it "re-raises a fault thrown by the server arm (fails the process up)" $ do
        let server = throwIO (RaceBoom "server")
            loop = forever (threadDelay 1_000_000)
        raceServerAgainstLoop server loop `shouldThrow` (== RaceBoom "server")

    it "re-raises a fault thrown by the loop arm (fails the process up)" $ do
        let server = forever (threadDelay 1_000_000)
            loop = throwIO (RaceBoom "loop")
        raceServerAgainstLoop server loop `shouldThrow` (== RaceBoom "loop")

-- The front door a bare npm mount resolves to, over the composition root's binding, not this module's fixture.
composedNpmMountSpec :: Spec
composedNpmMountSpec = describe "the composed npm front door (a bare npm mount over mkServerConfig)" $
    withSweptApp npmApp $ do
        it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
            get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

        it "routes an npm packument under the mount into the data plane (503; upstreams closed)" $
            -- Both upstreams are bound to a closed port, so no version survives and the serve
            -- path answers 503. A 404 would mean the mount's router never claimed the path.
            get "/npm/is-odd" `shouldRespondWith` 503

        it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
            get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

        it "renders an unmounted prefix as a neutral text/plain 404" $
            get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

-- | Two mounts over one advisory database, one slot filled and the other still empty.
partialAdvisorySpec :: Spec
partialAdvisorySpec = describe "two mounts, one advisory database" $ do
    it "serves npm, refuses the PyPI read, and serves it once the PyPI artifact lands" $
        -- The owner's case: npm synced and the PyPI artifact never arrived. One upstream
        -- serves both documents, so the advisory slot is the only difference between them.
        withRoutedStub upstreamReply $ \stub -> withSweeper $ \deadlines -> do
            (app, handles) <- advisoryApp deadlines (stubLocalhostUrl stub) advisoryPolicy [Npm] False

            npm <- requestPath app "/npm/leftpad"
            status npm `shouldBe` 200
            servedVersions npm `shouldBe` ["1.0.0"]

            pypi <- requestPath app "/pypi/simple/leftpad/"
            status pypi `shouldBe` 503

            awaiting <- requestPath app "/readyz"
            status awaiting `shouldBe` 200
            bodyOf awaiting `shouldSatisfy` BS.isInfixOf "\"pypi\":\"awaiting the advisory database that ecluse pilot publishes\""

            -- The recovery is what pins that 503 on the empty slot, because an upstream
            -- failure renders a bare 503 too and this upstream never changed.
            advisoriesLanded handles PyPI
            recovered <- requestPath app "/pypi/simple/leftpad/"
            status recovered `shouldBe` 200

            ready <- requestPath app "/readyz"
            bodyOf ready `shouldSatisfy` BS.isInfixOf "\"npm\":\"ready\""
            bodyOf ready `shouldSatisfy` BS.isInfixOf "\"pypi\":\"ready\""

    it "admits the PyPI read on an identity override while that slot is still empty" $
        -- An intentional allow outranks the advisory deny, so it keeps admitting through
        -- the outage the mount beside it is still waiting out.
        withRoutedStub upstreamReply $ \stub -> withSweeper $ \deadlines -> do
            (app, _) <- advisoryApp deadlines (stubLocalhostUrl stub) overridePolicy [Npm] False
            overridden <- requestPath app "/pypi/simple/leftpad/"
            status overridden `shouldBe` 200

startupAdvisorySpec :: Spec
startupAdvisorySpec = describe "advisory acquisition at startup" $
    for_ ["unavailable S3", "missing object", "rejected artifact"] $ \failure ->
        for_ [("refuse", advisoryPolicy, False, 503), ("skip", skipAdvisoryPolicy, False, 200), ("override", overridePolicy, False, 200), ("trusted read", advisoryPolicy, True, 200)] $ \(policyName, policy, trusted, expected) ->
            it (failure <> " keeps serving under " <> policyName <> " and recovers without restart") $
                withSystemTempDirectory "startup-advisories" $ \dir ->
                    withRoutedStub upstreamReply $ \stub -> withSweeper $ \deadlines -> do
                        (app, handles) <- advisoryApp deadlines (stubLocalhostUrl stub) policy [] trusted
                        handle <- maybe (fail "missing PyPI sync handle") pure (Map.lookup PyPI handles)
                        pushedAt <- csClock handle
                        recovery <- newTVarIO False
                        steps <- newTVarIO (0 :: Int)
                        let initial = case failure of
                                "unavailable S3" -> headOnlyFetch (Left (OsvDbTransport (transportFault TransportUnreachable "S3 unavailable")))
                                "missing object" -> headOnlyFetch (Right Nothing)
                                _ -> fetchServingAt (Just servedAt) (Just "rejected") mkDbWithWrongEpoch
                            healthy = fetchServingAt (Just pushedAt) (Just "recovered") $ \dest -> do
                                mkMinimalValidDbWithMeta dest "leftpad" []
                                withConnection dest $ \conn -> execute_ conn "UPDATE meta SET value = 'pypi' WHERE key = 'ecosystem'"
                            choose = readTVarIO recovery <&> \ready -> if ready then healthy else initial
                            fetch = CveFetch{fetchHead = choose >>= fetchHead, fetchDownload = \dest -> choose >>= (`fetchDownload` dest)}
                            syncEnv = (csEnv handle){syncFetch = fetch, syncDbPath = dir </> "pypi.db"}
                            hooks = SyncHooks (atomically (writeTVar (csReady handle) True)) (atomically (modifyTVar' steps (+ 1)))
                            schedule = SyncSchedule [1_000, 1_000] 20_000 900_000_000
                        (_, logged) <- captureJsonLog $ \logEnv ->
                            withAsync (runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (runCveSync noopAdvisorySyncMetricsPort passthroughAdvisorySyncTracingPort syncEnv schedule hooks)) $ \worker -> do
                                awaitUntil 5_000_000 1_000 ((>= 4) <$> readTVarIO steps) `shouldReturn` True
                                poll worker >>= (`shouldSatisfy` isNothing)
                                status <$> requestPath app "/livez" `shouldReturn` 200
                                status <$> requestPath app "/readyz" `shouldReturn` 503
                                status <$> requestPath app "/pypi/simple/leftpad/" `shouldReturn` expected
                                advisoriesLanded handles Npm
                                status <$> requestPath app "/readyz" `shouldReturn` 200
                                status <$> requestPath app "/npm/leftpad" `shouldReturn` 200
                                status <$> requestPath app "/pypi/simple/leftpad/" `shouldReturn` expected
                                atomically (writeTVar recovery True)
                                awaitUntil 5_000_000 1_000 (readTVarIO (csReady handle)) `shouldReturn` True
                                poll worker >>= (`shouldSatisfy` isNothing)
                                recovered <- requestPath app "/readyz"
                                bodyOf recovered `shouldSatisfy` BS.isInfixOf "\"pypi\":\"ready\""
                                status <$> requestPath app "/pypi/simple/leftpad/" `shouldReturn` 200
                        let report = if failure == "rejected artifact" then "downloaded artifact refused" else "stays not-ready"
                            errors = filter (T.isInfixOf report) (lines logged)
                        errors `shouldSatisfy` (not . null)
                        for_ errors $ \line -> line `shouldSatisfy` T.isInfixOf "\"sev\":\"Error\""

skipAdvisoryPolicy :: [PrecededRule]
skipAdvisoryPolicy =
    [ atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8.0 FailNoDecision))
    , atDefaultPrecedence (AllowIfOlderThan 0)
    ]

refusedRenderSpec :: Spec
refusedRenderSpec = describe "a served document whose render the adapter refuses" $
    it "answers the neutral 500 as a render fault, and caches no assembled body" $
        withRoutedStub upstreamReply $ \stub -> do
            (statuses, logged) <- captureJsonLog $ \logEnv -> withSweeper $ \deadlines -> do
                app <- refusingRenderApp deadlines (stubLocalhostUrl stub) logEnv
                -- An assembled body cached by the first request would answer the second with 200.
                replicateM 2 (status <$> requestPath app "/npm/leftpad")
            statuses `shouldBe` [500, 500]
            T.count "\"perimeterCause\":\"RenderFault\"" logged `shouldBe` 2

-- One npm mount over the upstream whose serialiser refuses every render, logging to the given environment.
refusingRenderApp :: Deadlines -> Text -> LogEnv -> IO Application
refusingRenderApp deadlines upstreamBase logEnv = do
    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan 0)]
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    env <- newTestEnvLogging deadlines logEnv queue (manager, manager) telemetryDisabled
    let deps = npmServeDeps Nothing (loopbackRegistryUrl upstreamBase) NoMirrorWrite rules (pure servedAt)
        refusing = deps{pdMetadata = (pdMetadata deps){metadataSerialise = const (Left RenderRefused)}}
    pure (application (mkServerConfig (maybeToList (mountBindingFor Npm refusing Nothing))) env)

-- | Bind both mounts to their advisory handles, with the named ecosystems initially loaded.
advisoryApp :: Deadlines -> Text -> [PrecededRule] -> [Ecosystem] -> Bool -> IO (Application, Map.Map Ecosystem CveSyncHandle)
advisoryApp deadlines upstreamBase policy initiallyLoaded trusted = do
    handles <- Map.fromList <$> newAdvisoryHandles [Npm, PyPI]
    for_ handles $ \handle -> atomically (writeTVar (csReady handle) False)
    for_ initiallyLoaded (advisoriesLanded handles)
    let depsFor = cveRuleDepsFor handles noBreakerReporter (\_ _ -> pass)
    npmRules <- prepare (depsFor Npm) policy
    pypiRules <- prepare (depsFor PyPI) policy
    env <- newTestEnv deadlines
    let public = loopbackRegistryUrl upstreamBase
        private = if trusted then Just public else Nothing
        bindings =
            catMaybes
                [ mountBindingFor Npm (npmServeDeps private public NoMirrorWrite npmRules (pure servedAt)) Nothing
                , mountBindingFor PyPI (pypiServeDeps private public NoMirrorWrite pypiRules (pure servedAt)) Nothing
                ]
        cfg = (mkServerConfig bindings){scCheckReady = cveSyncReadiness handles}
    pure (application cfg env, handles)

-- Fill one slot and mark its first sync, with a publication time for freshness checks.
advisoriesLanded :: Map.Map Ecosystem CveSyncHandle -> Ecosystem -> IO ()
advisoriesLanded handles eco =
    for_ (Map.lookup eco handles) $ \handle -> do
        pushedAt <- csClock handle
        swapIn (syncSlot (csEnv handle)) (DbEtag "landed") (Just pushedAt) (fakeCveDb [])
        atomically (writeTVar (csReady handle) True)

-- The advisory deny outranks the age allow, so an empty slot refuses.
advisoryPolicy :: [PrecededRule]
advisoryPolicy =
    [ atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8.0 FailDeny))
    , atDefaultPrecedence (AllowIfOlderThan 0)
    ]

-- PyPI identity uses canonical PEP 440 spelling, so 1.0.0 is 1.
overridePolicy :: [PrecededRule]
overridePolicy = atDefaultPrecedence (AllowByIdentity "leftpad@1") : advisoryPolicy

-- One upstream for both ecosystems. Each document names the authority that served it, which is
-- the authority a projection accepts artifact locations on.
upstreamReply :: Captured -> (Status, [Header], LByteString)
upstreamReply cap
    | capPath cap == "/leftpad" = served "application/json" (npmPackument authority)
    | "/simple/leftpad" `BS.isPrefixOf` capPath cap =
        served "application/vnd.pypi.simple.v1+json" (pypiIndex authority)
    | otherwise = (status404, [], "")
  where
    authority = selfBaseUrlOf (capHeaders cap)
    served mediaType document = (status200, [(hContentType, mediaType)], encode document)

-- | One admissible npm version, digested to the fixtures' SHA-256 floor.
npmPackument :: Text -> Value
npmPackument authority =
    packumentValue
        "leftpad"
        "1.0.0"
        [("1.0.0", versionValue (versionSpec "leftpad" "1.0.0" (authority <> "/leftpad/-/leftpad-1.0.0.tgz")){vsIntegrity = Just validSha256Sri})]
        ["1.0.0" .= publishedDaysAgo servedAt 30]
        []

-- | One admissible PyPI release, on the index's own authority.
pypiIndex :: Text -> Value
pypiIndex authority =
    object
        [ "name" .= ("leftpad" :: Text)
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "files"
            .= [ object
                    [ "filename" .= ("leftpad-1.0.0.tar.gz" :: Text)
                    , "url" .= (authority <> "/simple/leftpad/leftpad-1.0.0.tar.gz")
                    , "hashes" .= object ["sha256" .= validSha256]
                    , "requires-python" .= (">=3.10" :: Text)
                    , "upload-time" .= ("2026-01-01T00:00:00Z" :: Text)
                    ]
               ]
        ]

-- | A fixed "now", so the age rule is deterministic.
servedAt :: UTCTime
servedAt = UTCTime (fromGregorian 2026 6 20) 0

-- | One GET against the composed application, as an orchestrator or a client would send it.
requestPath :: Application -> ByteString -> IO WaiTest.SResponse
requestPath app path = WaiTest.runSession (WaiTest.request (WaiTest.setPath WaiTest.defaultRequest path)) app

-- | A response body as strict bytes, for a substring assertion over rendered JSON.
bodyOf :: WaiTest.SResponse -> ByteString
bodyOf = LBS.toStrict . WaiTest.simpleBody

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ServerSpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (hConnection, methodPost, methodPut, status200, statusCode)
import Network.Wai (
    Application,
    Request (requestBodyLength, requestMethod),
    RequestBodyLength (ChunkedBody),
    Response,
    ResponseReceived,
    consumeRequestBodyStrict,
    defaultRequest,
    responseLBS,
    responseStatus,
 )
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Test (SRequest (SRequest), runSession, setPath, simpleStatus, srequest)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO.Exception (throwIO, try)

import Data.Time (addUTCTime, getCurrentTime)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry (RegistryUnconfigured (RegistryUnconfigured))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..), relayPublishDocument)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PublishDeps (..))
import Ecluse.Core.Server.Fault (RequestFault (rqCause))
import Ecluse.Core.Server.Route (Classifier, Route (..))
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (GateFault, UnclassifiedFault))
import Ecluse.Core.Worker (workerHeartbeatStaleAfter)
import Ecluse.Proxy (unconfiguredRegistry)
import Ecluse.Runtime.Env (Env, envWorkerHeartbeat, newEnv, newWorkerHeartbeat, recordPoll)
import Ecluse.Runtime.Server (
    DrainSignal,
    MountBinding (..),
    RequestSizeLimit (..),
    ServerConfig (..),
    ShutdownDrainTimeout (..),
    application,
    beginDrain,
    defaultPort,
    defaultShutdownDrainTimeout,
    mkServerConfig,
    newDrainSignal,
    perimeterGuard,
    serverMiddleware,
 )
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Queue (newTestMemoryQueue)

{- | A registry-handle double whose effectful fields are never invoked: the web
layer only routes, classifies, and renders -- it never fetches -- so a handle that
refuses loudly is enough to assemble an 'Env'. If a route reached an effectful
field, the refusal would surface the leak.
-}

-- | A credential-handle double: a fixed, non-expiring token, never read here.

-- | A manager with no TLS and no connection opened on construction.
newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

-- | Assemble an 'Env' from the handle doubles, touching no network.
newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newTestManager
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv unconfiguredRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | A test mount binding: the given prefix and classifier, npm's denial renderer,
and no packument-serve dependencies (so a 'Packument' route is the recognised-but-
unserved @501@ stub).
-}
mountAt :: NonEmpty Text -> Classifier -> MountBinding
mountAt prefix classifier =
    publishMountAt prefix classifier Nothing

{- | A test mount binding like 'mountAt' but with the given (optional) first-party
publish dependencies -- 'Nothing' leaves a @PUT \/{pkg}@ the @405@ opt-out, 'Just'
enables the publish path (the scope guard and the relay).
-}
publishMountAt :: NonEmpty Text -> Classifier -> Maybe PublishDeps -> MountBinding
publishMountAt prefix classifier publishDeps =
    MountBinding
        { bindingPrefix = prefix
        , bindingClassifier = classifier
        , bindingPackumentDeps = Nothing
        , bindingPublishDeps = publishDeps
        , bindingRenderer = npmRenderer
        }

{- | The 'application' under a single @\/npm@ mount carrying npm's path grammar, so
prefix-strip dispatch and the npm routing table can be asserted through the real
classifier the composition root wires in.
-}
npmMountApp :: IO Application
npmMountApp = application (mkServerConfig [mountAt ("npm" :| []) Npm.classify]) <$> newTestEnv

{- | The 'application' under a single @\/npm@ mount with the first-party publish path
__enabled__: a publish-scope allow-list of @\@acme@ and a publication target pointed at
an __unconnectable__ address. So an in-scope publish passes the anti-shadowing guard and
attempts the relay (which then fails to connect -- a @502@, proving the write was
reached), while an out-of-scope publish is refused at the guard (a @403@) before any
connection -- the assertion that the guard fires before any upstream write.
-}
publishMountApp :: IO Application
publishMountApp = publishAppWith basePublishDeps

{- | The base first-party publish dependencies the publish tests build on: an @\@acme@
scope allow-list and a publication target at an unconnectable port (so an in-scope
publish reaches the relay and fails to connect -- a @502@).
-}
basePublishDeps :: PublishDeps
basePublishDeps =
    PublishDeps
        { pubTargetUrl = "http://127.0.0.1:1" -- an unconnectable port
        , pubScopes = [mkScope "acme"]
        , pubStaticToken = Nothing
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits
        , pubHelp = Nothing
        , pubRelayPublish = \l m t s -> relayPublishDocument (NpmClientConfig t m s l)
        , pubCanonicaliseName = rightToMaybe . Project.projectName
        }

-- | The 'application' under a single @\/npm@ mount carrying the given publish deps.
publishAppWith :: PublishDeps -> IO Application
publishAppWith deps =
    application (mkServerConfig [publishMountAt ("npm" :| []) Npm.classify (Just deps)]) <$> newTestEnv

{- | The 'application' under a single @\/npm@ mount whose classifier is a __fake__
grammar (not npm's), proving dispatch routes through the binding's classifier
rather than any hardwired grammar. The fake recognises a single sentinel path and
denies everything else, so a response that follows it can only have come from the
injected function.
-}
fakeClassifierApp :: IO Application
fakeClassifierApp = application (mkServerConfig [mountAt ("npm" :| []) fakeClassify]) <$> newTestEnv
  where
    -- A deliberately non-npm grammar: @beep@ is the (locally answered) Ping route,
    -- every other path is denied. npm's @is-odd@ would be a Packument; here it is
    -- Unsupported, so the two grammars give observably different routes. The grammar
    -- ignores the method (it recognises no write route), proving dispatch routes
    -- through the injected method-aware classifier.
    fakeClassify :: Classifier
    fakeClassify _method ["beep"] = Ping
    fakeClassify _method _ = Unsupported

{- | The 'application' with __no mounts__: every path but the control-plane health
probes matches no mount and is the neutral @404@.
-}
neutralApp :: IO Application
neutralApp = application (mkServerConfig []) <$> newTestEnv

{- | An npm-mount 'application' whose 'DrainSignal' is __already raised__, standing
in for an instance mid-graceful-shutdown without binding a socket. Used to assert
the front door's draining behaviour: the readiness flip and the going-away header.
-}
drainingApp :: IO Application
drainingApp = do
    drain <- raisedDrain
    application (mkServerConfig [mountAt ("npm" :| []) Npm.classify]){scDrain = drain} <$> newTestEnv

-- | A live 'DrainSignal' raised into the draining state.
raisedDrain :: IO DrainSignal
raisedDrain = do
    drain <- newDrainSignal
    beginDrain drain
    pure drain

{- | An npm-mount 'application' whose worker heartbeat is __stale__: its last
successful poll is recorded well past 'workerHeartbeatStaleAfter' ago, standing in
for a single-process worker whose consume loop has gone quiet. Used to drive the
liveness probe to its @503@ "worker stalled" arm -- the single-process liveness
signal the front door folds the worker heartbeat into.
-}
stalledWorkerApp :: IO Application
stalledWorkerApp = do
    env <- newTestEnv
    now <- getCurrentTime
    -- A poll older than the staleness threshold: the loop has not advanced its
    -- heartbeat within the window, so liveness must read it as stalled.
    recordPoll (envWorkerHeartbeat env) (addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now)
    pure (application (mkServerConfig [mountAt ("npm" :| []) Npm.classify]) env)

{- | A header matcher that passes only when the response carries __no__
@Connection@ header -- the not-draining expectation, the complement of the
going-away assertion. (@hspec-wai@'s '<:>' only asserts a header is present.)
-}
matchNoConnectionHeader :: MatchHeader
matchNoConnectionHeader = MatchHeader $ \headers _body ->
    if any ((== hConnection) . fst) headers
        then Just "expected no Connection header, but one was present"
        else Nothing

{- | The server middleware stack wrapping a body-reading application under a tiny
request-body cap. The size-limit middleware rejects an over-cap body only once a
handler reads it, so the inner app strictly consumes the body -- exercising the cap.
The 'realIp' and 'timeout' middleware are part of the same stack.
-}
cappedApp :: Application
cappedApp = serverMiddleware (mkServerConfig []){scSizeLimit = RequestSizeLimit 8} echoBody
  where
    echoBody :: Application
    echoBody req respond = do
        _ <- consumeRequestBodyStrict req
        respond (responseLBS status200 [] "read")

{- | Drive a POST with the given body through an 'Application' and return its
status code. The request is marked 'ChunkedBody' (rather than a known length) so
the size-limit middleware applies its streaming byte-count check as the body is
read -- the path @hspec-wai@'s @request@, which fixes @requestBodyLength@ at a known
zero, cannot reach. (@srequest@ supplies the body chunks from the 'LByteString'.)
-}
statusForBody :: Application -> LByteString -> IO Int
statusForBody app body = do
    let req =
            (setPath defaultRequest{requestMethod = methodPost} "/")
                { requestBodyLength = ChunkedBody
                }
    response <- runSession (srequest (SRequest req body)) app
    pure (statusCode (simpleStatus response))

spec :: Spec
spec = do
    perimeterGuardSpec
    describe "control-plane health probes (above any mount)" $
        with npmMountApp $ do
            it "answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

    describe "liveness -- worker-stall arm of /livez" $
        with stalledWorkerApp $ do
            it "fails /livez with 503 once the worker heartbeat is stale" $
                -- The single-process liveness signal folds in the mirror worker's
                -- consume-loop heartbeat: a loop quiet past the staleness threshold is a
                -- genuine stall, so liveness must flip to 503 (fail-stop visibility) even
                -- though the HTTP front door itself is still serving.
                get "/livez" `shouldRespondWith` 503

            it "keeps /readyz at 200 (readiness ignores worker staleness; it is not draining)" $
                -- Readiness is about whether to route NEW traffic, gated only on the drain
                -- signal -- not on worker liveness. A stalled worker fails /livez, never
                -- /readyz, so the two probes stay independent.
                get "/readyz" `shouldRespondWith` 200

    describe "graceful shutdown -- readiness flip while draining" $
        with drainingApp $ do
            it "fails /readyz with 503 (the LB stops routing new traffic here)" $
                get "/readyz" `shouldRespondWith` 503

            it "keeps /livez at 200 (a draining instance is alive, not unhealthy)" $
                get "/livez" `shouldRespondWith` 200

    describe "graceful shutdown -- going-away header" $ do
        with drainingApp $
            it "stamps Connection: close on a response while draining" $
                -- A keep-alive pool (a client's, or a mesh's) must not reuse a socket
                -- on a closing instance; the header is what tells it to close.
                get "/npm/-/ping"
                    `shouldRespondWith` 200{matchHeaders = ["Connection" <:> "close"]}

        with npmMountApp $
            it "adds no Connection header when not draining" $
                get "/npm/-/ping" `shouldRespondWith` 200{matchHeaders = [matchNoConnectionHeader]}

    describe "meta-routes under a mount" $
        with npmMountApp $ do
            it "answers /npm/-/ping locally with 200 {}" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "answers /npm/-/v1/search with 501 (search is not an install path)" $
                get "/npm/-/v1/search" `shouldRespondWith` 501

    describe "dispatch -- /npm mount (prefix strip + npm grammar)" $
        with npmMountApp $ do
            it "recognises a packument route but does not yet serve it (501, not a fake 200)" $
                get "/npm/is-odd" `shouldRespondWith` 501

            it "recognises a tarball route but does not yet serve it (501)" $
                get "/npm/is-odd/-/is-odd-3.0.1.tgz" `shouldRespondWith` 501

            it "accepts the bare mount prefix with a trailing slash (empty path → 404)" $
                get "/npm/" `shouldRespondWith` 404

            it "normalises repeated trailing slashes the same as one (empty path → 404)" $
                -- @/npm//@ collapses its run of trailing empty segments to the bare mount,
                -- exactly like @/npm/@, rather than leaving a spurious empty path component.
                get "/npm//" `shouldRespondWith` 404

            it "leaves an internal empty segment for the router to reject (404, not collapsed)" $
                -- Only /trailing/ empties are dropped: @/npm//is-odd@ keeps its leading empty
                -- segment, so it stays an unrecognised path (404) rather than normalising to the
                -- @/npm/is-odd@ packument route.
                get "/npm//is-odd" `shouldRespondWith` 404

            it "404s an unknown /-/… meta-route under the mount" $
                get "/npm/-/whoami" `shouldRespondWith` 404

            it "404s a hostile traversal path rather than routing it" $
                -- @%2F@ decodes to one segment carrying a slash; the router denies it.
                get "/npm/foo%2Fbar" `shouldRespondWith` 404

    describe "first-party publish path (PUT /{pkg})" $ do
        with npmMountApp $
            it "405s a publish when no publication target is configured (the opt-in is off)" $
                -- No publish dependencies are wired on this mount, so there is no implicit
                -- write path; a PUT /{pkg} is Method Not Allowed.
                request methodPut "/npm/widget" [] "" `shouldRespondWith` 405

        with publishMountApp $ do
            it "refuses an out-of-scope publish with 403, before any upstream write (anti-shadowing)" $
                -- @other is outside the @acme allow-list, so the guard fires before the
                -- relay: the unconnectable target is never contacted (a 403, not the 502 an
                -- attempted write to it would yield).
                request methodPut "/npm/@other/widget" [] "" `shouldRespondWith` 403

            it "refuses an unscoped publish with 403 (an unscoped name is within no scope)" $
                request methodPut "/npm/widget" [] "" `shouldRespondWith` 403

            it "lets an in-scope publish through the guard to the relay (502 when the target is unreachable)" $
                -- @acme is in scope, so the guard admits the publish and the relay is
                -- attempted; the target is unconnectable, so it fails with 502 -- proving the
                -- guard let the write through rather than refusing it at the scope check.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 502

            -- The body-name agreement leg of the anti-shadowing guard (issue #391). The
            -- URL @acme/widget is in scope, but the document body declares a DIFFERENT name,
            -- so a relay would write a name the scope guard never authorised. The refusal is
            -- a 403 BEFORE the relay -- distinguishable here from the 502 an attempted write
            -- to the unconnectable target would yield, which is the proof the relay never ran.
            it "refuses an in-scope publish whose body _id / name disagree with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@victim/target\",\"name\":\"@victim/target\",\"versions\":{}}" `shouldRespondWith` 403

            it "refuses an in-scope publish whose body versions[].name disagrees with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@victim/target\",\"version\":\"1.0.0\"}}}" `shouldRespondWith` 403

            it "lets an in-scope publish whose body _id / name / versions[].name all agree with the URL through to the relay (502 when unreachable)" $
                -- A body whose every declared name matches the URL is not over-refused: it
                -- reaches the relay (502 to the unconnectable target), not a 403.
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@acme/widget\",\"version\":\"1.0.0\"}}}" `shouldRespondWith` 502

        with (publishAppWith basePublishDeps{pubRelayPublish = \_ _ _ _ _ _ -> throwIO (RelayContractEscape "simulated relay contract escape")}) $
            it "answers a relay contract escape with the perimeter's mount-shaped 500 (not a torn session, not a 502)" $
                -- The relay reports its failures as typed values, so a throw here is
                -- an invariant break. The typed request perimeter must answer it:
                -- the session survives with the neutral 500 -- not the 502 a
                -- classified relay fault renders, and not a session abort.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 500

        with (publishAppWith basePublishDeps{pubTargetUrl = ""}) $
            it "500s an in-scope publish when the publication target URL is unformable (misconfig)" $
                -- An empty target URL cannot form a request, a configuration fault rather
                -- than a transient outage, so the publish is a 500 (not a 502).
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 500

        with (publishAppWith basePublishDeps{pubInboundToken = Just (mkSecret "edge-token")}) $ do
            it "401s a publish that fails the edge token gate (before the scope guard)" $
                -- With an edge token configured, a publish carrying none is rejected at the
                -- edge -- the same gate the read paths apply.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 401

    describe "the two response tiers (neutral above mounts, mount renderer within)" $
        with npmMountApp $ do
            it "renders an UNMOUNTED path as a neutral text/plain 404" $
                -- No mount matches @/pypi/...@; there is no ecosystem to shape it, so
                -- the body is the generic plain-text Not Found, not an npm error object.
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unrecognised IN-MOUNT path through the mount's npm renderer" $
                -- @/npm/is-odd/3.0.1@ is under the npm mount but not a recognised npm
                -- path, so its 404 body is the npm {\"error\": …} object -- the mount's
                -- surface, distinct from the neutral plain-text 404 above.
                get "/npm/is-odd/3.0.1" `shouldRespondWith` "{\"error\":\"not found\"}"{matchStatus = 404}

    describe "dispatch -- injected classifier (the routing boundary)" $
        -- Drive dispatch with a FAKE classifier (not npm's): the route a request
        -- takes must follow the injected function, proving the web layer is not
        -- hardwired to npm's grammar.
        with fakeClassifierApp $ do
            it "routes the fake classifier's recognised path (/npm/beep → Ping → 200 {})" $
                get "/npm/beep" `shouldRespondWith` "{}"{matchStatus = 200}

            it "denies a path npm would accept but the fake does not (/npm/is-odd → 404)" $
                -- Under npm's grammar @is-odd@ is a Packument (501); under the injected
                -- fake it is Unsupported (404). The 404 proves dispatch followed the
                -- injected function, not a baked-in npm router.
                get "/npm/is-odd" `shouldRespondWith` 404

            it "denies npm's ping meta-route (the fake's grammar does not recognise it)" $
                get "/npm/-/ping" `shouldRespondWith` 404

    describe "no mounts -- neutral by default" $
        -- With no mount wired, the web layer serves nothing but the health probes;
        -- every other path matches no mount and is the neutral 404.
        with neutralApp $ do
            it "404s a package-shaped path (no mount is configured)" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "still answers the control-plane health probes (above any mount)" $ do
                get "/livez" `shouldRespondWith` 200
                get "/readyz" `shouldRespondWith` 200

    describe "middleware -- request size limit" $ do
        it "rejects a request body over the cap with 413" $
            -- The body exceeds the 8-byte cap; reading it trips the size-limit
            -- middleware, which answers 413 rather than letting the handler buffer
            -- an unbounded body.
            statusForBody cappedApp "this body is well over eight bytes"
                `shouldReturn` 413

        it "passes a request whose body is within the cap through to the handler" $
            statusForBody cappedApp "tiny" `shouldReturn` 200

    describe "mkServerConfig -- defaults" $ do
        it "listens on the conventional npm proxy port" $
            scPort (mkServerConfig []) `shouldBe` defaultPort

        it "the default port is 4873" $
            defaultPort `shouldBe` 4873

        it "caps the request body at 25 MiB by default" $
            scSizeLimit (mkServerConfig []) `shouldBe` RequestSizeLimit (25 * 1024 * 1024)

        it "defaults the graceful-drain timeout to 30 seconds" $
            scDrainTimeout (mkServerConfig []) `shouldBe` defaultShutdownDrainTimeout

        it "the default graceful-drain timeout is 30 seconds" $
            defaultShutdownDrainTimeout `shouldBe` ShutdownDrainTimeout 30

        it "path-mounts a binding under its prefix (never the root)" $
            bindingPrefix (mountAt ("npm" :| []) Npm.classify) `shouldBe` "npm" :| []

-- | A typed stand-in for a relay implementation escaping its typed contract.
newtype RelayContractEscape = RelayContractEscape Text
    deriving stock (Show)

instance Exception RelayContractEscape

{- | Drive 'perimeterGuard' over a recording respond and observation channel:
returns the statuses answered (in order), the classified causes observed, and
the guard's own outcome (a rethrow arrives as 'Left').
-}
driveGuard :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO ([Int], [RequestFaultCause], Either SomeException ())
driveGuard handler = do
    responses <- newIORef []
    observed <- newIORef []
    let respond response = do
            modifyIORef' responses (<> [statusCode (responseStatus response)])
            pure ResponseReceived
    outcome <- try (void (perimeterGuard (\fault -> modifyIORef' observed (<> [rqCause fault])) npmRenderer respond handler))
    (,,) <$> readIORef responses <*> readIORef observed <*> pure outcome

perimeterGuardSpec :: Spec
perimeterGuardSpec = describe "perimeterGuard (the typed request perimeter)" $ do
    it "passes a committed response through untouched, observing nothing" $ do
        (statuses, observed, outcome) <- driveGuard (\respond -> respond (responseLBS status200 [] "ok"))
        statuses `shouldBe` [200]
        observed `shouldBe` []
        outcome `shouldSatisfy` isRight

    it "answers a recognised pre-commit escape with the neutral 500, observed as a GateFault" $ do
        (statuses, observed, _) <- driveGuard (\_respond -> throwIO RegistryUnconfigured)
        statuses `shouldBe` [500]
        observed `shouldBe` [GateFault]

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

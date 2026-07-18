-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ServerSpec (spec) where

import Prelude hiding (get)

import Network.HTTP.Types (hConnection, methodHead, methodPut, status200, status500, statusCode)
import Network.Wai (
    Application,
    Response,
    ResponseReceived,
    responseLBS,
    responseStatus,
 )
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO.Exception (throwIO, try)

import Data.Time (addUTCTime, getCurrentTime)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Fault (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..), relayPublishDocument)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Publish qualified as NpmPublish
import Ecluse.Core.Registry.Npm.Route (npmNotFound, npmRouter)
import Ecluse.Core.Security (LimitError (BodyTooLarge), defaultLimits)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission)
import Ecluse.Core.Server.Context (MountRouter, PublishDeps (..), ResponseAction (AnswerLocally), RouteAction (RouteAction))
import Ecluse.Core.Server.Contract (ResponseContract, VariableResponse, variableOpaqueContract, variableResponse)
import Ecluse.Core.Server.Fault (RequestFault (rqCause))
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (GateFault, UnclassifiedFault))
import Ecluse.Core.Worker (heartbeatHealthyNow, workerHeartbeatStaleAfter)
import Ecluse.Runtime.Env (envWorkerHeartbeat, recordPoll)
import Ecluse.Runtime.Server (
    DrainSignal,
    MountBinding (..),
    ServerConfig (..),
    ShutdownDrainTimeout (..),
    application,
    beginDrain,
    defaultPort,
    defaultShutdownDrainTimeout,
    mkServerConfig,
    newDrainSignal,
    perimeterGuard,
 )
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | A registry-handle double whose effectful fields are never invoked: the web
layer only routes, classifies, and renders -- it never fetches -- so a handle that
refuses loudly is enough to assemble an 'Env'. If a route reached an effectful
field, the refusal would surface the leak.
-}

-- | A credential-handle double: a fixed, non-expiring token, never read here.

{- | A test mount binding: the given prefix and router, and __inert__ packument-serve
dependencies (every upstream a closed port). A bound mount always
carries them, so these specs supply the fixture and simply do not drive the data plane: they
exercise routing, the meta-routes, the prefix strip, and the publish path.
-}
mountAt :: NonEmpty Text -> MountRouter -> MountBinding
mountAt prefix router =
    publishMountAt prefix router Nothing

{- | A test mount binding like 'mountAt' but with the given (optional) first-party
publish dependencies -- 'Nothing' leaves a @PUT \/{pkg}@ the @405@ opt-out, 'Just'
enables the publish path (the scope guard and the relay).
-}
publishMountAt :: NonEmpty Text -> MountRouter -> Maybe PublishDeps -> MountBinding
publishMountAt prefix router publishDeps =
    MountBinding
        { bindingPrefix = prefix
        , bindingRouter = router
        , bindingPackumentDeps = inertPackumentDeps
        , bindingPublishDeps = publishDeps
        }

{- | The 'application' under a single @\/npm@ mount carrying npm's path grammar, so
prefix-strip dispatch and the npm routing table can be asserted through the real
router the composition root wires in.
-}
npmMountApp :: IO Application
npmMountApp = application (mkServerConfig [mountAt ("npm" :| []) npmRouter]) <$> newTestEnv

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
publish reaches the relay and fails to connect -- a @502@). A function over the
body-byte budget: the aggregate admission is allocated per application
('publishAppWith'), so the pure fixture cannot carry it.
-}
basePublishDeps :: ByteAdmission -> PublishDeps
basePublishDeps bodyBudget =
    PublishDeps
        { pubTargetUrl = "http://127.0.0.1:1" -- an unconnectable port
        , pubScopes = [mkScope "acme"]
        , pubStaticToken = Nothing
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits
        , pubBodyBudget = bodyBudget
        , pubMaxRequestBytes = 26214400
        , pubHelp = Nothing
        , pubRelayPublish = \l m t s -> relayPublishDocument (NpmClientConfig t m s l)
        , pubCanonicaliseName = rightToMaybe . Project.projectName
        , pubDeclaredNames = NpmPublish.declaredNames
        }

{- | The 'application' under a single @\/npm@ mount carrying the given publish deps,
handed a generously-sized body-byte budget these routing tests never contend on.
-}
publishAppWith :: (ByteAdmission -> PublishDeps) -> IO Application
publishAppWith mkDeps = do
    bodyBudget <- newByteAdmission (128 * 1024 * 1024)
    application (mkServerConfig [publishMountAt ("npm" :| []) npmRouter (Just (mkDeps bodyBudget))]) <$> newTestEnv

{- | The 'application' under a single @\/npm@ mount whose router is a __fake__ (not
npm's), proving dispatch follows the binding's router rather than any hardwired
grammar. The fake recognises a single sentinel path and denies everything else, so a
response that follows it can only have come from the injected function.
-}
fakeRouterApp :: IO Application
fakeRouterApp = application (mkServerConfig [mountAt ("npm" :| []) fakeRouter]) <$> newTestEnv
  where
    -- A deliberately non-npm router: @beep@ is answered locally with @200 {}@, and
    -- every other path is the deny-by-default @404@. npm's @is-odd@ would be a
    -- packument read; under this router it is a miss, so the two give observably
    -- different answers. It ignores the method (it names no write action), proving the
    -- web layer follows the injected router rather than a baked-in npm grammar.
    fakeRouter :: MountRouter
    fakeRouter _method ["beep"] = RouteAction fakeContract (AnswerLocally (variableResponse status200 [] "{}"))
    fakeRouter _method _ = npmNotFound

    fakeContract :: ResponseContract (VariableResponse LByteString)
    fakeContract = variableOpaqueContract "application/json" "The fake router's response."

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
    application (mkServerConfig [mountAt ("npm" :| []) npmRouter]){scDrain = drain} <$> newTestEnv

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
    -- The composition root folds the heartbeat into /livez only when a worker
    -- runs; this fixture models that mirrored-deployment wiring explicitly.
    let cfg = (mkServerConfig [mountAt ("npm" :| []) npmRouter]){scCheckLive = heartbeatHealthyNow (envWorkerHeartbeat env)}
    pure (application cfg env)

{- | A header matcher that passes only when the response carries __no__
@Connection@ header -- the not-draining expectation, the complement of the
going-away assertion. (@hspec-wai@'s '<:>' only asserts a header is present.)
-}
matchNoConnectionHeader :: MatchHeader
matchNoConnectionHeader = MatchHeader $ \headers _body ->
    if any ((== hConnection) . fst) headers
        then Just "expected no Connection header, but one was present"
        else Nothing

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

        with (publishAppWith (\b -> (basePublishDeps b){pubRelayPublish = \_ _ _ _ _ _ -> throwIO (RelayContractEscape "simulated relay contract escape")})) $
            it "answers a relay contract escape with the route's declared 500 (not a torn session, not a 502)" $
                -- The relay reports its failures as typed values, so a throw here is
                -- an invariant break. The typed request perimeter must answer it:
                -- the session survives with the neutral 500 -- not the 502 a
                -- classified relay fault renders, and not a session abort.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 500

        with (publishAppWith (\b -> (basePublishDeps b){pubTargetUrl = ""})) $
            it "500s an in-scope publish when the publication target URL is unformable (misconfig)" $
                -- An empty target URL cannot form a request, a configuration fault rather
                -- than a transient outage, so the publish is a 500 (not a 502).
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 500

        with (publishAppWith (\b -> (basePublishDeps b){pubInboundToken = Just (mkSecret "edge-token")})) $ do
            it "401s a publish that fails the edge token gate (before the scope guard)" $
                -- With an edge token configured, a publish carrying none is rejected at the
                -- edge -- the same gate the read paths apply.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 401

    describe "the two response tiers (neutral above mounts, route contract within)" $
        with npmMountApp $ do
            it "renders an UNMOUNTED path as a neutral text/plain 404" $
                -- No mount matches @/pypi/...@; there is no ecosystem to shape it, so
                -- the body is the generic plain-text Not Found, not an npm error object.
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unrecognised IN-MOUNT path through npm's fallback contract" $
                -- @/npm/is-odd/3.0.1@ is under the npm mount but not a recognised npm
                -- path, so its 404 body is the npm {\"error\": …} object -- the mount's
                -- surface, distinct from the neutral plain-text 404 above.
                get "/npm/is-odd/3.0.1" `shouldRespondWith` "{\"error\":\"not found\"}"{matchStatus = 404}

            it "derives a bodiless response for an unrecognised HEAD path" $
                request methodHead "/npm/is-odd/3.0.1" [] ""
                    `shouldRespondWith` ""{matchStatus = 404}

    describe "dispatch -- injected router (the routing boundary)" $
        -- Drive dispatch with a FAKE router (not npm's): the action a request takes
        -- must follow the injected function, proving the web layer is not hardwired to
        -- npm's grammar.
        with fakeRouterApp $ do
            it "routes the fake router's recognised path (/npm/beep → answered locally → 200 {})" $
                get "/npm/beep" `shouldRespondWith` "{}"{matchStatus = 200}

            it "denies a path npm would accept but the fake does not (/npm/is-odd → 404)" $
                -- Under npm's router @is-odd@ is a packument read; under the injected fake
                -- it is a miss (404). The 404 proves dispatch followed the injected
                -- function, not a baked-in npm router.
                get "/npm/is-odd" `shouldRespondWith` 404

            it "denies npm's ping meta-route (the fake router does not recognise it)" $
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

    describe "mkServerConfig -- defaults" $ do
        it "listens on the conventional npm proxy port" $
            scPort (mkServerConfig []) `shouldBe` defaultPort

        it "the default port is 4873" $
            defaultPort `shouldBe` 4873

        it "defaults the graceful-drain timeout to 30 seconds" $
            scDrainTimeout (mkServerConfig []) `shouldBe` defaultShutdownDrainTimeout

        it "the default graceful-drain timeout is 30 seconds" $
            defaultShutdownDrainTimeout `shouldBe` ShutdownDrainTimeout 30

        it "path-mounts a binding under its prefix (never the root)" $
            bindingPrefix (mountAt ("npm" :| []) npmRouter) `shouldBe` "npm" :| []

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
    outcome <- try (void (perimeterGuard (\fault -> modifyIORef' observed (<> [rqCause fault])) respond fallback handler))
    (,,) <$> readIORef responses <*> readIORef observed <*> pure outcome
  where
    fallback = responseLBS status500 [] "internal server error"

perimeterGuardSpec :: Spec
perimeterGuardSpec = describe "perimeterGuard (the typed request perimeter)" $ do
    it "passes a committed response through untouched, observing nothing" $ do
        (statuses, observed, outcome) <- driveGuard (\respond -> respond (responseLBS status200 [] "ok"))
        statuses `shouldBe` [200]
        observed `shouldBe` []
        outcome `shouldSatisfy` isRight

    it "answers a recognised pre-commit escape with the neutral 500, observed as a GateFault" $ do
        (statuses, observed, _) <- driveGuard (\_respond -> throwIO (ResponseBoundExceeded (BodyTooLarge 1024)))
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

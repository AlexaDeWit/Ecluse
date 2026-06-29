module Ecluse.ServerSpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (hConnection, methodPost, methodPut, status200, statusCode)
import Network.Wai (
    Application,
    Request (requestBodyLength, requestMethod),
    RequestBodyLength (ChunkedBody),
    consumeRequestBodyStrict,
    defaultRequest,
    responseLBS,
 )
import Network.Wai.Test (SRequest (SRequest), runSession, setPath, simpleStatus, srequest)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)
import UnliftIO.Timeout (timeout)

import Data.Time (addUTCTime, getCurrentTime)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..), relayPublishDocument)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PublishDeps (..))
import Ecluse.Core.Server.Route (Classifier, Route (..))
import Ecluse.Core.Worker (workerHeartbeatStaleAfter)
import Ecluse.Env (Env, envWorkerHeartbeat, newEnv, newWorkerHeartbeat, recordPoll)
import Ecluse.Server (
    DrainSignal,
    InteractiveHalt (..),
    MountBinding (..),
    RequestSizeLimit (..),
    ServerConfig (..),
    ShutdownDrainTimeout (..),
    application,
    beginDrain,
    defaultPort,
    defaultShutdownDrainTimeout,
    isDraining,
    mkServerConfig,
    neverDraining,
    newDrainSignal,
    serverMiddleware,
    withInteractiveHalt,
 )
import Ecluse.Telemetry (telemetryDisabled)

{- | A registry-handle double whose effectful fields are never invoked: the web
layer only routes, classifies, and renders — it never fetches — so a handle that
refuses loudly is enough to assemble an 'Env'. If a route reached an effectful
field, the refusal would surface the leak.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const unused
        , fetchArtifact = \_ _ -> unused
        , publishArtifact = \_ _ _ -> unused
        , parsePackageInfo = \_ _ -> Left unusedParse
        , parseVersionDetails = \_ _ -> Left unusedParse
        , parseVersionList = const (Left unusedParse)
        }
  where
    unused :: IO a
    unused = throwString "fakeRegistry: the web layer must not fetch when only routing"

    unusedParse :: ParseError
    unusedParse = ParseError "fakeRegistry: the web layer must not parse when only routing"

-- | A credential-handle double: a fixed, non-expiring token, never read here.
fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "server-spec", authExpiresAt = Nothing}

-- | A manager with no TLS and no connection opened on construction.
newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

-- | Assemble an 'Env' from the handle doubles, touching no network.
newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newTestManager
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | A test mount binding: the given prefix and classifier, npm's denial renderer,
and no packument-serve dependencies (so a 'Packument' route is the recognised-but-
unserved @501@ stub).
-}
mountAt :: NonEmpty Text -> Classifier -> MountBinding
mountAt prefix classifier =
    publishMountAt prefix classifier Nothing

{- | A test mount binding like 'mountAt' but with the given (optional) first-party
publish dependencies — 'Nothing' leaves a @PUT \/{pkg}@ the @405@ opt-out, 'Just'
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
attempts the relay (which then fails to connect — a @502@, proving the write was
reached), while an out-of-scope publish is refused at the guard (a @403@) before any
connection — the assertion that the guard fires before any upstream write.
-}
publishMountApp :: IO Application
publishMountApp = publishAppWith basePublishDeps

{- | The base first-party publish dependencies the publish tests build on: an @\@acme@
scope allow-list and a publication target at an unconnectable port (so an in-scope
publish reaches the relay and fails to connect — a @502@).
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
liveness probe to its @503@ "worker stalled" arm — the single-process liveness
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
@Connection@ header — the not-draining expectation, the complement of the
going-away assertion. (@hspec-wai@'s '<:>' only asserts a header is present.)
-}
matchNoConnectionHeader :: MatchHeader
matchNoConnectionHeader = MatchHeader $ \headers _body ->
    if any ((== hConnection) . fst) headers
        then Just "expected no Connection header, but one was present"
        else Nothing

{- | The server middleware stack wrapping a body-reading application under a tiny
request-body cap. The size-limit middleware rejects an over-cap body only once a
handler reads it, so the inner app strictly consumes the body — exercising the cap.
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
read — the path @hspec-wai@'s @request@, which fixes @requestBodyLength@ at a known
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
    describe "control-plane health probes (above any mount)" $
        with npmMountApp $ do
            it "answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

    describe "liveness — worker-stall arm of /livez" $
        with stalledWorkerApp $ do
            it "fails /livez with 503 once the worker heartbeat is stale" $
                -- The single-process liveness signal folds in the mirror worker's
                -- consume-loop heartbeat: a loop quiet past the staleness threshold is a
                -- genuine stall, so liveness must flip to 503 (fail-stop visibility) even
                -- though the HTTP front door itself is still serving.
                get "/livez" `shouldRespondWith` 503

            it "keeps /readyz at 200 (readiness ignores worker staleness; it is not draining)" $
                -- Readiness is about whether to route NEW traffic, gated only on the drain
                -- signal — not on worker liveness. A stalled worker fails /livez, never
                -- /readyz, so the two probes stay independent.
                get "/readyz" `shouldRespondWith` 200

    describe "graceful shutdown — readiness flip while draining" $
        with drainingApp $ do
            it "fails /readyz with 503 (the LB stops routing new traffic here)" $
                get "/readyz" `shouldRespondWith` 503

            it "keeps /livez at 200 (a draining instance is alive, not unhealthy)" $
                get "/livez" `shouldRespondWith` 200

    describe "graceful shutdown — going-away header" $ do
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

    describe "dispatch — /npm mount (prefix strip + npm grammar)" $
        with npmMountApp $ do
            it "recognises a packument route but does not yet serve it (501, not a fake 200)" $
                get "/npm/is-odd" `shouldRespondWith` 501

            it "recognises a tarball route but does not yet serve it (501)" $
                get "/npm/is-odd/-/is-odd-3.0.1.tgz" `shouldRespondWith` 501

            it "accepts the bare mount prefix with a trailing slash (empty path → 404)" $
                get "/npm/" `shouldRespondWith` 404

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
                -- attempted; the target is unconnectable, so it fails with 502 — proving the
                -- guard let the write through rather than refusing it at the scope check.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 502

            -- The body-name agreement leg of the anti-shadowing guard (issue #391). The
            -- URL @acme/widget is in scope, but the document body declares a DIFFERENT name,
            -- so a relay would write a name the scope guard never authorised. The refusal is
            -- a 403 BEFORE the relay — distinguishable here from the 502 an attempted write
            -- to the unconnectable target would yield, which is the proof the relay never ran.
            it "refuses an in-scope publish whose body _id / name disagree with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@victim/target\",\"name\":\"@victim/target\",\"versions\":{}}" `shouldRespondWith` 403

            it "refuses an in-scope publish whose body versions[].name disagrees with the URL with 403, before any relay" $
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@victim/target\",\"version\":\"1.0.0\"}}}" `shouldRespondWith` 403

            it "lets an in-scope publish whose body _id / name / versions[].name all agree with the URL through to the relay (502 when unreachable)" $
                -- A body whose every declared name matches the URL is not over-refused: it
                -- reaches the relay (502 to the unconnectable target), not a 403.
                request methodPut "/npm/@acme/widget" [] "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@acme/widget\",\"version\":\"1.0.0\"}}}" `shouldRespondWith` 502

        with (publishAppWith basePublishDeps{pubTargetUrl = ""}) $
            it "500s an in-scope publish when the publication target URL is unformable (misconfig)" $
                -- An empty target URL cannot form a request, a configuration fault rather
                -- than a transient outage, so the publish is a 500 (not a 502).
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 500

        with (publishAppWith basePublishDeps{pubInboundToken = Just (mkSecret "edge-token")}) $ do
            it "401s a publish that fails the edge token gate (before the scope guard)" $
                -- With an edge token configured, a publish carrying none is rejected at the
                -- edge — the same gate the read paths apply.
                request methodPut "/npm/@acme/widget" [] "" `shouldRespondWith` 401

    describe "the two response tiers (neutral above mounts, mount renderer within)" $
        with npmMountApp $ do
            it "renders an UNMOUNTED path as a neutral text/plain 404" $
                -- No mount matches @/pypi/...@; there is no ecosystem to shape it, so
                -- the body is the generic plain-text Not Found, not an npm error object.
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unrecognised IN-MOUNT path through the mount's npm renderer" $
                -- @/npm/is-odd/3.0.1@ is under the npm mount but not a recognised npm
                -- path, so its 404 body is the npm {\"error\": …} object — the mount's
                -- surface, distinct from the neutral plain-text 404 above.
                get "/npm/is-odd/3.0.1" `shouldRespondWith` "{\"error\":\"not found\"}"{matchStatus = 404}

    describe "dispatch — injected classifier (the routing boundary)" $
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

    describe "no mounts — neutral by default" $
        -- With no mount wired, the web layer serves nothing but the health probes;
        -- every other path matches no mount and is the neutral 404.
        with neutralApp $ do
            it "404s a package-shaped path (no mount is configured)" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "still answers the control-plane health probes (above any mount)" $ do
                get "/livez" `shouldRespondWith` 200
                get "/readyz" `shouldRespondWith` 200

    describe "middleware — request size limit" $ do
        it "rejects a request body over the cap with 413" $
            -- The body exceeds the 8-byte cap; reading it trips the size-limit
            -- middleware, which answers 413 rather than letting the handler buffer
            -- an unbounded body.
            statusForBody cappedApp "this body is well over eight bytes"
                `shouldReturn` 413

        it "passes a request whose body is within the cap through to the handler" $
            statusForBody cappedApp "tiny" `shouldReturn` 200

    describe "mkServerConfig — defaults" $ do
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

    describe "DrainSignal" $ do
        it "a fresh signal is not draining" $ do
            drain <- newDrainSignal
            isDraining drain `shouldReturn` False

        it "beginDrain raises it" $ do
            drain <- newDrainSignal
            beginDrain drain
            isDraining drain `shouldReturn` True

        it "raising is idempotent (a second raise keeps it raised)" $ do
            drain <- newDrainSignal
            beginDrain drain
            beginDrain drain
            isDraining drain `shouldReturn` True

        it "neverDraining stays lowered even after a raise (the inert default)" $ do
            beginDrain neverDraining
            isDraining neverDraining `shouldReturn` False

    describe "withInteractiveHalt (local-dev quit key)" $ do
        -- The real wiring (TTY guard, stdin EOF, _exit) is process-global and not
        -- deterministically drivable in-process — the same boundary the OS-signal
        -- path has. So the three injection points are wired and the combinator's logic is
        -- tested: armed only when interactive, halts when the signal fires, and the
        -- watcher is torn down with the action (never fires after it returns).

        it "runs the action and never halts when NOT interactive (the production guard)" $ do
            halted <- newIORef False
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure False
                        , awaitHaltSignal = pass -- would fire immediately, but must be ignored
                        , halt = writeIORef halted True
                        }
            result <- withInteractiveHalt ih (pure "served")
            result `shouldBe` ("served" :: Text)
            -- No watcher was installed, so the halt path was never reached.
            readIORef halted `shouldReturn` False

        it "halts when interactive and the halt signal fires (Ctrl-D)" $ do
            halted <- newEmptyMVar
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure True
                        , awaitHaltSignal = pass -- stands in for an immediate stdin EOF
                        , halt = putMVar halted ()
                        }
            -- The action blocks; the watcher's signal fires at once and runs halt.
            -- (In production halt is _exit; here it records, so the test observes it.)
            outcome <- timeout 1_000_000 (withInteractiveHalt ih (void (threadDelay 5_000_000)))
            -- The action did not complete on its own (it was meant to be cut short);
            -- what matters is that halt ran.
            outcome `shouldBe` Nothing
            fired <- timeout 1_000_000 (takeMVar halted)
            fired `shouldBe` Just ()

        it "tears the watcher down with the action — halt never fires once the action returns" $ do
            halted <- newIORef False
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure True
                        , awaitHaltSignal = threadDelay 5_000_000 -- never fires within the test
                        , halt = writeIORef halted True
                        }
            -- The action completes promptly; 'withAsync' cancels the still-blocked
            -- watcher on the way out, so halt is never reached.
            result <- withInteractiveHalt ih (pure "done")
            result `shouldBe` ("done" :: Text)
            -- Give a cancelled watcher every chance to (wrongly) fire.
            threadDelay 50_000
            readIORef halted `shouldReturn` False

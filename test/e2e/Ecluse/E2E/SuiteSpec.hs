{- | The end-to-end scenarios, driven through the real @npm@ CLI against the real image.

__Per-test isolation is the assumed default.__ Every active case gets its __own fresh
environment__ ('withE2E' under 'around') — a freshly booted proxy + Verdaccio + nginx
stub on their own docker network — so each case starts from a pristine system and no case
can observe or disrupt another's harness state (a published mirror, a paused upstream, a
@SIGTERM@ed proxy). This is slower than sharing one environment, deliberately:
independence over speed. If the wall-clock bites, shard the cases across CI workers rather
than reintroduce shared state. When the environment is unavailable (no docker / image),
every case is reported @pending@ rather than failed.

See @planning\/slices\/S53-e2e-ecosystem.md@ for the design and the full scenario list.
Graceful-drain is @pending@ here — it tracks the #160 drain work; it is kept outside the
per-test 'around' so it boots no environment until it is implemented.
-}
module Ecluse.E2E.SuiteSpec (spec) where

import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.E2E.Fixtures (PkgSpec, allowPkg, denyPkg, headPkg, mirrorPkg, psName, psVersion, tamperPkg, telemetryPkg)
import Ecluse.E2E.Harness

shouldSucceed :: NpmResult -> IO ()
shouldSucceed res = case npmExit res of
    ExitSuccess -> pure ()
    _ -> expectationFailure $ "npm failed!\nSTDOUT:\n" <> T.unpack (npmStdout res) <> "\nSTDERR:\n" <> T.unpack (npmStderr res)

shouldFail :: NpmResult -> IO ()
shouldFail res = case npmExit res of
    ExitSuccess -> expectationFailure $ "npm incorrectly succeeded!\nSTDOUT:\n" <> T.unpack (npmStdout res) <> "\nSTDERR:\n" <> T.unpack (npmStderr res)
    _ -> pure ()

spec :: Spec
spec = do
    unavailable <- runIO e2eUnavailable
    case unavailable of
        Just reason -> it "end-to-end suite (environment unavailable)" (pendingWith reason)
        Nothing -> aroundAll withGlobalDataPlane $ do
            scenarios
            telemetryScenarios
            publishScenarios
            pendingScenarios

{- | The active scenarios. Under 'around' each @it@ gets its own freshly booted
environment, torn down before the next — per-test isolation (see the module header).
-}
scenarios :: SpecWith GlobalDataPlane
scenarios = aroundWith withE2E $ do
    describe "public surface — install and policy" $ do
        it "installs an allow-listed package end to end" $ \e2e -> do
            res <- npmInstall e2e (psName allowPkg)
            shouldSucceed res

        it "blocks a package that declares an install script, and never mirrors it" $ \e2e -> do
            res <- npmInstall e2e (psName denyPkg)
            shouldFail res
            mirrored <- verdaccioHasVersion e2e (psName denyPkg) (psVersion denyPkg)
            mirrored `shouldBe` False

        it "runs no package lifecycle script during a harness install (defence in depth)" $ \e2e -> do
            -- Distinct from the rules-engine block above: that proves the *proxy* refuses to
            -- serve a package declaring an install script; this proves our *own* npm CLI
            -- executes no lifecycle script even when one is present, the guard that closes the
            -- arbitrary-code-execution surface in our supply-chain-security tool's own CI. The
            -- probe project's own `postinstall` would create a sentinel; a successful install
            -- that creates none proves npm_config_ignore_scripts held.
            (installed, scriptRan) <- installWithLifecycleProbe e2e
            shouldSucceed installed
            scriptRan `shouldBe` False

    describe "server↔worker — the integrity gate" $
        it "refuses to mirror an artifact whose bytes fail the integrity gate" $ \e2e -> do
            -- A tarball request enqueues a mirror (S15 demand-driven). The proxy serves
            -- the bytes, but the worker's strongest-digest gate must reject them, so the
            -- tampered version never reaches the private mirror.
            _ <- proxyGet e2e (tarballPath tamperPkg)
            mirrored <- verdaccioHasVersion e2e (psName tamperPkg) (psVersion tamperPkg)
            mirrored `shouldBe` False

    describe "server↔worker — the full mirror lifecycle" $
        it "mirrors a package served from public, then installs it from the mirror with public down" $ \e2e -> do
            -- The core resilience loop, end to end, as a real upstream-outage scenario.
            -- A package absent from the private mirror but present on public is served
            -- from public (an `npm install` succeeds and writes a lockfile); the worker
            -- mirrors it to the private mirror; then, with the public upstream PAUSED, an
            -- `npm ci` from that lockfile still installs it. `npm ci` fetches the artifact
            -- from the lockfile's `resolved` URL (the proxy's private-first tarball path)
            -- without re-resolving via the packument, so it never touches public — the
            -- success proves the bytes came from the mirror, not a still-reachable public.
            let name = psName mirrorPkg
                ver = psVersion mirrorPkg
            presentBefore <- verdaccioHasVersionNow e2e name ver -- (1) a miss in the private mirror
            presentBefore `shouldBe` False
            withNpmProject e2e $ \proj -> do
                installed <- npmInstallIn proj name -- (2,3) served from public; writes the lockfile
                shouldSucceed installed
                mirrored <- verdaccioHasVersion e2e name ver -- (4) the worker mirrors it to private
                mirrored `shouldBe` True
                fromMirror <- withUpstreamPaused e2e (npmCiIn proj) -- (5) public down → from the mirror
                shouldSucceed fromMirror

    describe "protocol behaviours" $
        it "answers HEAD on a tarball with its size but no body, and enqueues no mirror" $ \e2e -> do
            -- A HEAD goes through the same gating as the GET path but probes the upstream
            -- as a HEAD and relays the headers with no body (#211/#269): it reports a
            -- Content-Length yet streams zero body bytes, and — serving no bytes to
            -- back-fill — enqueues no mirror. Driven on a package only ever HEADed (never
            -- installed/GET), so the empty mirror is attributable to the HEAD alone.
            (status, declared, bodyBytes) <- proxyHead e2e (tarballPath headPkg)
            status `shouldBe` 200
            bodyBytes `shouldBe` 0
            declared `shouldSatisfy` maybe False (> 0)
            mirrored <- verdaccioHasVersion e2e (psName headPkg) (psVersion headPkg)
            mirrored `shouldBe` False

{- | The whole-system telemetry scenarios. Each gets its __own__ freshly booted
environment with the telemetry topology it needs — an OTLP collector and the proxy's
telemetry dialect ('E2EConfig') — under its own 'around', still per-test isolated like
'scenarios'. The collector validation keys on the collector's @debug@ exporter output (no
Datadog SaaS); the stdout/log validation keys on the proxy container's own JSONL stream.
See @planning\/slices\/S53-e2e-ecosystem.md@.
-}
telemetryScenarios :: SpecWith GlobalDataPlane
telemetryScenarios = do
    -- #324 — real healthy OTLP publication: with telemetry on and an OTLP endpoint, a
    -- real npm request's ecluse.* metrics and its span actually reach a collector.
    describe "telemetry — OTLP healthy publication (#324)" $
        aroundWith (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}) $
            it "exports ecluse.* metrics and a span to the collector on a real npm request" $ \e2e -> do
                res <- npmInstall e2e (psName allowPkg)
                shouldSucceed res
                -- The serve path's catalogue metric and a request span both land in the
                -- collector's debug exporter: keyed on the catalogue metric name and the
                -- exporter's per-span marker, so both signals are proven received.
                delivered <-
                    awaitCollectorLog
                        e2e
                        (\logs -> "ecluse.serve.decision" `T.isInfixOf` logs && "Span #" `T.isInfixOf` logs)
                        80
                delivered `shouldBe` True

    -- #307(2) — domain-span emission proven end to end: a real mirror round-trip drives the
    -- hand-added domain spans (rule eval on the public tarball gate, the mirror-enqueue
    -- producer span, and the mirror-job consumer span the worker opens), and all three reach
    -- the collector's debug exporter — so the domain instrumentation is exercised end to end,
    -- not only the WAI/http-client spans the #324 case proves.
    describe "telemetry — domain-span emission (#307)" $
        aroundWith (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}) $
            it "emits the rule-eval, mirror-enqueue, and mirror-job domain spans to the collector on a mirror round-trip" $ \e2e -> do
                -- A public-served install gates the version (rule-eval span) and enqueues a
                -- mirror (enqueue span); the worker then mirrors it (job span).
                withNpmProject e2e $ \proj -> do
                    installed <- npmInstallIn proj (psName telemetryPkg)
                    shouldSucceed installed
                -- The worker mirrors asynchronously, so the mirror-job span lands after the
                -- install returns; the published mirror is the cue the job has run.
                mirrored <- verdaccioHasVersion e2e (psName telemetryPkg) (psVersion telemetryPkg)
                mirrored `shouldBe` True
                emitted <-
                    awaitCollectorLog
                        e2e
                        ( \logs ->
                            all
                                (`T.isInfixOf` logs)
                                ["ecluse.rule.eval", "ecluse.mirror.enqueue", "ecluse.mirror.job"]
                        )
                        120
                emitted `shouldBe` True

    -- #325(a) — OTLP absent / telemetry off: the real image still boots, serves a real
    -- install, and logs JSONL to stdout/stderr, with no collector anywhere.
    describe "telemetry — OTLP off, no collector (#325)" $
        aroundWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = [("ECLUSE_TELEMETRY", "off")]}) $
            it "starts, serves a real install, and logs JSONL to stdout — no collector needed" $ \e2e -> do
                res <- npmInstall e2e (psName allowPkg)
                shouldSucceed res
                -- It still writes structured JSONL to its stdout/stderr (docker captures
                -- both): await any log object (keyed on the `msg` field every katip JSONL
                -- line carries) — the worker's async publish line reliably provides one.
                logged <- awaitProxyLog e2e (T.isInfixOf "\"msg\":") 80
                logged `shouldBe` True

    -- #325(b) — OTLP on but the collector unreachable/absent: the SAME proxy config as the
    -- healthy #324 case, only the collector is never stood up (ecCollector = False), so its
    -- network alias does not resolve. The proxy must still boot, serve, and KEEP serving:
    -- the SDK's batch exporter fails asynchronously off the request path, so the absent
    -- collector can never take the proxy down or block a request. Écluse wraps the OTLP span
    -- and metric exporters (hs-opentelemetry 1.0.0.0 would otherwise drop the failed export
    -- silently), so the failure is OBSERVED and surfaced through katip under a throttle —
    -- the first failure is the "telemetry export error" line this asserts on, on top of the
    -- keeps-serving proof.
    describe "telemetry — OTLP on but the collector unreachable (#325)" $
        aroundWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = otlpCollectorEnv}) $
            it "surfaces a throttled export-failure warning yet keeps serving — an absent collector degrades visibly, no crash" $ \e2e -> do
                firstInstall <- npmInstall e2e (psName allowPkg)
                shouldSucceed firstInstall
                logged <- awaitProxyLog e2e (T.isInfixOf "\"msg\":") 80
                logged `shouldBe` True
                -- The first install's spans (1s batch flush) and metrics (1s reader) export
                -- and fail against the unreachable endpoint; the wrapped exporters route that
                -- failure through katip, so the throttle's first-failure warning lands in the
                -- proxy's JSONL — the operator signal that telemetry has stopped flowing.
                exportWarned <- awaitProxyLog e2e (T.isInfixOf "telemetry export error") 80
                exportWarned `shouldBe` True
                -- And it KEEPS serving: still ready and still serves a fresh install, proving
                -- the failed-and-surfaced export never took the proxy down or blocked a request.
                stillReady <- proxyStatus e2e "/readyz"
                stillReady `shouldBe` 200
                secondInstall <- npmInstall e2e (psName mirrorPkg)
                shouldSucceed secondInstall

    -- #323 — Datadog pattern: DD_SERVICE/DD_ENV/DD_VERSION (+ DD_AGENT_HOST) flow through
    -- the self-aligning resolver to Datadog unified-service-tag resource attributes on the
    -- exported signals and the dd object on the JSONL logs.
    describe "telemetry — Datadog pattern (#323)" $
        aroundWith (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = datadogCollectorEnv}) $
            it "carries the Datadog unified-service tags to the collector and the dd object onto the logs" $ \e2e -> do
                -- A mirror round-trip drives request spans plus a worker job span, the
                -- span-scoped path whose log line carries a populated dd.trace_id.
                withNpmProject e2e $ \proj -> do
                    installed <- npmInstallIn proj (psName telemetryPkg)
                    shouldSucceed installed
                -- The exported signals carry the UST resource attributes the resolver
                -- derived from the DD_* identity (service.name/deployment.environment/
                -- service.version), both the key and the configured value.
                ust <-
                    awaitCollectorLog
                        e2e
                        ( \logs ->
                            all
                                (`T.isInfixOf` logs)
                                [ "service.name"
                                , ddTagService
                                , "deployment.environment"
                                , ddTagEnv
                                , "service.version"
                                , ddTagVersion
                                ]
                        )
                        80
                ust `shouldBe` True
                -- And the proxy's JSONL lines carry the dd object: the same UST identity
                -- plus a populated trace_id (the active-span log↔trace correlation).
                correlated <-
                    awaitProxyLog
                        e2e
                        ( \logs ->
                            hasPopulatedTraceId logs
                                && ("\"service\":\"" <> ddTagService <> "\"") `T.isInfixOf` logs
                                && ("\"env\":\"" <> ddTagEnv <> "\"") `T.isInfixOf` logs
                                && ("\"version\":\"" <> ddTagVersion <> "\"") `T.isInfixOf` logs
                        )
                        80
                correlated `shouldBe` True

{- | The first-party publish scenarios. The round-trip and the anti-shadowing refusal each
get their __own__ freshly booted environment with the publication target enabled
('publishTargetEnv', layered through 'E2EConfig' so only these scenarios see it) — Verdaccio
is both the publication target and the private upstream, the architected "publish, then read
back over the private leg" model. The opt-in @405@ posture runs on the base topology (no
publication target). Each is per-test isolated like 'scenarios'. See
@planning\/slices\/S52-publish-path.md@.
-}
publishScenarios :: SpecWith GlobalDataPlane
publishScenarios = do
    describe "first-party publish — publication target enabled" $
        aroundWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = publishTargetEnv}) $ do
            it "publishes an in-scope package, then installs it back through the private leg" $ \e2e -> do
                let name = publishInScopeName
                    ver = publishVersion
                -- An in-scope `npm publish` is admitted by the anti-shadowing guard and
                -- relayed to the publication target (Verdaccio), so the version is then
                -- present there...
                published <- withPublishProject e2e name ver npmPublishIn
                shouldSucceed published
                onTarget <- verdaccioHasVersion e2e name ver
                onTarget `shouldBe` True
                -- ...and readable back: the proxy serves it over the private (trusted) leg,
                -- so a fresh install through the proxy resolves and succeeds — publish →
                -- publication target → readable-back, end to end through the real image.
                installed <- npmInstall e2e name
                shouldSucceed installed

            it "refuses an out-of-scope publish before any upstream write (anti-shadowing guard)" $ \e2e -> do
                let name = publishOutOfScopeName
                    ver = publishVersion
                -- Precondition: the freshly booted, sealed Verdaccio has never seen it, so the
                -- post-publish absence below is attributable to the refusal, not a stale state.
                absentBefore <- verdaccioHasVersionNow e2e name ver
                absentBefore `shouldBe` False
                -- A name outside ECLUSE_PUBLISH_SCOPES is refused with a 403 BEFORE the relay, so npm
                -- exits non-zero and the publication target never receives it. The absence is a
                -- sound proof of refused-before-write *because* the harness configures Verdaccio
                -- to accept anonymous publishes: had the document reached the target it would
                -- have been stored and visible — exactly as the in-scope scenario, under the
                -- identical relay and ACL, shows it is (that scenario is the control). So a
                -- False after the patience window can only mean the write never left the proxy.
                published <- withPublishProject e2e name ver npmPublishIn
                shouldFail published
                reached <- verdaccioHasVersion e2e name ver
                reached `shouldBe` False

    describe "first-party publish — opt-in posture" $
        aroundWith withE2E $
            it "answers a publish with 405 when no publication target is configured" $ \e2e -> do
                -- The base topology sets no ECLUSE_PUBLICATION_TARGET, so the publish path is
                -- off: a PUT /{pkg} is not an allowed method (no implicit write path). A raw
                -- PUT is enough — the 405 precedes any body read — so npm need not be driven.
                status <- proxyPut e2e ("/npm/" <> publishInScopeName)
                status `shouldBe` 405

{- | Placeholders for not-yet-implemented work, kept outside 'around' so they boot no
environment. Graceful drain (#160) @SIGTERM@s the proxy, so when written it belongs in the
per-test isolated 'scenarios' above.
-}
pendingScenarios :: SpecWith GlobalDataPlane
pendingScenarios =
    describe "graceful shutdown" $
        it "drains in-flight work on SIGTERM" $ \_ ->
            pendingWith "activates with the #160 graceful-drain work"

-- | The mount-relative tarball path for a fixture package's single version.
tarballPath :: PkgSpec -> Text
tarballPath p = "/npm/" <> psName p <> "/-/" <> psName p <> "-" <> psVersion p <> ".tgz"

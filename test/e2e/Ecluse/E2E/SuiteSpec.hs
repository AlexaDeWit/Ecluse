-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The end-to-end scenarios, driven through the real @npm@ CLI against the real image.

__One data plane, shared per-describe proxies__. The whole suite shares a single data
plane: the docker network plus the Verdaccio, nginx, and ministack containers, booted once
by 'withGlobalDataPlane' under @aroundAll@. On top of it each @describe@ block boots its
own proxy under @aroundAllWith@ ('withE2E' \/ 'withE2EWith'). A telemetry scenario also
gets its own OTLP collector. A block's cases share that proxy rather than booting one per
case. Cases stay independent by acting on __distinct fixture packages__, not by
a fresh environment each. The scenarios use @allowPkg@, @denyPkg@, @tamperPkg@, @headPkg@,
@mirrorPkg@, and others, so no case observes another's mirror state or its bracketed
(paused-then-resumed) upstream. When the environment is unavailable (no docker or image),
every case reports @pending@ rather than failing.

Graceful drain is @pending@ here, and it sits outside the @aroundAll@ so it boots no
environment until the drain path exists.
-}
module Ecluse.E2E.SuiteSpec (spec) where

import Data.Text qualified as T

import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

import Ecluse.E2E.Fixtures (PkgSpec, allowPkg, denyPkg, headPkg, mirrorPkg, psName, psVersion, tamperPkg, telemetryDdPkg, telemetryPkg)
import Ecluse.E2E.Harness

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

{- | The active scenarios, grouped into @describe@ blocks that each share one proxy under
@aroundAllWith@. Cases stay independent through distinct fixture packages, not a fresh
environment each (see the module header).
-}
scenarios :: SpecWith GlobalDataPlane
scenarios = do
    describe "read-only and non-interfering scenarios (shared environment)" $ aroundAllWith withE2E $ do
        describe "public surface -- install and policy" $ do
            it "installs an allow-listed package end to end" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed

            it "blocks a package that declares an install script, and never mirrors it" $ \e2e -> do
                void $ npmInstall e2e (psName denyPkg) >>= shouldFail
                -- Give the worker a 1.5s window to erroneously mirror it, then assert absence.
                -- Using verdaccioHasVersion here would incur a 20-second timeout penalty.
                threadDelay 1500000
                mirrored <- verdaccioHasVersionNow e2e (psName denyPkg) (psVersion denyPkg)
                mirrored `shouldBe` False

            it "runs no package lifecycle script during a harness install (defence in depth)" $ \e2e -> do
                -- Distinct from the rules-engine block above. That block proves the *proxy*
                -- refuses to serve a package declaring an install script. This one proves our
                -- \*own* npm CLI executes no lifecycle script even when one is present. That
                -- guard closes the arbitrary-code-execution surface in Écluse's own CI. The probe
                -- project's own `postinstall` would create a sentinel, so a successful install
                -- that creates none proves npm_config_ignore_scripts held.
                (installed, scriptRan) <- installWithLifecycleProbe e2e
                void $ shouldSucceed installed
                scriptRan `shouldBe` False

        describe "server↔worker -- the integrity gate" $
            it "refuses to mirror an artifact whose bytes fail the integrity gate" $ \e2e -> do
                -- A tarball request enqueues a mirror on demand. The proxy serves the bytes,
                -- but the worker's strongest-digest gate must reject them, so the tampered
                -- version never reaches the private mirror.
                _ <- proxyGet e2e (tarballPath tamperPkg)
                threadDelay 1500000
                mirrored <- verdaccioHasVersionNow e2e (psName tamperPkg) (psVersion tamperPkg)
                mirrored `shouldBe` False

        describe "protocol behaviours" $
            it "answers HEAD on a tarball with its size but no body, and enqueues no mirror" $ \e2e -> do
                -- A HEAD goes through the same gating as the GET path, but probes the upstream
                -- as a HEAD and relays the headers with no body. It reports a Content-Length yet
                -- streams zero body bytes, and it enqueues no mirror because it serves no bytes
                -- to back-fill. This case drives a package only ever HEADed, never installed or
                -- GET, so the empty mirror is attributable to the HEAD alone.
                (status, declared, bodyBytes) <- proxyHead e2e (tarballPath headPkg)
                status `shouldBe` 200
                bodyBytes `shouldBe` 0
                declared `shouldSatisfy` maybe False (> 0)
                threadDelay 1500000
                mirrored <- verdaccioHasVersionNow e2e (psName headPkg) (psVersion headPkg)
                mirrored `shouldBe` False

        describe "server↔worker -- the full mirror lifecycle" $
            it "mirrors a package served from public, then installs it from the mirror with public down" $ \e2e -> do
                -- The core resilience loop, end to end, as a real upstream-outage scenario.
                -- The proxy serves a package absent from the private mirror but present on
                -- public: an `npm install` succeeds and writes a lockfile. The worker mirrors it
                -- to the private mirror. Then, with the public upstream PAUSED, an `npm ci` from
                -- that lockfile still installs it. `npm ci` fetches the artifact from the
                -- lockfile's `resolved` URL (the proxy's private-first tarball path) without
                -- re-resolving through the packument, so it never touches public. The success
                -- therefore proves the bytes came from the mirror, not from a reachable public.
                let name = psName mirrorPkg
                    ver = psVersion mirrorPkg
                presentBefore <- verdaccioHasVersionNow e2e name ver -- (1) a miss in the private mirror
                presentBefore `shouldBe` False
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj name >>= shouldSucceed -- (2,3) served from public, writes the lockfile
                    mirrored <- verdaccioHasVersion e2e name ver -- (4) the worker mirrors it to private
                    mirrored `shouldBe` True
                    void $ withUpstreamPaused e2e (npmCiIn proj) >>= shouldSucceed -- (5) public down → from the mirror
        describe "first-party publish -- opt-in posture" $
            it "answers a publish with 405 when no publication target is configured" $ \e2e -> do
                -- The base topology sets no ECLUSE_PUBLICATION_TARGET, so the publish path is
                -- off. A PUT /{pkg} is not an allowed method, and there is no implicit write
                -- path. A raw PUT is enough, since the 405 precedes any body read, so this case
                -- need not drive npm.
                status <- proxyPut e2e ("/npm/" <> publishInScopeName)
                status `shouldBe` 405

{- | The whole-system telemetry scenarios. Each @describe@ block boots its __own__ proxy
under @aroundAllWith@ on the shared data plane. It carries the telemetry topology that
block needs: an OTLP collector and the proxy's telemetry dialect ('E2EConfig'). Its
collector and dialect
never reach a sibling block. The collector validation keys on the collector's @debug@
exporter output, with no Datadog SaaS. The stdout and log validation keys on the proxy
container's own JSONL stream.
-}
telemetryScenarios :: SpecWith GlobalDataPlane
telemetryScenarios = do
    -- Real healthy OTLP publication: with telemetry on and an OTLP endpoint, a real npm
    -- request's ecluse.* metrics and its span actually reach a collector.
    describe "telemetry -- OTLP healthy publication (#324) and domain-span emission (#307)" $
        aroundAllWith (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}) $ do
            it "exports ecluse.* metrics and a span to the collector on a real npm request" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                -- The serve path's catalogue metric and a request span both land in the
                -- collector's debug exporter. The assertion keys on the catalogue metric name
                -- and the exporter's per-span marker, so it proves both signals arrived.
                delivered <-
                    awaitCollectorLog
                        e2e
                        (\logs -> "ecluse.serve.decision" `T.isInfixOf` logs && "Span #" `T.isInfixOf` logs)
                        80
                delivered `shouldBe` True

            it "emits the rule-eval, mirror-enqueue, and mirror-job domain spans to the collector on a mirror round-trip" $ \e2e -> do
                -- A public-served install gates the version (rule-eval span) and enqueues a
                -- mirror (enqueue span). The worker then mirrors it (job span).
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj (psName telemetryPkg) >>= shouldSucceed
                -- The worker mirrors asynchronously, so the mirror-job span lands after the
                -- install returns. The published mirror is the cue that the job ran.
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

    -- OTLP absent and telemetry off: the real image still boots, serves a real install,
    -- and logs JSONL to stdout/stderr, with no collector anywhere.
    describe "telemetry -- OTLP off, no collector (#325)" $
        aroundAllWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = [("ECLUSE_OBSERVABILITY__TELEMETRY", "off")]}) $
            it "starts, serves a real install, and logs JSONL to stdout -- no collector needed" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                -- It still writes structured JSONL to its stdout/stderr, and docker captures
                -- both. This awaits any log object, keyed on the `message` field every JSONL
                -- line carries. The worker's async publish line reliably provides one.
                logged <- awaitProxyLog e2e (T.isInfixOf "\"message\":") 80
                logged `shouldBe` True

    -- OTLP on but the collector unreachable or absent. The proxy config is the SAME as the
    -- healthy publication case, except that nothing stands the collector up
    -- (ecCollector = False). Its network alias therefore does not resolve. The proxy must
    -- still boot, serve, and KEEP serving. The SDK's batch exporter fails asynchronously off
    -- the request path, so the absent collector can never take the proxy down or block a
    -- request. The runtime wraps the OTLP span and metric exporters, since hs-opentelemetry
    -- 1.0.0.0 would otherwise drop the failed export silently. The failure therefore reaches
    -- katip under a throttle. The first failure is the "telemetry export error" line this
    -- asserts on, on top of the keeps-serving proof.
    describe "telemetry -- OTLP on but the collector unreachable (#325)" $
        aroundAllWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = otlpCollectorEnv}) $
            it "surfaces a throttled export-failure warning yet keeps serving -- an absent collector degrades visibly, no crash" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                logged <- awaitProxyLog e2e (T.isInfixOf "\"message\":") 80
                logged `shouldBe` True
                -- The first install's spans (1s batch flush) and metrics (1s reader) export
                -- and fail against the unreachable endpoint. The wrapped exporters route that
                -- failure through katip, so the throttle's first-failure warning lands in the
                -- proxy's JSONL: the operator signal that telemetry stopped flowing.
                exportWarned <- awaitProxyLog e2e (T.isInfixOf "telemetry export error") 80
                exportWarned `shouldBe` True
                -- It KEEPS serving: still ready, and still serving a fresh install. The
                -- failed-and-surfaced export never took the proxy down or blocked a request.
                stillReady <- proxyStatus e2e "/readyz"
                stillReady `shouldBe` 200
                void $ npmInstall e2e (psName mirrorPkg) >>= shouldSucceed

    -- Datadog pattern: DD_SERVICE/DD_ENV/DD_VERSION and DD_AGENT_HOST flow through the
    -- self-aligning resolver. They become Datadog unified-service-tag resource attributes on
    -- the exported signals, and the dd object on the JSONL logs.
    describe "telemetry -- Datadog pattern (#323)" $
        aroundAllWith (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = datadogCollectorEnv}) $
            it "carries the Datadog unified-service tags to the collector and the dd object onto the logs" $ \e2e -> do
                -- A mirror round-trip drives request spans plus a worker job span, the
                -- span-scoped path whose log line carries a populated dd.trace_id.
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj (psName telemetryDdPkg) >>= shouldSucceed
                -- The exported signals carry the UST resource attributes the resolver derived
                -- from the DD_* identity: service.name, deployment.environment, and
                -- service.version. The assertion checks both the key and the configured value.
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
                -- The proxy's JSONL lines carry the dd object: the same UST identity plus a
                -- populated trace_id, the active-span log↔trace correlation.
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

{- | The first-party publish scenarios. The round-trip and the anti-shadowing refusal share
one proxy under @aroundAllWith@, with the publication target enabled. 'publishTargetEnv'
layers through 'E2EConfig', so only these scenarios see it. Verdaccio is both the
publication target and the private upstream, the architected "publish, then read back over
the private leg" model. The opt-in @405@ posture runs on the base topology, with no
publication target. The two cases act on distinct package names, so neither observes the
other's publish.
-}
publishScenarios :: SpecWith GlobalDataPlane
publishScenarios = do
    describe "first-party publish -- publication target enabled" $
        aroundAllWith (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = publishTargetEnv}) $ do
            it "publishes an in-scope package, then installs it back through the private leg" $ \e2e -> do
                let name = publishInScopeName
                    ver = publishVersion
                -- The anti-shadowing guard admits an in-scope `npm publish` and the relay
                -- forwards it to the publication target (Verdaccio), so the version is then
                -- present there...
                void $ withPublishProject e2e name ver npmPublishIn >>= shouldSucceed
                onTarget <- verdaccioHasVersion e2e name ver
                onTarget `shouldBe` True
                -- ...and readable back: the proxy serves it over the private (trusted) leg, so
                -- a fresh install through the proxy resolves and succeeds. That is publish →
                -- publication target → readable-back, end to end through the real image.
                void $ npmInstall e2e name >>= shouldSucceed

            it "refuses an out-of-scope publish before any upstream write (anti-shadowing guard)" $ \e2e -> do
                let name = publishOutOfScopeName
                    ver = publishVersion
                -- Precondition: no other case publishes this out-of-scope name, so the
                -- suite-shared, sealed Verdaccio never receives it. The post-publish absence
                -- below is therefore attributable to the refusal, not to a stale state.
                absentBefore <- verdaccioHasVersionNow e2e name ver
                absentBefore `shouldBe` False
                -- The proxy refuses a name outside ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW with a 403
                -- BEFORE the relay, so npm exits non-zero and the publication target never
                -- receives it. The absence is a sound proof of refused-before-write *because*
                -- the harness configures Verdaccio to accept anonymous publishes. Verdaccio
                -- would store any document that reached the target, and the version would show
                -- up. The in-scope scenario is the control, and it shows exactly that under the
                -- identical relay and ACL. So a False after the patience window can only mean
                -- the write never left the proxy.
                withPublishProject e2e name ver $ \proj -> do
                    void $ npmPublishIn proj >>= shouldFail
                    threadDelay 1500000
                    reached <- verdaccioHasVersionNow e2e name ver
                    reached `shouldBe` False

{- | Placeholders for unimplemented work, kept outside @aroundAll@ so they boot no
environment. Graceful drain @SIGTERM@s the proxy, a destructive act on shared state. When
written it needs its own single-case @describe@ block, with its own proxy, rather than a
seat in a shared-proxy block above.
-}
pendingScenarios :: SpecWith GlobalDataPlane
pendingScenarios =
    describe "graceful shutdown" $
        it "drains in-flight work on SIGTERM" $ \_ ->
            pendingWith "activates with the #160 graceful-drain work"

-- | The mount-relative tarball path for a fixture package's single version.
tarballPath :: PkgSpec -> Text
tarballPath p = "/npm/" <> psName p <> "/-/" <> psName p <> "-" <> psVersion p <> ".tgz"

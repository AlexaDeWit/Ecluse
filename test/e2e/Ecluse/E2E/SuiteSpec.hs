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

import Ecluse.E2E.Fixtures (PkgSpec, allowPkg, denyPkg, headPkg, mirrorPkg, psName, psVersion, tamperPkg)
import Ecluse.E2E.Harness

spec :: Spec
spec = do
    unavailable <- runIO e2eUnavailable
    case unavailable of
        Just reason -> it "end-to-end suite (environment unavailable)" (pendingWith reason)
        Nothing -> do
            around withE2E scenarios
            telemetryScenarios
            pendingScenarios

{- | The active scenarios. Under 'around' each @it@ gets its own freshly booted
environment, torn down before the next — per-test isolation (see the module header).
-}
scenarios :: SpecWith E2E
scenarios = do
    describe "public surface — install and policy" $ do
        it "installs an allow-listed package end to end" $ \e2e -> do
            res <- npmInstall e2e (psName allowPkg)
            npmExit res `shouldBe` ExitSuccess

        it "blocks a package that declares an install script, and never mirrors it" $ \e2e -> do
            res <- npmInstall e2e (psName denyPkg)
            npmExit res `shouldNotBe` ExitSuccess
            mirrored <- verdaccioHasVersion e2e (psName denyPkg) (psVersion denyPkg)
            mirrored `shouldBe` False

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
                npmExit installed `shouldBe` ExitSuccess
                mirrored <- verdaccioHasVersion e2e name ver -- (4) the worker mirrors it to private
                mirrored `shouldBe` True
                fromMirror <- withUpstreamPaused e2e (npmCiIn proj) -- (5) public down → from the mirror
                npmExit fromMirror `shouldBe` ExitSuccess

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
telemetryScenarios :: Spec
telemetryScenarios = do
    -- #324 — real healthy OTLP publication: with telemetry on and an OTLP endpoint, a
    -- real npm request's ecluse.* metrics and its span actually reach a collector.
    describe "telemetry — OTLP healthy publication (#324)" $
        around (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}) $
            it "exports ecluse.* metrics and a span to the collector on a real npm request" $ \e2e -> do
                res <- npmInstall e2e (psName allowPkg)
                npmExit res `shouldBe` ExitSuccess
                -- The serve path's catalogue metric and a request span both land in the
                -- collector's debug exporter: keyed on the catalogue metric name and the
                -- exporter's per-span marker, so both signals are proven received.
                delivered <-
                    awaitCollectorLog
                        e2e
                        (\logs -> "ecluse.serve.decision" `T.isInfixOf` logs && "Span #" `T.isInfixOf` logs)
                        80
                delivered `shouldBe` True

    -- #325(a) — OTLP absent / telemetry off: the real image still boots, serves a real
    -- install, and logs JSONL to stdout/stderr, with no collector anywhere.
    describe "telemetry — OTLP off, no collector (#325)" $
        around (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = [("PROXY_TELEMETRY", "off")]}) $
            it "starts, serves a real install, and logs JSONL to stdout — no collector needed" $ \e2e -> do
                res <- npmInstall e2e (psName allowPkg)
                npmExit res `shouldBe` ExitSuccess
                -- It still writes structured JSONL to its stdout/stderr (docker captures
                -- both): await any log object (keyed on the `msg` field every katip JSONL
                -- line carries) — the worker's async publish line reliably provides one.
                logged <- awaitProxyLog e2e (T.isInfixOf "\"msg\":") 80
                logged `shouldBe` True

    -- #325(b) — OTLP on but the collector unreachable/absent: the SAME proxy config as the
    -- healthy #324 case, only the collector is never stood up (ecCollector = False), so its
    -- network alias does not resolve. The proxy must still boot, serve, and log, and KEEP
    -- serving: the SDK's batch exporter fails asynchronously off the request path and the
    -- failure is swallowed by the library (it never reaches the request or the process), so
    -- the absent collector can never take the proxy down. (hs-opentelemetry 1.0.0.0 drops a
    -- failed export silently rather than routing it through the global error handler, so
    -- there is deliberately no export-warning line to assert on — see the PR discussion.)
    describe "telemetry — OTLP on but the collector unreachable (#325)" $
        around (withE2EWith E2EConfig{ecCollector = False, ecExtraEnv = otlpCollectorEnv}) $
            it "still starts, serves, and keeps serving — an absent collector degrades silently, no crash" $ \e2e -> do
                firstInstall <- npmInstall e2e (psName allowPkg)
                npmExit firstInstall `shouldBe` ExitSuccess
                logged <- awaitProxyLog e2e (T.isInfixOf "\"msg\":") 80
                logged `shouldBe` True
                -- After the first install's spans/metrics have been exported-and-failed (the
                -- worker's async publish, awaited above, is well past the 1s export window),
                -- the proxy is still ready and still serves a fresh install — proving the
                -- unreachable collector never took it down or blocked a request.
                stillReady <- proxyStatus e2e "/readyz"
                stillReady `shouldBe` 200
                secondInstall <- npmInstall e2e (psName mirrorPkg)
                npmExit secondInstall `shouldBe` ExitSuccess

    -- #323 — Datadog pattern: DD_SERVICE/DD_ENV/DD_VERSION (+ DD_AGENT_HOST) flow through
    -- the self-aligning resolver to Datadog unified-service-tag resource attributes on the
    -- exported signals and the dd object on the JSONL logs.
    describe "telemetry — Datadog pattern (#323)" $
        around (withE2EWith E2EConfig{ecCollector = True, ecExtraEnv = datadogCollectorEnv}) $
            it "carries the Datadog unified-service tags to the collector and the dd object onto the logs" $ \e2e -> do
                -- A mirror round-trip drives request spans plus a worker job span, the
                -- span-scoped path whose log line carries a populated dd.trace_id.
                withNpmProject e2e $ \proj -> do
                    installed <- npmInstallIn proj (psName mirrorPkg)
                    npmExit installed `shouldBe` ExitSuccess
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

{- | Placeholders for not-yet-implemented work, kept outside 'around' so they boot no
environment. Graceful drain (#160) @SIGTERM@s the proxy, so when written it belongs in the
per-test isolated 'scenarios' above.
-}
pendingScenarios :: Spec
pendingScenarios =
    describe "graceful shutdown" $
        it "drains in-flight work on SIGTERM" $
            pendingWith "activates with the #160 graceful-drain work"

-- | The mount-relative tarball path for a fixture package's single version.
tarballPath :: PkgSpec -> Text
tarballPath p = "/npm/" <> psName p <> "/-/" <> psName p <> "-" <> psVersion p <> ".tgz"

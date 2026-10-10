-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Telemetry export from the product image under each telemetry configuration: OTLP metrics and
spans reaching a collector, the JSONL log stream with no collector, a visible degradation when the
collector is unreachable, and the Datadog unified service tags. The configuration is independent of
the mount. The traffic is npm's, and the mirror-span and private-leg cases need a mount with a
mirror target.
-}
module Ecluse.E2E.TelemetryE2ESpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T

import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (PkgSpec, allowPkg, mirrorPkg, psName, psVersion, telemetryDdPkg, telemetryPkg)
import Ecluse.E2E.Harness

-- | Drive the product image under each telemetry configuration and read what it exported.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios = do
    -- Real healthy OTLP publication: with telemetry on and an OTLP endpoint, a real npm
    -- request's ecluse.* metrics and its span actually reach a collector.
    describe "telemetry -- OTLP healthy publication (#324) and domain-span emission (#307)" $
        aroundAllWith (withE2EWith defaultE2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}) $ do
            it "exports ecluse.* metrics and a span to the collector on a real npm request" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                -- The assertion keys on the catalogue metric name and the exporter's per-span
                -- marker, so it proves both signals reached the collector.
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
                -- Each span must carry this case's own coordinate: the install before it
                -- emits the same three span names for another package.
                emitted <-
                    awaitCollectorLog
                        e2e
                        ( \logs ->
                            all
                                (\name -> any (spanFor telemetryPkg name) (exportedSpans logs))
                                ["ecluse.rule.eval", "ecluse.mirror.enqueue", "ecluse.mirror.job"]
                        )
                        120
                emitted `shouldBe` True

            it "serves a mirrored artifact from the private leg, and the collector receives that upstream fetch" $ \e2e -> do
                let name = psName telemetryPkg
                    ver = psVersion telemetryPkg
                -- The mirror-span case above served this version from the public leg, and the
                -- worker mirrored it, so the private upstream now holds the artifact.
                verdaccioHasVersion e2e name ver `shouldReturn` True
                -- A fresh project has an empty npm cache, so this install requests the artifact.
                void $ npmInstall e2e name >>= shouldSucceed
                -- The serve signals of Écluse itself name no leg for an artifact. The upstream fetch
                -- span does: the private upstream answered the earlier serve 404 and this one 200.
                let answers = privateArtifactAnswers name ver
                answered <- answers <$> awaitCollectorSpans e2e (elem (IntValue 200) . answers) 80
                ordNub answered `shouldBe` [IntValue 404, IntValue 200]

    -- OTLP absent and telemetry off: the real image still boots, serves a real install,
    -- and logs JSONL to stdout/stderr, with no collector anywhere.
    describe "telemetry -- OTLP off, no collector (#325)" $
        aroundAllWith (withE2EWith defaultE2EConfig{ecExtraEnv = [("ECLUSE_OBSERVABILITY__TELEMETRY", "off")]}) $
            it "starts, serves a real install, and logs JSONL to stdout -- no collector needed" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                -- This awaits any log object, keyed on the message field every JSONL line
                -- carries. The worker's async publish line reliably provides one.
                logged <- awaitProxyLog e2e (T.isInfixOf "\"message\":") 80
                logged `shouldBe` True

    describe "telemetry -- OTLP on but the collector unreachable (#325)" $
        aroundAllWith (withE2EWith defaultE2EConfig{ecExtraEnv = otlpCollectorEnv}) $
            it "surfaces a throttled export-failure warning yet keeps serving -- an absent collector degrades visibly, no crash" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed
                logged <- awaitProxyLog e2e (T.isInfixOf "\"message\":") 80
                logged `shouldBe` True
                -- Spans (1s batch flush) and metrics (1s reader) fail against the unreachable
                -- endpoint, and the throttle's first-failure warning reaches the proxy's JSONL.
                exportWarned <- awaitProxyLog e2e (T.isInfixOf "telemetry export error") 80
                exportWarned `shouldBe` True
                -- It KEEPS serving: still ready, and still serving a fresh install. The
                -- failed-and-surfaced export never took the proxy down or blocked a request.
                stillReady <- proxyStatus e2e "/readyz"
                stillReady `shouldBe` 200
                void $ npmInstall e2e (psName mirrorPkg) >>= shouldSucceed

    -- DD_SERVICE, DD_ENV, DD_VERSION and DD_AGENT_HOST flow through the self-aligning resolver.
    -- They become unified-service-tag resource attributes and the dd object on the JSONL logs.
    describe "telemetry -- Datadog pattern (#323)" $
        aroundAllWith (withE2EWith defaultE2EConfig{ecCollector = True, ecExtraEnv = datadogCollectorEnv}) $
            it "carries the Datadog unified-service tags to the collector and the dd object onto the logs" $ \e2e -> do
                -- A mirror round-trip drives request spans plus a worker job span, the
                -- span-scoped path whose log line carries a populated dd.trace_id.
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj (psName telemetryDdPkg) >>= shouldSucceed
                -- The resolver derives service.name, deployment.environment and service.version
                -- from the DD_* identity. The assertion checks both the key and the value.
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

-- Whether a span has this name and carries the coordinate of the fixture's latest version.
spanFor :: PkgSpec -> Text -> ExportedSpan -> Bool
spanFor pkg name exported =
    esName exported == name
        && spanCarries [("ecluse.package", TextValue (psName pkg)), ("ecluse.version", TextValue (psVersion pkg))] exported

{- The statuses the private upstream answered one artifact's fetches with, oldest first. The
attribute names are the http-client instrumentation's, and @mirror@ is that upstream's host. -}
privateArtifactAnswers :: Text -> Text -> [ExportedSpan] -> [AttributeValue]
privateArtifactAnswers name version =
    mapMaybe (lookup "http.status_code" . esAttributes) . filter (spanCarries fetch)
  where
    fetch =
        [ ("http.method", TextValue "GET")
        , ("http.host", TextValue "mirror")
        , ("http.target", TextValue (npmArtifactPath name version))
        ]

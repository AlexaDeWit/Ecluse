-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Docker (
    e2eUnavailable,
    withGlobalDataPlane,
    withE2E,
    withE2EWith,

    -- * Telemetry topology
    collectorOtlpEndpoint,
    otlpCollectorEnv,
    datadogCollectorEnv,
    ddTagService,
    ddTagEnv,
    ddTagVersion,

    -- * Observability
    withUpstreamPaused,

    -- * Utilities
    uniqueSuffix,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (
    Manager,
    Request (method),
    Response,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Types (statusCode)
import Network.Socket (
    Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    getSocketName,
    socket,
    tupleToHostAddress,
 )
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, readProcessStdout)
import UnliftIO (bracket, bracket_, handleAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.E2E.Fixtures (buildFixtures, fixturePackages)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Container.Image (ImageRef (LocallyBuilt, PinnedExternal), mkPinnedImageRef, renderImageRef)
import Ecluse.Test.Containers (dockerLabelArgs)

{- | 'Nothing' when the suite can run; @Just reason@ when it must be skipped -- no
docker daemon, or @ECLUSE_E2E_IMAGE@ unset (the image is built and named by
@make test-e2e@ / the CI e2e job).
-}
e2eUnavailable :: IO (Maybe String)
e2eUnavailable = do
    useExisting <- lookupEnv "ECLUSE_E2E_USE_EXISTING"
    case useExisting of
        Just "1" -> pure Nothing
        _ ->
            lookupEnv imageVar >>= \case
                Nothing -> pure (Just (imageVar <> " is unset -- run via `make test-e2e`"))
                Just "" -> pure (Just (imageVar <> " is empty -- run via `make test-e2e`"))
                Just _ -> do
                    ok <- dockerDaemonReachable
                    pure (if ok then Nothing else Just "no reachable docker daemon")

imageVar :: String
imageVar = "ECLUSE_E2E_IMAGE"

dockerDaemonReachable :: IO Bool
dockerDaemonReachable =
    handleAny (\_ -> pure False) (exitOk <$> readProcess (proc "docker" ["info"]))

{- | Build the shared fixture tree (packument\/tarball HTML, the Verdaccio and nginx
configs, and a fresh test CA + server cert) in a unique per-run temp directory for the
duration of the action, then remove the whole tree on every exit path. The directory is
unique per run (rather than a fixed name) so two worktrees running the e2e at once never
share -- or delete out from under each other -- one another's fixtures.
-}
withFixtureDir :: (FilePath -> IO a) -> IO a
withFixtureDir = bracket acquire (handleAny (const pass) . removePathForcibly)
  where
    acquire = do
        tmpRoot <- getTemporaryDirectory
        sfx <- uniqueSuffix
        let workDir = tmpRoot </> ("ecluse-e2e-fixtures-" <> sfx)
            htmlDir = workDir </> "html"
        createDirectoryIfMissing True htmlDir
        buildFixtures htmlDir fixturePackages
        writeFileText (workDir </> "verdaccio.yaml") verdaccioConfig
        writeFileText (workDir </> "nginx.conf") nginxStubConfig
        generateCerts (workDir </> "certs")
        pure workDir

withGlobalDataPlane :: (GlobalDataPlane -> IO ()) -> IO ()
withGlobalDataPlane action = do
    useExisting <- lookupEnv "ECLUSE_E2E_USE_EXISTING"
    case useExisting of
        Just "1" -> do
            -- Local development escape hatch: use existing ports on localhost.
            action GlobalDataPlane{gdpNet = "", gdpStub = "upstream", gdpVerd = "verdaccio", gdpMini = "ministack", gdpVerdPort = 4873, gdpMiniPort = 4566, gdpWorkDir = ""}
        _ -> do
            sfx <- uniqueSuffix
            -- Every container and the network carries the reaping labels, so a run that is
            -- hard-killed past the brackets below can still be swept by `task test-clean`
            -- (see "Ecluse.Test.Containers"). The nested brackets tear each resource down in
            -- reverse order on every exit path: success, failure, or exception.
            labelArgs <- dockerLabelArgs "e2e"
            withFixtureDir $ \workDir -> do
                -- Resolve each pulled image's pinned reference up front, aborting the suite
                -- loudly (see 'pinnedExternal') if a literal is not digest-pinned. The
                -- 'ImageRef' these run specs then carry keeps a tag out of every 'dockerRun'.
                -- verdaccio/verdaccio:5, nginx:alpine, and ministack (tag 1.3-full), each
                -- pinned by its multi-arch index digest: a mutable tag can be re-pointed at a
                -- poisoned image, an immutable @sha256@ digest cannot.
                verdImage <- pinnedExternal "verdaccio/verdaccio@sha256:9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c"
                stubImage <- pinnedExternal "nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fefa"
                miniImage <- pinnedExternal "ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594"
                let net = "ecluse-e2e-global-net-" <> sfx
                    verd = "ecluse-e2e-global-verd-" <> sfx
                    stub = "ecluse-e2e-global-stub-" <> sfx
                    mini = "ecluse-e2e-global-mini-" <> sfx
                    verdRun =
                        (dockerRun verd net verdImage)
                            { drAliases = ["verdaccio"]
                            , drPorts = ["127.0.0.1:0:4873"]
                            , drMounts = [(workDir </> "verdaccio.yaml", "/verdaccio/conf/config.yaml:ro")]
                            }
                    -- One nginx terminates TLS for both registry stubs, so it answers to two
                    -- in-network aliases (`upstream` and `mirror`) -- the multi-alias the raw
                    -- docker CLI supports and testcontainers 0.5.3.0 does not.
                    stubRun =
                        (dockerRun stub net stubImage)
                            { drAliases = ["upstream", "mirror"]
                            , drMounts =
                                [ (workDir </> "html", "/usr/share/nginx/html:ro")
                                , (workDir </> "nginx.conf", "/etc/nginx/conf.d/default.conf:ro")
                                , (workDir </> "certs", "/certs:ro")
                                ]
                            }
                    miniRun =
                        (dockerRun mini net miniImage)
                            { drAliases = ["ministack"]
                            , drPorts = ["127.0.0.1:0:4566"]
                            }
                -- RFC 5737 TEST-NET-3: an external-looking range the egress guard never
                -- blocks (see "Ecluse.Core.Security.Host" and its spec), so the real image
                -- runs unmodified with no production escape hatch. See docs/testing.md.
                withDockerNetwork labelArgs net ["--subnet", "203.0.113.0/24"] $ \_ ->
                    withDockerContainer labelArgs verdRun $ \_ ->
                        withDockerContainer labelArgs stubRun $ \_ ->
                            withDockerContainer labelArgs miniRun $ \_ -> do
                                miniPort <- publishedPort mini "4566/tcp"
                                verdPort <- publishedPort verd "4873/tcp"
                                action GlobalDataPlane{gdpNet = net, gdpStub = stub, gdpVerd = verd, gdpMini = mini, gdpVerdPort = verdPort, gdpMiniPort = miniPort, gdpWorkDir = workDir}

{- | Bring up a proxy on the shared data plane, wait for its readiness, run the action,
then tear the proxy down on every exit path -- the plain topology ('defaultE2EConfig'),
no collector and no extra proxy environment. Assumes 'e2eUnavailable' returned 'Nothing'.
Used under @aroundAllWith@, so one proxy is shared across a describe block's cases (see
"Ecluse.E2E.SuiteSpec").
-}
withE2E :: (E2E -> IO ()) -> GlobalDataPlane -> IO ()
withE2E = withE2EWith defaultE2EConfig

{- | 'withE2E' parameterised by an 'E2EConfig': optionally stand up an OTLP collector the
proxy exports to (on the shared data-plane network, reached by its @otelcol@ alias), and
layer extra proxy environment over the base 'proxyEnv'. The collector -- when asked for --
is brought up __before__ the proxy and waited until ready, so it is already receiving when
the proxy makes its first export, and is torn down with it. The proxy and collector are
per-invocation (a describe block's own, under @aroundAllWith@); the network and the
Verdaccio\/nginx\/ministack data plane underneath are the suite-shared ones from
'withGlobalDataPlane'.
-}
withE2EWith :: E2EConfig -> (E2E -> IO ()) -> GlobalDataPlane -> IO ()
withE2EWith cfg action gdp = do
    useExisting <- lookupEnv "ECLUSE_E2E_USE_EXISTING"
    case useExisting of
        Just "1" -> do
            manager <- newManager defaultManagerSettings
            let base = "http://127.0.0.1:4873"
            action
                E2E
                    { e2eRegistry = base <> "/npm/"
                    , e2eBaseUrl = base
                    , e2eVerdaccio = "http://127.0.0.1:4874" -- Assuming local verdaccio is on 4874 in local dev
                    , e2eStubContainer = gdpStub gdp
                    , e2eProxyContainer = "ecluse-proxy" -- Placeholder for local dev
                    , e2eCollectorContainer = if ecCollector cfg then Just "otelcol" else Nothing
                    , e2eManager = manager
                    }
        _ -> do
            image <- maybe (fail (imageVar <> " unset")) pure =<< lookupEnv imageVar
            sfx <- uniqueSuffix
            labelArgs <- dockerLabelArgs "e2e"
            let net = gdpNet gdp
                stub = gdpStub gdp
                prox = "ecluse-e2e-proxy-" <> sfx
                coll = "ecluse-e2e-otelcol-" <> sfx
                certsDir = gdpWorkDir gdp </> "certs"
            -- The OTLP collector (only when the scenario asks for one) comes up and is waited
            -- ready BEFORE the proxy, so it is already accepting when the proxy first exports;
            -- it is torn down after the body. 'Nothing' when the scenario runs without one.
            withOptionalCollector cfg labelArgs net coll $ \collectorName -> do
                manager <- newManager defaultManagerSettings
                -- Create the mirror queue in ministack and learn its URL. The proxy routes to
                -- ministack via AWS_ENDPOINT_URL_SQS and matches the queue by its path, so the
                -- URL's host (here ministack's own `localhost:4566`) is immaterial.
                let queueName = "ecluse-e2e-queue-" <> T.pack sfx
                queueUrl <- createMinistackQueue manager (gdpMiniPort gdp) queueName
                -- Pick the host port up front so ECLUSE_PUBLIC_URL (which makes the proxy
                -- rewrite dist.tarball to an absolute, npm-fetchable URL) is known before the
                -- container starts -- the assigned port is only readable after.
                proxyPort <- freeHostPort
                -- The real proxy image: server ‖ worker over the real SQS backend, pointed at
                -- ministack through the production AWS_ENDPOINT_URL_SQS override. The test CA
                -- bundle it trusts (SSL_CERT_FILE in 'proxyEnv') is bind-mounted from the shared
                -- certs dir -- the documented "extend the image with your cert chain" workflow.
                -- The product image is built by this run (`make test-e2e` / the CI e2e
                -- job), never pulled, so it is 'LocallyBuilt' and carries no digest: the
                -- pin invariant applies only to images pulled from a registry.
                let proxRun =
                        (dockerRun prox net (LocallyBuilt (toText image)))
                            { drPorts = ["127.0.0.1:" <> show proxyPort <> ":4873"]
                            , drMounts = [(certsDir, "/certs:ro")]
                            , drEnv = proxyEnv proxyPort queueUrl <> ecExtraEnv cfg
                            }
                withDockerContainer labelArgs proxRun $ \_ -> do
                    let verdPort = gdpVerdPort gdp
                        base = "http://127.0.0.1:" <> show proxyPort
                        e2e =
                            E2E
                                { e2eRegistry = base <> "/npm/"
                                , e2eBaseUrl = base
                                , e2eVerdaccio = "http://127.0.0.1:" <> show verdPort
                                , e2eStubContainer = stub
                                , e2eProxyContainer = prox
                                , e2eCollectorContainer = collectorName
                                , e2eManager = manager
                                }
                    ready <- waitFor manager (base <> "/readyz") 200
                    unless ready (fail "proxy did not become ready on /readyz within the timeout")
                    action e2e

{- | The proxy's environment, given the host port it is published on and the mirror
queue URL created in ministack. The real SQS backend is pointed at ministack through
the production @AWS_ENDPOINT_URL_SQS@ override and signs with the standard
@AWS_ACCESS_KEY_ID@\/@AWS_SECRET_ACCESS_KEY@ (the emulator ignores them). Both upstream
legs and the mirror target point at the stub containers by their network aliases.
@ECLUSE_PUBLIC_URL@ is the host-loopback address npm reaches the proxy on, so each
served @dist.tarball@ is rewritten to an absolute URL npm can fetch.
-}
proxyEnv :: Int -> Text -> [(Text, Text)]
proxyEnv hostPort queueUrl =
    [ ("ECLUSE_PORT", "4873")
    , -- ECLUSE_PUBLIC_URL is the proxy's own client-facing URL (for dist.tarball
      -- rewriting), not a registry-egress target, so it stays http on host loopback.
      ("ECLUSE_PUBLIC_URL", "http://127.0.0.1:" <> show hostPort)
    , -- The registry endpoints are https-only by construction: the upstream and mirror
      -- stubs are served over TLS (an nginx terminator with the test cert), and the proxy
      -- image's trust store is extended with the test CA via SSL_CERT_FILE below, the
      -- documented internal-CA operator workflow.
      ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://mirror/")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://upstream/")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror/")
    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "e2e-publish-token")
    , ("SSL_CERT_FILE", "/certs/bundle.pem")
    , ("ECLUSE_QUEUE_BACKEND", "sqs")
    , ("ECLUSE_QUEUE_URL", queueUrl)
    , -- The production endpoint override (AWS-SDK-standard), aimed at the ministack
      -- alias; the dummy keys sign the request the emulator does not validate.
      ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("ECLUSE_LOG_FORMAT", "json")
    , -- Add DenyInstallTimeExecution so the deny scenario has a rule to fire.
      -- We must also explicitly disable 'min-age' from the opinionated default policy,
      -- otherwise it will block the e2e test's freshly-created test packages.
      ("ECLUSE_RULES", "{\"min-age\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":0},\"deny-install-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}")
    ]

{- | A detached test container's @docker run@ specification: everything that varies
between the harness's containers, so the creation sites share one builder ('dockerRun')
and one bracket ('withDockerContainer') rather than each re-spelling the whole invocation.
-}
data DockerRun = DockerRun
    { drName :: String
    -- ^ The @--name@; also how the log\/pause helpers address the container later.
    , drNetwork :: String
    -- ^ The network to join (@--network@).
    , drAliases :: [String]
    -- ^ In-network aliases (@--network-alias@, repeatable).
    , drPorts :: [String]
    -- ^ @-p@ publish specs, e.g. @"127.0.0.1:0:4873"@.
    , drMounts :: [(FilePath, String)]
    -- ^ @-v@ bind mounts as @(hostPath, "containerPath[:ro]")@.
    , drEnv :: [(Text, Text)]
    -- ^ @-e@ environment.
    , drImage :: String
    -- ^ The image reference, already rendered to the string @docker@ receives.
    , drCmd :: [String]
    -- ^ Arguments after the image (override the default CMD); usually empty.
    }

{- | Resolve a raw external-image reference to a pinned 'ImageRef', failing the suite
loudly (the harness's IO idiom, 'fail') if the literal is not digest-pinned. The
'ImageRef' type keeps a tag out of 'dockerRun'; this is where a bad literal is caught, at
harness startup rather than at the pull.
-}
pinnedExternal :: Text -> IO ImageRef
pinnedExternal raw = PinnedExternal <$> either (fail . toString) pure (mkPinnedImageRef raw)

{- | The base 'DockerRun' for a named container on a network: no ports, mounts, env, or
cmd. The image is an 'ImageRef', so a pulled image is digest-pinned by construction and
only the run's own 'LocallyBuilt' product image may be unpinned; it is rendered to the
plain string here, at the single boundary to @docker@.
-}
dockerRun :: String -> String -> ImageRef -> DockerRun
dockerRun name net image =
    DockerRun
        { drName = name
        , drNetwork = net
        , drAliases = []
        , drPorts = []
        , drMounts = []
        , drEnv = []
        , drImage = toString (renderImageRef image)
        , drCmd = []
        }

{- | Render and run a 'DockerRun' detached (@docker run --rm -d@), stamped with the reaping
labels; fails the test loudly on a non-zero exit.
-}
runDetached :: [String] -> DockerRun -> IO ()
runDetached labelArgs spec =
    dockerOk $
        ["run", "--rm", "-d", "--name", drName spec, "--network", drNetwork spec]
            <> concatMap (\a -> ["--network-alias", a]) (drAliases spec)
            <> concatMap (\p -> ["-p", p]) (drPorts spec)
            <> concatMap (\(h, c) -> ["-v", h <> ":" <> c]) (drMounts spec)
            <> concatMap (\(k, v) -> ["-e", toString (k <> "=" <> v)]) (drEnv spec)
            <> labelArgs
            <> (drImage spec : drCmd spec)

{- | Run a detached container for the duration of the action, force-removing it
(@docker rm -f@) on every exit path -- success, failure, or exception. Yields the
container's name (which the caller chose, and the log\/pause helpers reuse).
-}
withDockerContainer :: [String] -> DockerRun -> (String -> IO a) -> IO a
withDockerContainer labelArgs spec =
    bracket (runDetached labelArgs spec >> pure (drName spec)) removeContainer

{- | Create a labelled docker network for the duration of the action, removing it on every
exit path (after any containers on it, since the brackets nest). @createArgs@ carries extra
@network create@ flags (e.g. @--subnet@). Yields the network name.
-}
withDockerNetwork :: [String] -> String -> [String] -> (String -> IO a) -> IO a
withDockerNetwork labelArgs name createArgs =
    bracket (dockerOk (["network", "create"] <> createArgs <> labelArgs <> [name]) >> pure name) removeNetwork

{- | Bring up the OTLP collector for a scenario that asks for one (an OTLP\/HTTP receiver
into a @debug@ exporter at detailed verbosity, so each received span and metric is written
to its logs, reached by the proxy as @otelcol@), waited ready and torn down around the
action; a no-op yielding 'Nothing' otherwise. The inline config arrives through the @env:@
provider, so the distroless image needs no shell, file, or bind mount.
-}
withOptionalCollector :: E2EConfig -> [String] -> String -> String -> (Maybe String -> IO a) -> IO a
withOptionalCollector cfg labelArgs net coll body
    | not (ecCollector cfg) = body Nothing
    | otherwise = do
        image <- pinnedExternal collectorImage
        let collectorRun =
                (dockerRun coll net image)
                    { drAliases = [toString collectorAlias]
                    , drEnv = [("OTELCOL_CONFIG", collectorConfig)]
                    , drCmd = ["--config", "env:OTELCOL_CONFIG"]
                    }
        withDockerContainer labelArgs collectorRun $ \_ -> do
            ready <- awaitContainerLog coll (T.isInfixOf "Everything is ready") 240
            unless ready (fail "OTLP collector did not become ready within the timeout")
            body (Just coll)

-- Force-remove a container by name; never throws (a missing container is fine), so a
-- bracket release cannot mask the action's own result or exception.
removeContainer :: String -> IO ()
removeContainer c = void (readProcess (proc "docker" ["rm", "-f", c]))

-- Remove a network by name; never throws, for the same reason as 'removeContainer'.
removeNetwork :: String -> IO ()
removeNetwork net = void (readProcess (proc "docker" ["network", "rm", net]))

-- The collector's network alias on the TEST-NET; the proxy exports to it by this name.
collectorAlias :: Text
collectorAlias = "otelcol"

{- | The in-cluster OTLP endpoint the proxy exports to -- the collector reached by its
network alias on the TEST-NET. A scenario names it through 'otlpCollectorEnv' (vanilla
OpenTelemetry) or has the resolver derive it from @DD_AGENT_HOST@ ('datadogCollectorEnv').
-}
collectorOtlpEndpoint :: Text
collectorOtlpEndpoint = "http://" <> collectorAlias <> ":4318"

{- Fast-flush export knobs -- standard @OTEL_*@ configuration (read by the SDK), not a
test-only path -- so a span and a metric reach the collector well within a scenario's
patience window rather than on the SDK's minute-scale batch defaults. -}
telemetryExportTuning :: [(Text, Text)]
telemetryExportTuning =
    [ ("OTEL_TRACES_EXPORTER", "otlp")
    , ("OTEL_METRICS_EXPORTER", "otlp")
    , ("OTEL_METRIC_EXPORT_INTERVAL", "1000")
    , ("OTEL_BSP_SCHEDULE_DELAY", "1000")
    ]

{- | Proxy environment for the vanilla-OpenTelemetry dialect: telemetry __on__, the OTLP
endpoint named by @OTEL_EXPORTER_OTLP_ENDPOINT@. Paired with @ecCollector = True@ the
collector is up and receives (the healthy-publication path); paired with
@ecCollector = False@ the named endpoint resolves to nothing, exercising the
missing-collector graceful-degradation path -- the same proxy configuration, only the
collector's presence differs.
-}
otlpCollectorEnv :: [(Text, Text)]
otlpCollectorEnv =
    [ ("ECLUSE_TELEMETRY", "on")
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", collectorOtlpEndpoint)
    ]
        <> telemetryExportTuning

{- | The Datadog unified-service-tag identity the Datadog scenario configures __and__
asserts on -- exported as resource attributes and stamped onto the @dd@ log object. Named
constants so the proxy environment and the assertions cannot drift apart.
-}
ddTagService, ddTagEnv, ddTagVersion :: Text
ddTagService = "ecluse-e2e-dd"
ddTagEnv = "e2e-staging"
ddTagVersion = "9.9.9-e2e"

{- | Proxy environment for the Datadog dialect: @DD_SERVICE@\/@DD_ENV@\/@DD_VERSION@ (the
unified-service tags) plus @DD_AGENT_HOST@ pointing the self-aligning resolver at the
collector. The resolver projects these onto @service.name@\/@deployment.environment@\/
@service.version@ resource attributes and the @dd@ log object. Pair with @ecCollector = True@.
-}
datadogCollectorEnv :: [(Text, Text)]
datadogCollectorEnv =
    [ ("ECLUSE_TELEMETRY", "on")
    , ("DD_SERVICE", ddTagService)
    , ("DD_ENV", ddTagEnv)
    , ("DD_VERSION", ddTagVersion)
    , ("DD_AGENT_HOST", collectorAlias)
    ]
        <> telemetryExportTuning

-- The OTLP Collector image, version 0.119.0 (matching the integration tier), pinned by
-- its multi-arch manifest-list digest like the ministack pin above: the scenarios assert
-- on this image's exact `debug`-exporter output and its readiness line, so its surface
-- must be immutable, not a movable tag. Resolved to a 'PinnedImageRef' (via
-- 'pinnedExternal') at collector startup, so an unpinned literal aborts the suite there.
-- The core distribution carries the OTLP receiver and the `debug` exporter the assertions
-- read.
collectorImage :: Text
collectorImage = "otel/opentelemetry-collector@sha256:3805724e26351df55a45032a793c9b64a2117ac9a58f13f070674a9723fab373"

{- The whole collector configuration as a single-line (flow-style) YAML document, passed
through the @env:@ config provider so no shell, file, or bind mount is needed on the
distroless image: an OTLP/HTTP receiver feeding a `debug` exporter at detailed verbosity
through __both__ a traces and a metrics pipeline, so every received span and metric is
written to the container logs. -}
collectorConfig :: Text
collectorConfig =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {"
        <> "traces: {receivers: [otlp], exporters: [debug]}, "
        <> "metrics: {receivers: [otlp], exporters: [debug]}}}}"

-- | Run a docker command, failing the test loudly if it exits non-zero.
dockerOk :: [String] -> IO ()
dockerOk args = do
    (code, _, err) <- readProcess (proc "docker" args)
    unless (code == ExitSuccess) $
        fail ("docker command " <> show args <> " failed: " <> toString (decodeUtf8 (LBS.toStrict err) :: Text))

{- | Generate a test CA and a server certificate (SANs: @upstream@, @mirror@,
@localhost@, @127.0.0.1@) into @dir@, plus a @bundle.pem@ trust bundle of the system
CAs and the test CA for the proxy's @SSL_CERT_FILE@. This stands in for an operator
extending the image with their own cert chain, the documented internal-CA workflow that
makes the https-only egress reachable in the sealed test network.
-}
generateCerts :: FilePath -> IO ()
generateCerts dir = do
    createDirectoryIfMissing True dir
    let caCrt = dir </> "ca.crt"
        caKey = dir </> "ca.key"
        srvCrt = dir </> "server.crt"
        srvKey = dir </> "server.key"
        srvCsr = dir </> "server.csr"
        ext = dir </> "san.ext"
    writeFileText ext "subjectAltName=DNS:upstream,DNS:mirror,DNS:localhost,IP:127.0.0.1\n"
    opensslOk ["req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", caKey, "-out", caCrt, "-days", "2", "-subj", "/CN=Ecluse E2E Test CA"]
    opensslOk ["genrsa", "-out", srvKey, "2048"]
    opensslOk ["req", "-new", "-key", srvKey, "-out", srvCsr, "-subj", "/CN=ecluse-e2e"]
    opensslOk ["x509", "-req", "-in", srvCsr, "-CA", caCrt, "-CAkey", caKey, "-CAcreateserial", "-out", srvCrt, "-days", "2", "-extfile", ext]
    -- The proxy's trust bundle: the system CAs (so an unmodified deployment still trusts
    -- public TLS) plus the test CA, exactly the operator's "system store + my CA" extension.
    systemCas <- lookupEnv "NIX_SSL_CERT_FILE" >>= maybe (pure "") readBytesOrEmpty
    testCa <- readFileBS caCrt
    writeFileBS (dir </> "bundle.pem") (systemCas <> "\n" <> testCa)

-- Read a file's bytes, or empty on any error (the system CA bundle is best-effort: the
-- proxy reaches only the test stubs over TLS in the e2e, so the test CA alone suffices).
readBytesOrEmpty :: FilePath -> IO ByteString
readBytesOrEmpty path = handleAny (\_ -> pure "") (readFileBS path)

-- | Run an openssl command, failing the test loudly if it exits non-zero.
opensslOk :: [String] -> IO ()
opensslOk args = do
    (code, _, err) <- readProcess (proc "openssl" args)
    unless (code == ExitSuccess) $
        fail ("openssl command " <> show args <> " failed: " <> toString (decodeUtf8 (LBS.toStrict err) :: Text))

-- | The host loopback port a container's given @\<port\>\/tcp@ is published to.
publishedPort :: String -> String -> IO Int
publishedPort cname containerPort = do
    (_, out) <- readProcessStdout (proc "docker" ["port", cname, containerPort])
    let firstLine = fromMaybe "" (listToMaybe (lines (decodeUtf8 (LBS.toStrict out))))
        portText = T.takeWhileEnd (/= ':') (T.strip firstLine)
    maybe
        (fail ("could not parse published port from " <> show firstLine))
        pure
        (readMaybe (toString portText))

{- | Create (idempotently) the mirror queue in the ministack SQS emulator on its
host-published port and return the queue URL. Uses the plain SQS query API -- the
emulator needs no signed request -- and retries while the emulator's SQS service warms
up. @CreateQueue@ is idempotent (a repeat returns the existing URL), so the retry is
safe. The returned URL's host is the emulator's own (@localhost:4566@); the proxy
routes to ministack via @AWS_ENDPOINT_URL_SQS@ and matches the queue by its path, so
that host is never dialled.
-}
createMinistackQueue :: Manager -> Int -> Text -> IO Text
createMinistackQueue manager hostPort queueName = go (60 :: Int)
  where
    endpoint =
        "http://127.0.0.1:"
            <> show hostPort
            <> "/?Action=CreateQueue&QueueName="
            <> queueName
            <> "&Version=2012-11-05"
    go :: Int -> IO Text
    go 0 = fail "ministack SQS CreateQueue never succeeded within the timeout"
    go n =
        handleAny (\_ -> retry n) $ do
            base <- parseRequest (toString endpoint)
            resp <- httpLbs base{method = "POST"} manager
            let body = decodeUtf8 (LBS.toStrict (responseBody resp)) :: Text
            case (statusCode (responseStatus resp), between "<QueueUrl>" "</QueueUrl>" body) of
                (200, Just url) | not (T.null url) -> pure url
                _ -> retry n
    retry :: Int -> IO Text
    retry n = threadDelay 500000 >> go (n - 1)

-- | The text between the first @opening@ and the following @closing@ marker, or 'Nothing'.
between :: Text -> Text -> Text -> Maybe Text
between opening closing t =
    let afterOpen = snd (T.breakOn opening t)
     in if T.null afterOpen
            then Nothing
            else
                let (inner, rest) = T.breakOn closing (T.drop (T.length opening) afterOpen)
                 in if T.null rest then Nothing else Just inner

-- | Poll a URL until it returns the wanted status, up to ~30s.
waitFor :: Manager -> Text -> Int -> IO Bool
waitFor manager url want = go (100 :: Int)
  where
    go 0 = pure False
    go n = do
        got <-
            handleAny (\_ -> pure Nothing) $ do
                req <- parseRequest (toString url)
                Just . statusCode . responseStatus <$> (httpLbs req manager :: IO (Response LByteString))
        if got == Just want then pure True else threadDelay 300000 >> go (n - 1)

exitOk :: (ExitCode, a, b) -> Bool
exitOk (code, _, _) = code == ExitSuccess

{- | A free host loopback port: bind to @127.0.0.1:0@, read the port the OS assigned,
release it. The brief window before docker rebinds it is a tolerable race for a
loopback test. Picked up front so ECLUSE_PUBLIC_URL can name it before boot.
-}
freeHostPort :: IO Int
freeHostPort =
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
        getSocketName sock >>= \case
            SockAddrInet port _ -> pure (fromIntegral port)
            other -> fail ("unexpected socket address: " <> show other)

-- | A unique, monotonic-ish suffix for network/container/temp names.
uniqueSuffix :: IO String
uniqueSuffix = do
    t <- getPOSIXTime
    pure (show (round (t * 1000) :: Integer))

{- | The nginx stub config, served over __TLS__ with the generated test cert. One nginx
terminates TLS for both registry stubs, routed by SNI\/@server_name@: the @upstream@
public stub serves static packuments\/tarballs from the file root (a package's packument
at @\/\<pkg\>@ from @\<pkg\>\/packument.json@, its tarball from @\/\<pkg\>\/-\/\<file\>.tgz@
by the default root, so @\<pkg\>@ is both the packument path and the tarball prefix without
a file\/directory clash), while the @mirror@ stub reverse-proxies to the Verdaccio container
over plain HTTP on the internal network. This is what makes the proxy dial https-only
registry endpoints; only the proxy validates the cert, so the harness's own probes stay
plain HTTP. @client_max_body_size 0@ lets a published tarball through the mirror leg, and
the forwarded @X-Forwarded-Proto https@ keeps Verdaccio generating https URLs.
-}
nginxStubConfig :: Text
nginxStubConfig =
    T.unlines
        [ "server {"
        , "    listen 443 ssl;"
        , "    server_name upstream;"
        , "    ssl_certificate /certs/server.crt;"
        , "    ssl_certificate_key /certs/server.key;"
        , "    root /usr/share/nginx/html;"
        , "    location ~ ^/(?<pkg>[^/]+)$ {"
        , "        default_type application/json;"
        , "        alias /usr/share/nginx/html/$pkg/packument.json;"
        , "    }"
        , "    location / {"
        , "        try_files $uri =404;"
        , "    }"
        , "}"
        , "server {"
        , "    listen 443 ssl;"
        , "    server_name mirror;"
        , "    ssl_certificate /certs/server.crt;"
        , "    ssl_certificate_key /certs/server.key;"
        , "    client_max_body_size 0;"
        , "    location / {"
        , "        proxy_pass http://verdaccio:4873;"
        , "        proxy_set_header Host $host;"
        , "        proxy_set_header X-Forwarded-Proto https;"
        , "        proxy_set_header X-Forwarded-For $remote_addr;"
        , "    }"
        , "}"
        ]

{- | The Verdaccio config: anonymous read + publish, no uplinks (a sealed local
mirror), listening on all interfaces so a peer container can reach it.
-}
verdaccioConfig :: Text
verdaccioConfig =
    T.unlines
        [ "listen: 0.0.0.0:4873"
        , "storage: /verdaccio/storage/data"
        , "auth:"
        , "  htpasswd:"
        , "    file: /verdaccio/storage/htpasswd"
        , "    max_users: -1"
        , "uplinks: {}"
        , "packages:"
        , "  '@*/*':"
        , "    access: $all"
        , "    publish: $all"
        , "    unpublish: $all"
        , "  '**':"
        , "    access: $all"
        , "    publish: $all"
        , "    unpublish: $all"
        , "log: { type: stdout, format: pretty, level: warn }"
        ]

{- | Pause the public-upstream stub for the duration of an action, then resume it
(@docker pause@ / @docker unpause@). Used to prove an install is served from the private
mirror while the public registry is unreachable: with the stub frozen, the only source
that can answer is the mirror. Resumed on every exit path so later cases see it again.
-}
withUpstreamPaused :: E2E -> IO a -> IO a
withUpstreamPaused e2e =
    bracket_
        (dockerOk ["pause", e2eStubContainer e2e])
        (dockerOk ["unpause", e2eStubContainer e2e])

-- Poll a container's logs until the predicate holds, up to @attempts@ times at ~250ms.
awaitContainerLog :: String -> (Text -> Bool) -> Int -> IO Bool
awaitContainerLog cname matches = go
  where
    go n
        | n <= 0 = pure False
        | otherwise = do
            logs <- containerLogs cname
            if matches logs then pure True else threadDelay 250000 >> go (n - 1)

-- A container's combined stdout+stderr so far ('docker logs'); empty on any docker
-- error (e.g. the container does not exist yet, mid image-pull).
containerLogs :: String -> IO Text
containerLogs cname =
    handleAny (\_ -> pure "") $ do
        (_, out, err) <- readProcess (proc "docker" ["logs", cname])
        pure (decodeUtf8 (LBS.toStrict out) <> decodeUtf8 (LBS.toStrict err))

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Container topology and role lifecycle for the end-to-end harness.
module Ecluse.E2E.Harness.Docker (
    e2eUnavailable,
    withGlobalDataPlane,
    withDataPlane,
    withDredgerPrivateCache,
    withE2E,
    withE2EWith,

    -- * Telemetry topology
    mirrorHost,
    collectorOtlpEndpoint,
    otlpCollectorEnv,
    datadogCollectorEnv,
    ddTagService,
    ddTagEnv,
    ddTagVersion,

    -- * Fixture controls
    withPublicUpstreamsDown,
    withStubFault,
    withPrivateCacheDeletesRefused,
    withPublicArtifactWithheld,
    stubReadsNow,
    awaitStubReads,

    -- * The product image, run to completion
    RoleRun (..),
    runRoleOnce,
    runDredgerOnce,
    advisoryDataDir,
    ministackAwsEnv,

    -- * Roles that outlive one invocation
    sharedMirrorQueue,
    withMirrorRole,
    mirrorRoleEnv,

    -- * Container logs
    awaitContainerLog,
    containerLogs,

    -- * Utilities
    uniqueSuffix,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
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
import System.Directory (createDirectoryIfMissing, renamePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import System.Process.Typed (proc, readProcess)
import UnliftIO (bracket_, finally, handleAny, onException, throwIO)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.E2E.Fixtures.Advisories (buildAdvisoryFixtures)
import Ecluse.E2E.Fixtures.Cargo.Advisories (buildCargoAdvisoryReleases)
import Ecluse.E2E.Fixtures.Npm (artifactFile, buildFixtures, fixturePackages)
import Ecluse.E2E.Fixtures.PyPI (buildPyPIFixtures, pypiUpstreamUrl)
import Ecluse.E2E.Harness.Container
import Ecluse.E2E.Harness.Stub (
    StubFault (..),
    StubRead,
    StubRoute (CargoPublic, Mirror, NpmPublic, PrivateCache, RubyGemsPublic),
    retiredWorkers,
    startedWorkers,
    stubConfig,
    stubHosts,
    stubReads,
    stubUrl,
 )
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Collector (collectorConfig)
import Ecluse.Test.Container.Image (
    ImageRef (LocallyBuilt),
    collectorImage,
    ministackImage,
    nginxImage,
    verdaccioImage,
 )
import Ecluse.Test.Containers (dockerLabelArgs)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.Cargo.Fixtures (loadCargoGraph, writeCargoRegistry)
import Ecluse.Test.Registry.RubyGems.Fixtures (writeRubyRegistry)

-- | The private mirror host on the container network, also recorded by upstream fetch spans.
mirrorHost :: Text
mirrorHost = "mirror"

-- | Explain missing infrastructure before acquiring a required group's resources.
e2eUnavailable :: IO (Maybe String)
e2eUnavailable =
    usingExistingPlane >>= \case
        True -> pure Nothing
        False ->
            lookupEnv imageVar >>= \case
                Nothing -> pure (Just (imageVar <> " is unset -- run via `task test-e2e`"))
                Just "" -> pure (Just (imageVar <> " is empty -- run via `task test-e2e`"))
                Just _ -> do
                    ok <- dockerDaemonReachable
                    pure (if ok then Nothing else Just "no reachable docker daemon")

imageVar :: String
imageVar = "ECLTEST_E2E_IMAGE"

{- Whether the local development escape hatch is on: the topology is already up on fixed
localhost ports, so the harness neither builds nor starts anything. -}
usingExistingPlane :: IO Bool
usingExistingPlane = (== Just "1") <$> lookupEnv "ECLTEST_E2E_USE_EXISTING"

dockerDaemonReachable :: IO Bool
dockerDaemonReachable =
    handleAny (\_ -> pure False) (exitOk <$> readProcess (proc "docker" ["info"]))

-- | Build only the fixtures required by this group's mounted ecosystems.
withFixtureDir :: Topology -> (FilePath -> IO a) -> IO a
withFixtureDir topology action = withSystemTempDirectory "ecluse-e2e-fixtures" $ \workDir -> do
    let htmlDir = workDir </> "html"
        pypiDir = workDir </> "pypi"
        cargoDir = workDir </> "cargo"
        rubyDir = workDir </> "rubygems"
    createDirectoryIfMissing True htmlDir
    createDirectoryIfMissing True pypiDir
    createDirectoryIfMissing True cargoDir
    createDirectoryIfMissing True rubyDir
    when (Npm `elem` topologyMounts topology) (buildFixtures htmlDir fixturePackages)
    when (PyPI `elem` topologyMounts topology) (buildPyPIFixtures pypiDir)
    when (Cargo `elem` topologyMounts topology) $ do
        releases <- loadCargoGraph "test/fixtures/cargo/graph"
        writeCargoRegistry cargoDir (stubUrl CargoPublic) Nothing False releases
        buildCargoAdvisoryReleases cargoDir (stubUrl CargoPublic)
    when (RubyGems `elem` topologyMounts topology) (writeRubyRegistry rubyDir)
    when (topologyAws topology) (buildAdvisoryFixtures htmlDir)
    writeFileText (workDir </> "verdaccio.yaml") verdaccioConfig
    writeFileText (workDir </> "nginx.conf") (stubConfig Nothing)
    generateCerts (workDir </> "certs")
    action workDir

-- | npm's basic writing topology, with its in-process queue.
withGlobalDataPlane :: (GlobalDataPlane -> IO ()) -> IO ()
withGlobalDataPlane = withDataPlane npmTopology

-- | Wait for the owned registry stub before yielding a group, releasing resources on every exit path.
withDataPlane :: Topology -> (GlobalDataPlane -> IO ()) -> IO ()
withDataPlane topology action =
    usingExistingPlane >>= \case
        True ->
            action
                GlobalDataPlane
                    { gdpNet = ""
                    , gdpStub = "upstream"
                    , gdpStore = if topologyStore topology then Just (Service "verdaccio" 4874) else Nothing
                    , gdpMinistack = if topologyAws topology then Just (Service "ministack" 4566) else Nothing
                    , gdpWorkDir = ""
                    , gdpTopology = topology
                    }
        False -> do
            sfx <- uniqueSuffix
            labels <- dockerLabelArgs "e2e"
            withFixtureDir topology $ \workDir -> do
                stubImage <- pinnedExternal nginxImage
                let net = "ecluse-e2e-net-" <> sfx
                    stub = "ecluse-e2e-stub-" <> sfx
                    stubRun =
                        (dockerRun stub net stubImage)
                            { drAliases = map toString stubHosts
                            , drMounts =
                                [ (workDir </> "html", "/usr/share/nginx/html:ro")
                                , (workDir </> "pypi", "/usr/share/nginx/pypi:ro")
                                , (workDir </> "cargo", "/usr/share/nginx/cargo:ro")
                                , (workDir </> "rubygems", "/usr/share/nginx/rubygems:ro")
                                , (workDir </> "nginx.conf", "/etc/nginx/conf.d/default.conf:ro")
                                , (workDir </> "certs", "/certs:ro")
                                ]
                            }
                -- The production egress guard accepts this documentation range without a bypass.
                withDockerNetwork labels net ["--subnet", "203.0.113.0/24"] $ \_ ->
                    withStore topology labels net workDir $ \store ->
                        withDockerContainer labels stubRun $ \_ -> do
                            ready <- awaitContainerLog stub ((> 0) . startedWorkers) 40
                            -- Abort acquisition through the container bracket to retain diagnostics and cleanup.
                            unless ready (throwIO RegistryStubStartupTimedOut)
                            withAws topology labels net $ \aws ->
                                action
                                    GlobalDataPlane
                                        { gdpNet = net
                                        , gdpStub = stub
                                        , gdpStore = store
                                        , gdpMinistack = aws
                                        , gdpWorkDir = workDir
                                        , gdpTopology = topology
                                        }

data RegistryStubStartupTimedOut = RegistryStubStartupTimedOut
    deriving stock (Show)

instance Exception RegistryStubStartupTimedOut where
    displayException _ = "the registry stub did not start a worker within the timeout"

withStore :: Topology -> [String] -> String -> FilePath -> (Maybe Service -> IO a) -> IO a
withStore topology labels net workDir action
    | not (topologyStore topology) = action Nothing
    | otherwise = do
        image <- pinnedExternal verdaccioImage
        suffix <- uniqueSuffix
        let name = "ecluse-e2e-store-" <> suffix
            run =
                (dockerRun name net image)
                    { drAliases = ["verdaccio"]
                    , drPorts = ["127.0.0.1:0:4873"]
                    , drMounts = [(workDir </> "verdaccio.yaml", "/verdaccio/conf/config.yaml:ro")]
                    }
        withDockerContainer labels run $ \_ -> do
            port <- publishedPort name "4873/tcp"
            manager <- newManager defaultManagerSettings
            ready <- waitFor manager (serviceUrl (Service name port) <> "/-/ping") 200
            unless ready (fail "the package store did not become ready")
            action (Just (Service name port))

withAws :: Topology -> [String] -> String -> (Maybe Service -> IO a) -> IO a
withAws topology labels net action
    | not (topologyAws topology) = action Nothing
    | otherwise = do
        image <- pinnedExternal ministackImage
        suffix <- uniqueSuffix
        let name = "ecluse-e2e-aws-" <> suffix
            run =
                (dockerRun name net image)
                    { drAliases = ["ministack"]
                    , drPorts = ["127.0.0.1:0:4566"]
                    }
        withDockerContainer labels run $ \_ -> do
            port <- publishedPort name "4566/tcp"
            action (Just (Service name port))

-- | Run a separate Verdaccio for the Dredger preview, using the existing fixture configuration.
withDredgerPrivateCache :: GlobalDataPlane -> E2E -> (E2E -> IO ()) -> IO ()
withDredgerPrivateCache plane mirror action = do
    suffix <- uniqueSuffix
    labels <- dockerLabelArgs "e2e"
    image <- pinnedExternal verdaccioImage
    let name = "ecluse-e2e-private-cache-" <> suffix
        container =
            (dockerRun name (gdpNet plane) image)
                { drAliases = ["cache-verdaccio"]
                , drPorts = ["127.0.0.1:0:4873"]
                , drMounts = [(gdpWorkDir plane </> "verdaccio.yaml", "/verdaccio/conf/config.yaml:ro")]
                }
    withDockerContainer labels container $ \_ -> do
        port <- publishedPort name "4873/tcp"
        let url = "http://127.0.0.1:" <> show port
            cache = mirror{e2eClientEndpoints = Map.singleton Npm url, e2eStore = Just (Service name port)}
        ready <- waitFor (e2eManager mirror) (url <> "/-/ping") 200
        unless ready (fail "the private preview cache did not become ready")
        action cache

-- | Run a ready proxy on the shared data plane, removing it on every exit path.
withE2E :: (E2E -> IO a) -> GlobalDataPlane -> IO a
withE2E = withE2EWith defaultE2EConfig

-- | Run a configured proxy, starting its optional collector before the proxy.
withE2EWith :: E2EConfig -> (E2E -> IO a) -> GlobalDataPlane -> IO a
withE2EWith cfg action gdp =
    usingExistingPlane >>= \case
        True -> do
            manager <- newManager defaultManagerSettings
            environment gdp manager "http://127.0.0.1:4873" "ecluse-proxy" (if ecCollector cfg then Just "otelcol" else Nothing) >>= action
        False -> do
            image <- maybe (fail (imageVar <> " unset")) pure =<< lookupEnv imageVar
            sfx <- uniqueSuffix
            labelArgs <- dockerLabelArgs "e2e"
            let net = gdpNet gdp
                prox = "ecluse-e2e-proxy-" <> sfx
                coll = "ecluse-e2e-otelcol-" <> sfx
                certsDir = gdpWorkDir gdp </> "certs"
            withOptionalCollector cfg labelArgs net coll $ \collectorName -> do
                manager <- newManager defaultManagerSettings
                queueUrl <- case ecQueueUrl cfg of
                    Just url -> pure (Just url)
                    Nothing -> case gdpMinistack gdp of
                        Nothing -> pure Nothing
                        Just service
                            | Npm `elem` topologyMounts (gdpTopology gdp) ->
                                Just <$> createMinistackQueue manager (servicePort service) ("ecluse-e2e-queue-" <> T.pack sfx)
                        Just _ -> pure Nothing
                -- Pick the host port up front: ECLUSE_SERVER__PUBLIC_URL must be known before the
                -- container starts, and it makes the proxy rewrite dist.tarball to an absolute URL.
                proxyPort <- freeHostPort
                configuredEnv <- proxyEnv (gdpTopology gdp) proxyPort queueUrl
                -- The product image is built by `task test-e2e` or the CI e2e job, never pulled, so
                -- it is 'LocallyBuilt' and unpinned: the pin invariant covers only registry pulls.
                fixtureUser <- traverse (const ((\uid gid -> show uid <> ":" <> show gid) <$> getEffectiveUserID <*> getEffectiveGroupID)) (ecAdvisoryDirectory cfg)
                let proxRun =
                        (dockerRun prox net (LocallyBuilt (toText image)))
                            { drPorts = ["127.0.0.1:" <> show proxyPort <> ":4873"]
                            , drMounts = [(certsDir, "/certs:ro")] <> maybe [] (\directory -> [(directory, advisoryDataDir)]) (ecAdvisoryDirectory cfg)
                            , drTmpfs = [advisoryDataTmpfs | isNothing (ecAdvisoryDirectory cfg)]
                            , drUser = fixtureUser
                            , drEnv = configuredEnv <> ecExtraEnv cfg
                            , drCmd = ecArgs cfg
                            }
                withDockerContainer labelArgs proxRun $ \_ -> do
                    let base = "http://127.0.0.1:" <> show proxyPort
                    e2e <- environment gdp manager base prox collectorName
                    ready <- waitFor manager (base <> "/readyz") 200
                    unless ready (fail "proxy did not become ready on /readyz within the timeout")
                    action e2e{e2eAdvisoryDirectory = ecAdvisoryDirectory cfg}

environment :: GlobalDataPlane -> Manager -> Text -> String -> Maybe String -> IO E2E
environment plane manager base proxy collector = do
    endpoints <- forM (topologyMounts (gdpTopology plane)) $ \ecosystem -> do
        path <- mountPath ecosystem
        pure (ecosystem, base <> path)
    pure
        E2E
            { e2eClientEndpoints = Map.fromList endpoints
            , e2eBaseUrl = base
            , e2eStore = gdpStore plane
            , e2ePlane = plane
            , e2eAdvisoryDirectory = Nothing
            , e2eProxyContainer = proxy
            , e2eCollectorContainer = collector
            , e2eManager = manager
            }
  where
    mountPath Npm = pure "/npm/"
    mountPath PyPI = pure "/pypi/simple/"
    mountPath Cargo = pure "/cargo/"
    mountPath RubyGems = pure "/rubygems/"

proxyEnv :: Topology -> Int -> Maybe Text -> IO [(Text, Text)]
proxyEnv topology hostPort queueUrl = do
    mounts <- concat <$> traverse mountEnv (topologyMounts topology)
    pure $
        [ ("ECLUSE_SERVER__PORT", "4873")
        , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:" <> show hostPort)
        , ("SSL_CERT_FILE", "/certs/bundle.pem")
        , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
        , ("ECLUSE_RULES", "{\"min-age\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":0}}")
        ]
            <> mounts
            <> maybe [] (\url -> [("ECLUSE_QUEUE__URL", url), ("AWS_ENDPOINT_URL_SQS", ministackEndpoint)]) queueUrl
            <> [entry | topologyStore topology, entry <- mirrorTargetEnv]
            <> [entry | topologyAws topology, entry <- ministackAwsEnv]
  where
    mountEnv Npm =
        pure $
            [("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl NpmPublic)]
                <> [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl Mirror) | topologyStore topology]
    mountEnv Cargo = pure [("ECLUSE_MOUNTS__CARGO__ENABLED", "true"), ("ECLUSE_MOUNTS__CARGO__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl CargoPublic)]
    mountEnv PyPI = pure [("ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL", pypiUpstreamUrl)]
    mountEnv RubyGems = pure [("ECLUSE_MOUNTS__RUBYGEMS__ENABLED", "true"), ("ECLUSE_MOUNTS__RUBYGEMS__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPublic)]

mirrorTargetEnv :: [(Text, Text)]
mirrorTargetEnv =
    [ ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL", stubUrl Mirror)
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN", "e2e-publish-token")
    ]

-- | Configure a dedicated worker to write the store the proxy reads privately.
mirrorRoleEnv :: Text -> Text -> [(Text, Text)]
mirrorRoleEnv queueUrl rules =
    [ ("ECLUSE_SERVER__PORT", "4873")
    , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl Mirror)
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl NpmPublic)
    , ("ECLUSE_QUEUE__URL", queueUrl)
    , ("AWS_ENDPOINT_URL_SQS", ministackEndpoint)
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("ECLUSE_RULES", rules)
    , ("SSL_CERT_FILE", "/certs/bundle.pem")
    ]
        <> mirrorTargetEnv
        <> ministackAwsEnv

ministackEndpoint :: Text
ministackEndpoint = "http://ministack:4566"

-- | Supply emulator credentials and an in-network endpoint to each product role.
ministackAwsEnv :: [(Text, Text)]
ministackAwsEnv =
    [ -- The generic endpoint override (AWS-SDK-standard) the S3 advisory client reads. The
      -- dummy keys sign a request the emulator does not validate.
      ("AWS_ENDPOINT_URL", ministackEndpoint)
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

withOptionalCollector :: E2EConfig -> [String] -> String -> String -> (Maybe String -> IO a) -> IO a
withOptionalCollector cfg labelArgs net coll body
    | not (ecCollector cfg) = body Nothing
    | otherwise = do
        image <- pinnedExternal collectorImage
        let collectorRun =
                (dockerRun coll net image)
                    { drAliases = [toString collectorAlias]
                    , drEnv = [("OTELCOL_CONFIG", collectorConfig ["traces", "metrics"])]
                    , drCmd = ["--config", "env:OTELCOL_CONFIG"]
                    }
        withDockerContainer labelArgs collectorRun $ \_ -> do
            ready <- awaitContainerLog coll (T.isInfixOf "Everything is ready") 240
            unless ready (fail "OTLP collector did not become ready within the timeout")
            body (Just coll)

-- The collector's network alias on the TEST-NET. The proxy exports to it by this name.
collectorAlias :: Text
collectorAlias = "otelcol"

-- | The collector endpoint shared by the OpenTelemetry and Datadog scenarios.
collectorOtlpEndpoint :: Text
collectorOtlpEndpoint = "http://" <> collectorAlias <> ":4318"

{- Standard @OTEL_*@ configuration the SDK reads, not a test-only path. A span and a metric
reach the collector within a scenario's patience window, not on minute-scale defaults. -}
telemetryExportTuning :: [(Text, Text)]
telemetryExportTuning =
    [ ("OTEL_TRACES_EXPORTER", "otlp")
    , ("OTEL_METRICS_EXPORTER", "otlp")
    , ("OTEL_METRIC_EXPORT_INTERVAL", "1000")
    , ("OTEL_BSP_SCHEDULE_DELAY", "1000")
    ]

-- | Enable OTLP export. Without a collector, this configuration exercises exporter failure.
otlpCollectorEnv :: [(Text, Text)]
otlpCollectorEnv =
    [ ("ECLUSE_OBSERVABILITY__TELEMETRY", "on")
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", collectorOtlpEndpoint)
    ]
        <> telemetryExportTuning

-- | The shared Datadog service identity used by configuration and assertions.
ddTagService, ddTagEnv, ddTagVersion :: Text
ddTagService = "ecluse-e2e-dd"
ddTagEnv = "e2e-staging"
ddTagVersion = "9.9.9-e2e"

-- | Configure Datadog identity and export to the optional collector.
datadogCollectorEnv :: [(Text, Text)]
datadogCollectorEnv =
    [ ("ECLUSE_OBSERVABILITY__TELEMETRY", "on")
    , ("DD_SERVICE", ddTagService)
    , ("DD_ENV", ddTagEnv)
    , ("DD_VERSION", ddTagVersion)
    , ("DD_AGENT_HOST", collectorAlias)
    ]
        <> telemetryExportTuning

-- | The exit status and JSONL logs from a completed product-image invocation.
data RoleRun = RoleRun
    { roleExit :: ExitCode
    , roleOutput :: Text
    }

-- | The image's advisory directory, mounted writable for sync and compilation.
advisoryDataDir :: FilePath
advisoryDataDir = "/var/lib/ecluse/advisories"

-- The image's own filesystem is read-only to its non-root user, so the directory arrives as a
-- world-writable tmpfs rather than the volume a deployment mounts.
advisoryDataTmpfs :: String
advisoryDataTmpfs = advisoryDataDir <> ":mode=1777"

-- | Run a product role to completion on the shared network with the supplied environment.
runRoleOnce :: GlobalDataPlane -> [(Text, Text)] -> [String] -> IO RoleRun
runRoleOnce gdp env args = do
    run <- roleRun gdp env args
    labelArgs <- dockerLabelArgs "e2e"
    flip finally (removeContainerIfPresent (drName run)) $ do
        (code, out, err) <-
            readProcess (proc "docker" (runArgs [] labelArgs run))
                `onException` reportContainer (drName run)
        pure RoleRun{roleExit = code, roleOutput = decodeUtf8 (LBS.toStrict (out <> err))}

roleRun :: GlobalDataPlane -> [(Text, Text)] -> [String] -> IO DockerRun
roleRun gdp env args = do
    image <- maybe (fail (imageVar <> " unset")) pure =<< lookupEnv imageVar
    sfx <- uniqueSuffix
    let role = fromMaybe "role" (listToMaybe args)
        base = dockerRun ("ecluse-e2e-" <> role <> "-" <> sfx) (gdpNet gdp) (LocallyBuilt (toText image))
    pure
        base
            { drMounts = [(gdpWorkDir gdp </> "certs", "/certs:ro")]
            , drTmpfs = [advisoryDataTmpfs]
            , drEnv = env
            , drCmd = args
            }

-- | Create one emulator queue that several product roles can share.
sharedMirrorQueue :: GlobalDataPlane -> IO Text
sharedMirrorQueue gdp = do
    manager <- newManager defaultManagerSettings
    sfx <- uniqueSuffix
    port <- ministackPort gdp
    createMinistackQueue manager port ("ecluse-e2e-shared-queue-" <> T.pack sfx)

-- | Run a dedicated mirror worker, passing its container name and removing it on every exit.
withMirrorRole :: GlobalDataPlane -> [(Text, Text)] -> (String -> IO a) -> IO a
withMirrorRole gdp env action = do
    run <- roleRun gdp env ["mirror"]
    labelArgs <- dockerLabelArgs "e2e"
    withDockerContainer labelArgs run action

-- | Run Dredger once against the proxy's mirror target with the supplied environment overrides.
runDredgerOnce :: GlobalDataPlane -> [Text] -> [(Text, Text)] -> IO RoleRun
runDredgerOnce gdp flags extraEnv =
    runRoleOnce gdp (dredgerEnv <> extraEnv) ("dredger" : map toString flags)

-- A deleting role requires a private upstream distinct from its mirror target.
dredgerEnv :: [(Text, Text)]
dredgerEnv =
    [ ("ECLUSE_SERVER__PORT", "4873")
    , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl PrivateCache)
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__TOKEN", "e2e-private-maintenance-token")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__PERMIT_DELETION", "true")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl NpmPublic)
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION", "true")
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("SSL_CERT_FILE", "/certs/bundle.pem")
    ]
        <> mirrorTargetEnv
        <> ministackAwsEnv

-- Every stub alias needs a certificate SAN, and SSL_CERT_FILE needs the system and test CAs.
generateCerts :: FilePath -> IO ()
generateCerts dir = do
    createDirectoryIfMissing True dir
    let caCrt = dir </> "ca.crt"
        caKey = dir </> "ca.key"
        srvCrt = dir </> "server.crt"
        srvKey = dir </> "server.key"
        srvCsr = dir </> "server.csr"
        ext = dir </> "san.ext"
    writeFileText ext ("subjectAltName=" <> T.intercalate "," (map ("DNS:" <>) (stubHosts <> ["localhost"]) <> ["IP:127.0.0.1"]) <> "\n")
    commandOk "openssl" ["req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", caKey, "-out", caCrt, "-days", "2", "-subj", "/CN=Ecluse E2E Test CA"]
    commandOk "openssl" ["genrsa", "-out", srvKey, "2048"]
    commandOk "openssl" ["req", "-new", "-key", srvKey, "-out", srvCsr, "-subj", "/CN=ecluse-e2e"]
    commandOk "openssl" ["x509", "-req", "-in", srvCsr, "-CA", caCrt, "-CAkey", caKey, "-CAcreateserial", "-out", srvCrt, "-days", "2", "-extfile", ext]
    -- The system CAs plus the test CA, exactly the operator's "system store + my CA"
    -- extension. The system CAs keep an unmodified deployment trusting public TLS.
    systemCas <- lookupEnv "NIX_SSL_CERT_FILE" >>= maybe (pure "") readBytesOrEmpty
    testCa <- readFileBS caCrt
    writeFileBS (dir </> "bundle.pem") (systemCas <> "\n" <> testCa)

-- Read a file's bytes, or empty on any error. The system CA bundle is best-effort: the
-- proxy reaches only the test stubs over TLS in the e2e, so the test CA alone suffices.
readBytesOrEmpty :: FilePath -> IO ByteString
readBytesOrEmpty path = handleAny (\_ -> pure "") (readFileBS path)

-- CreateQueue is idempotent, so the emulator warm-up can retry it.
createMinistackQueue :: Manager -> Int -> Text -> IO Text
createMinistackQueue manager hostPort queueName =
    pollUntil 60 500000 isJust attempt
        >>= maybe (fail "ministack SQS CreateQueue never succeeded within the timeout") pure
  where
    endpoint =
        "http://127.0.0.1:"
            <> show hostPort
            <> "/?Action=CreateQueue&QueueName="
            <> queueName
            <> "&Version=2012-11-05"
    attempt :: IO (Maybe Text)
    attempt =
        handleAny (\_ -> pure Nothing) $ do
            base <- parseRequest (toString endpoint)
            resp <- httpLbs base{method = "POST"} manager
            let body = decodeUtf8 (LBS.toStrict (responseBody resp)) :: Text
            pure $ case (statusCode (responseStatus resp), between "<QueueUrl>" "</QueueUrl>" body) of
                (200, Just url) | not (T.null url) -> Just url
                _ -> Nothing

between :: Text -> Text -> Text -> Maybe Text
between opening closing t =
    let afterOpen = snd (T.breakOn opening t)
     in if T.null afterOpen
            then Nothing
            else
                let (inner, rest) = T.breakOn closing (T.drop (T.length opening) afterOpen)
                 in if T.null rest then Nothing else Just inner

-- Readiness polling uses 100 attempts with 300 ms pauses.
waitFor :: Manager -> Text -> Int -> IO Bool
waitFor manager url want = pollUntil 100 300000 id probe
  where
    probe :: IO Bool
    probe =
        handleAny (\_ -> pure False) $ do
            req <- parseRequest (toString url)
            (== want) . statusCode . responseStatus <$> (httpLbs req manager :: IO (Response LByteString))

exitOk :: (ExitCode, a, b) -> Bool
exitOk (code, _, _) = code == ExitSuccess

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

-- | Close both public routes without answering, while private routes remain available.
withPublicUpstreamsDown :: GlobalDataPlane -> IO a -> IO a
withPublicUpstreamsDown plane = withStubFault plane PublicUpstreamsDown

-- | Refuse private-cache writes while continuing to answer reads.
withPrivateCacheDeletesRefused :: GlobalDataPlane -> IO a -> IO a
withPrivateCacheDeletesRefused plane = withStubFault plane PrivateCacheWritesRefused

-- | Hold one route fault, restoring the default configuration when the action ends.
withStubFault :: GlobalDataPlane -> StubFault -> IO a -> IO a
withStubFault plane fault action =
    finally (reloadStub plane (Just fault) >> action) (reloadStub plane Nothing)

-- Rewrite the bind-mounted configuration and have nginx apply it. nginx retires its old workers in
-- the background, so this returns once its log shows every one of them has stopped accepting.
reloadStub :: GlobalDataPlane -> Maybe StubFault -> IO ()
reloadStub plane fault = do
    superseded <- startedWorkers <$> containerLogs stub
    when (superseded == 0) (fail "the stub's log names no worker process, so no reload can be confirmed")
    writeFileText (gdpWorkDir plane </> "nginx.conf") (stubConfig fault)
    commandOk "docker" ["exec", stub, "nginx", "-s", "reload"]
    applied <- awaitContainerLog stub ((>= superseded) . retiredWorkers) 40
    unless applied (fail "the stub did not retire its old workers after a reload")
  where
    stub = gdpStub plane

-- | Every request the stub has finished so far, oldest first.
stubReadsNow :: GlobalDataPlane -> IO [StubRead]
stubReadsNow plane = stubReads <$> containerLogs (gdpStub plane)

-- | Poll requests after @seen@ until @complete@ holds or the attempt budget expires.
awaitStubReads :: GlobalDataPlane -> Int -> ([StubRead] -> Bool) -> IO [StubRead]
awaitStubReads plane seen complete = pollUntil 40 250000 complete (drop seen <$> stubReadsNow plane)

-- | Withhold a public artifact without changing its metadata, restoring it on every exit.
withPublicArtifactWithheld :: GlobalDataPlane -> Text -> Text -> IO a -> IO a
withPublicArtifactWithheld plane name version =
    bracket_ (renamePath served withheld) (renamePath withheld served)
  where
    served = artifactFile (gdpWorkDir plane </> "html") name version
    withheld = served <> ".withheld"

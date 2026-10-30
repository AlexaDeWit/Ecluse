-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Maintenance construction and credentials use each store's cleared authority.
module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Aeson (encode)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Traversable (for)
import Network.HTTP.Client (Request (path, requestHeaders), defaultManagerSettings, newManager)
import Network.HTTP.Types.Header (Header, hAuthorization)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.BootError (Advisory (PrivateUpstreamUndecided), BootError (PrivateUpstreamProbeFailed, PrivateUpstreamUnsafe, StoreMaintenanceUnavailable), StoreMaintenanceReason (ClientBuildFailed))
import Ecluse.Composition.Credential (noCredentialProviders)
import Ecluse.Composition.Maintenance (BudgetPorts (BudgetPorts, bpGateFor, bpNominalPace, bpOverrides), StorePorts (..), buildStoreMaintenance, buildStoreObservation, buildUpstreamProbe, planStoreMaintenance, readUpstreamSafety, resolvedBudget, storeScope, upstreamFindings)
import Ecluse.Composition.Maintenance.Plan (ClearedBackend (cbFetchManifest, cbUrl), vetStoreBackends)
import Ecluse.Composition.Maintenance.Support (clearedBackendsFor, mountsFor, noAdapter, privateCaches, refusalsOf, unaddressablePrivateUpstream, verdaccioEnv, vetted, withoutPrivateAuthority)
import Ecluse.Composition.Support (NoCredentials (NoCredentials), cargoCodeArtifactEnv, clearedUrl, codeArtifactDomain, codeArtifactEnvVars, expectConfig, overrideEnv, rubyGemsMirrorEnv, staticEnvVars, twoStoreEnv, withObservablePrivate)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp, configMounts), MountConfig (mntPrivateUpstream), PrivateEndpoint, QuotaOverride (QuotaOverride, qoQuotas, qoScope, qoWeights))
import Ecluse.Core.BuildIdentity (parseOperatorContact, renderUserAgent, userAgent)
import Ecluse.Core.Credential (AuthToken (AuthToken), CredentialProvider (CredentialProvider), mkSecret)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Package (PackageInfo (infoName, infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Maintenance (CompletionNotion (CompletesOnCall), ConsentVerdict (ConsentGranted, ConsentWithheld), DeleteCeiling (AtMost), RefillPosture (RefillPermitted), RetryAdvice (RetryWorthwhile), StoreFacts (..), StoreFault (faultRetry), StoreMaintenance (deleteVersions, readStoreManifest, storeFacts, verifyConsent), StoreManifestRead, StoreObservation (obFacts, obReadManifest, obVerifyConsent), VersionOutcome (VersionRemoved))
import Ecluse.Core.Registry.Maintenance.Budget (QuotaDimension (StoreRequests), QuotaOrigin (QuotaDeclared, QuotaDerived), RequestGate (RequestGate, gateSpend), RequestKind (CursorWrite, DeleteBatch, ListingPage), StoreBudget (bgCosts, bgOrigin, bgQuotas, bgScope), mkQuotaScope, requestKinds, undeclaredBudget)
import Ecluse.Core.Registry.Maintenance.Upstream (ExternalConnection (ExternalConnection), RepositoryName (RepositoryName), UndecidabilityReason (NetworkFailure, NoMechanism), UnsafeReason (ConfigurationEvidence), UpstreamSafety (Safe, Undecidable, Unsafe))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataAbsent, MetadataFetch))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseRequest, ocToken), originClient)
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Text (joinUrlPath, registryPath)
import Ecluse.Test.Corpus.Cargo (cargoCaptureDiscoveryBytes)
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Env (withAmbientAws)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore, testDeleteGuard)
import Ecluse.Test.Package (npmVersion, unsafeRegistryUrl, unscopedNpm, unscopedPyPI)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestName, cargoTestRecord)
import Ecluse.Test.Server.Mount (requiredAdapter)
import Ecluse.Test.Stub (Captured (capHeaders, capMethod, capPath), allCaptured, headerValue, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)
import Network.HTTP.Types.Status (status200, status201, status404)

spec :: Spec
spec = do
    absentAdapterSpec
    protocolSpec
    buildSpec
    metadataCredentialSpec
    planSpec
    probeSpec
    previewCachesSpec
    budgetSpec

buildSpec :: Spec
buildSpec = describe "buildStoreMaintenance -- a store swept through the ecosystem protocol" $ do
    it "supplies the backend's standing facts under the tag the store was declared with" $ withSweeper $ \deadlines -> do
        handle <- protocolHandleFor deadlines MirrorPruner "true"
        let facts = storeFacts handle
        factBackend facts `shouldBe` "verdaccio"
        factDeleteCeiling facts `shouldBe` AtMost 1
        factRefill facts `shouldBe` RefillPermitted
        factCompletion facts `shouldBe` CompletesOnCall

    it "reads a manifest over the store's own endpoint, not the public upstream" $ withSweeper $ \deadlines -> do
        -- The endpoint answers nothing, so the read reaching the network at all is what this
        -- shows: the handle carries the ecosystem's codec rather than an absent read.
        handle <- protocolHandleFor deadlines MirrorPruner "true"
        (fmap faultRetry . leftToMaybe <$> readStoreManifest handle aPackage)
            `shouldReturn` Just RetryWorthwhile

    it "sends the configured contact on maintenance reads and deletion" $ do
        contact <- either (fail . toString) pure (parseOperatorContact "platform@example.com")
        let renderedAgent = renderUserAgent (Just contact)
            answer captured
                | capMethod captured == "GET" = (status200, [], "{\"_rev\":\"1-abc\",\"versions\":{\"1.0.0\":{}}}")
                | otherwise = (status201, [], "{}")
        withRoutedStub answer $ \stub -> do
            cleared <- clearedBackendsFor MirrorPruner (verdaccioEnv "true")
            case Map.elems cleared of
                [backend] -> withSweeper $ \deadlines -> do
                    handle <- buildStoreMaintenance (anonymousPorts deadlines){spUserAgent = renderedAgent} defaultLimits backend{cbUrl = loopbackRegistryUrl (stubBaseUrl stub)}
                    let version = npmVersion "1.0.0"
                    deleteVersions handle testDeleteGuard aPackage [version] `shouldReturn` [(version, VersionRemoved)]
                    captured <- allCaptured stub
                    map capMethod captured `shouldBe` ["GET", "DELETE"]
                    map (headerValue "User-Agent") captured `shouldBe` replicate 2 (Just renderedAgent)
                _ -> expectationFailure "expected one cleared protocol store"

    it "grants consent on the store the pass cleared" $ withSweeper $ \deadlines -> do
        handle <- protocolHandleFor deadlines MirrorPruner "true"
        verifyConsent handle `shouldReturn` Right ConsentGranted

    it "withholds it, naming the key an operator sets, on a store carrying none" $ withSweeper $ \deadlines -> do
        -- Only the preview role's pass clears a store carrying no consent, so its boot is the
        -- one that builds the handle reporting the withheld verdict.
        handle <- protocolHandleFor deadlines MirrorPreviewer "false"
        verifyConsent handle >>= \case
            Right (ConsentWithheld descriptor) ->
                descriptor `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"
            other -> expectationFailure ("expected a withheld verdict, got: " <> show other)

metadataCredentialSpec :: Spec
metadataCredentialSpec = describe "maintenance metadata credentials" $ do
    for_ metadataBuilders $ \(registryRole, role, build) ->
        for_ [Npm, PyPI] $ \eco ->
            it ("refreshes credentials on both " <> show eco <> " targets during " <> role) $ withAmbientAws $ do
                config <- expectConfig twoStoreEnv Nothing
                targets <- clearedMetadataTargets registryRole eco config
                for_ targets $ \cleared -> do
                    counter <- newIORef (0 :: Int)
                    let provider = CredentialProvider $ do
                            modifyIORef' counter (+ 1)
                            count <- readIORef counter
                            pure (AuthToken (mkSecret ("token-" <> show count)) Nothing)
                    requests <- captureMetadataRequests eco build cleared (Just provider)
                    map (lookup hAuthorization . requestHeaders) requests `shouldBe` expectedHeaders eco
                    map path requests `shouldBe` replicate 2 (expectedPath eco (clearedUrl cleared))
                    readIORef counter `shouldReturn` 2
    for_ metadataBuilders $ \(registryRole, role, build) ->
        it ("refreshes raw Cargo credentials through discovery and index reads during " <> role) $
            withAmbientAws $
                withRoutedStub (const (status404, [], "public fallback forbidden")) $ \public -> do
                    let publicUrl = T.replace "http://" "https://" (stubBaseUrl public)
                    config <- expectConfig (overrideEnv "ECLUSE_MOUNTS__CARGO__PUBLIC_UPSTREAM__REGISTRY__URL" (toString publicUrl) cargoCodeArtifactEnv) Nothing
                    targets <- clearedMetadataTargets registryRole Cargo config
                    for_ targets $ \cleared -> do
                        counter <- newIORef (0 :: Int)
                        let storePath = registryPath (clearedUrl cleared)
                            configPath = encodeUtf8 (joinUrlPath storePath "config.json")
                            indexPath = encodeUtf8 (joinUrlPath storePath "fi/xt/fixture")
                            answer captured
                                | capPath captured == configPath = (status200, [], toLazy cargoCaptureDiscoveryBytes)
                                | capPath captured == indexPath = (status200, [], encode (cargoTestRecord "1.0.0") <> "\n")
                                | otherwise = (status404, [], "unexpected store request")
                            provider = CredentialProvider $ do
                                modifyIORef' counter (+ 1)
                                count <- readIORef counter
                                pure (AuthToken (mkSecret ("token-" <> show count)) Nothing)
                        withRoutedStub answer $ \storeStub -> withSweeper $ \deadlines -> do
                            let endpoint = loopbackRegistryUrl (stubBaseUrl storeStub <> storePath)
                            readManifest <- build (anonymousPorts deadlines){spCredential = Just provider} defaultLimits cleared{cbUrl = endpoint}
                            replicateM_ 2 $ do
                                manifest <- expectRight =<< readManifest cargoTestName
                                infoName (manifestInfo manifest) `shouldBe` cargoTestName
                                Map.keys (infoVersions (manifestInfo manifest)) `shouldBe` ["1.0.0"]
                            requests <- allCaptured storeStub
                            map capMethod requests `shouldBe` replicate 4 "GET"
                            map capPath requests `shouldBe` [configPath, indexPath, configPath, indexPath]
                            map (headerValue "Authorization") requests `shouldBe` [Just "token-1", Just "token-1", Just "token-2", Just "token-2"]
                            readIORef counter `shouldReturn` 2
                    allCaptured public `shouldReturn` []
    for_ metadataBuilders $ \(registryRole, role, build) ->
        it ("sends each CodeArtifact store read with its exact headers during " <> role) $
            withAmbientAws $ do
                observed <- for storeReadCases $ \(eco, env, name) -> do
                    config <- expectConfig env Nothing
                    targets <- clearedMetadataTargets registryRole eco config
                    fmap (eco,) . for targets $ \cleared -> withRoutedStub (const (status404, [], "absent")) $ \storeStub -> withSweeper $ \deadlines -> do
                        let endpoint = loopbackRegistryUrl (stubBaseUrl storeStub <> registryPath (clearedUrl cleared))
                            provider = CredentialProvider (pure (AuthToken (mkSecret "token-1") Nothing))
                        readManifest <- build (anonymousPorts deadlines){spCredential = Just provider} defaultLimits cleared{cbUrl = endpoint}
                        void (readManifest name)
                        map (filter ((/= "Host") . fst) . capHeaders) <$> allCaptured storeStub
                observed `shouldBe` expectedStoreReads
    it "keeps an anonymous private observation unauthenticated" $ do
        config <- expectConfig (withObservablePrivate (withoutPrivateAuthority codeArtifactEnvVars)) Nothing
        caches <- either (fail . show) pure (privateCaches adapterFor MirrorPreviewer config)
        (_, cleared) <- maybe (fail "missing private cache") pure (Map.lookup Npm caches)
        requests <- captureMetadataRequests Npm observingMetadata cleared Nothing
        map (lookup hAuthorization . requestHeaders) requests `shouldBe` [Nothing, Nothing]
  where
    expectedHeaders PyPI = [Just "Basic YXdzOnRva2VuLTE=", Just "Basic YXdzOnRva2VuLTI="]
    expectedHeaders _ = [Just "Bearer token-1", Just "Bearer token-2"]
    expectedPath PyPI url = encodeUtf8 (joinUrlPath (registryPath url) "simple/leftpad/")
    expectedPath _ url = encodeUtf8 (joinUrlPath (registryPath url) "leftpad")

-- Each ecosystem's store read on a CodeArtifact mirror and cache.
storeReadCases :: [(Ecosystem, [(String, String)], PackageName)]
storeReadCases =
    [ (Npm, twoStoreEnv, aPackage)
    , (PyPI, twoStoreEnv, unscopedPyPI "leftpad")
    , (RubyGems, rubyGemsCodeArtifactEnv, mkPackageName RubyGems Nothing "leftpad")
    ]

-- The mirror's and then the cache's requests, each request's headers without its loopback Host.
expectedStoreReads :: [(Ecosystem, [[[Header]]])]
expectedStoreReads =
    [ (Npm, mirrorAndCache "Bearer token-1" "application/json")
    , (PyPI, mirrorAndCache "Basic YXdzOnRva2VuLTE=" "application/vnd.pypi.simple.v1+json")
    , (RubyGems, mirrorAndCache "Basic YXdzOnRva2VuLTE=" "text/plain")
    ]
  where
    mirrorAndCache authorization accept = replicate 2 [[("User-Agent", userAgent), ("Authorization", authorization), ("Accept", accept), ("Accept-Encoding", "gzip")]]

-- A RubyGems mount that mirrors to and caches from CodeArtifact.
rubyGemsCodeArtifactEnv :: [(String, String)]
rubyGemsCodeArtifactEnv =
    overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" (codeArtifactDomain <> "/ruby/internal/") $
        overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__CODE_ARTIFACT__URL" (codeArtifactDomain <> "/ruby/mirror/") $
            filter (not . (`elem` ["ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", "ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__REGISTRY__URL", "ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__REGISTRY__TOKEN"]) . fst) rubyGemsMirrorEnv

metadataBuilders :: [(RegistryRole, String, StorePorts -> Limits -> ClearedBackend -> IO StoreManifestRead)]
metadataBuilders =
    [ (MirrorPruner, "deletion", \ports limits cleared -> readStoreManifest <$> buildStoreMaintenance ports limits cleared)
    , (MirrorPreviewer, "preview", observingMetadata)
    ]

clearedMetadataTargets :: RegistryRole -> Ecosystem -> Config -> IO [ClearedBackend]
clearedMetadataTargets role eco config = do
    mirrors <- expectRight (snd (vetted role (configMounts config)))
    caches <- expectRight (privateCaches adapterFor role config)
    mirror <- maybe (fail "missing mirror") pure (Map.lookup eco mirrors)
    (_, cache) <- maybe (fail "missing private cache") pure (Map.lookup eco caches)
    pure [mirror, cache]

observingMetadata :: StorePorts -> Limits -> ClearedBackend -> IO StoreManifestRead
observingMetadata ports limits cleared = obReadManifest <$> buildStoreObservation ports limits cleared

captureMetadataRequests ::
    Ecosystem ->
    (StorePorts -> Limits -> ClearedBackend -> IO StoreManifestRead) ->
    ClearedBackend ->
    Maybe CredentialProvider ->
    IO [Request]
captureMetadataRequests eco build cleared credential = withSweeper $ \deadlines -> do
    adapter <- requiredAdapter eco
    captured <- newIORef []
    let capture _ origin name = do
            nativeRead <- prepareRead (metadataRead (adapterMetadata adapter)) origin >>= either (fail . show) pure
            request <- either (fail . show) pure (erRequest nativeRead (ocBaseRequest origin) (ocToken origin) name)
            modifyIORef' captured (<> [request])
            pure (Left MetadataAbsent)
    readManifest <- build (anonymousPorts deadlines){spCredential = credential} defaultLimits cleared{cbFetchManifest = capture}
    replicateM_ 2 (void (readManifest aPackage))
    readIORef captured

protocolHandleFor :: Deadlines -> RegistryRole -> String -> IO StoreMaintenance
protocolHandleFor deadlines role permitDeletion = do
    cleared <- clearedBackendsFor role (verdaccioEnv permitDeletion)
    case Map.elems cleared of
        [backend] -> buildStoreMaintenance (anonymousPorts deadlines) defaultLimits backend
        other -> fail ("expected one cleared protocol store, got " <> show (length other))

anonymousPorts :: Deadlines -> StorePorts
anonymousPorts deadlines =
    StorePorts
        { spTracing = passthroughTracingPort
        , spUserAgent = userAgent
        , spDeadlines = deadlines
        , spCredential = Nothing
        , spBudget = unpacedBudget
        }

unpacedBudget :: BudgetPorts
unpacedBudget =
    BudgetPorts{bpGateFor = const ungatedRequests, bpOverrides = Map.empty, bpNominalPace = testNominalPace}

ungatedRequests :: RequestGate
ungatedRequests = RequestGate{gateSpend = const pass}

aPackage :: PackageName
aPackage = unscopedNpm "leftpad"

-- An origin the absent read never dials, because it answers before it forms a request.
nowhere :: ExchangeClient -> OriginClient
nowhere client = originClient defaultLimits client (unsafeRegistryUrl "https://store.invalid/") Nothing

absentRead :: Text
absentRead = "this build serves the mount's ecosystem no metadata read"

planSpec :: Spec
planSpec = describe "planStoreMaintenance" $ do
    it "builds one handle per cleared store, keyed by the mount that declares it" $ do
        backends <- clearedBackendsFor MirrorPruner twoStoreEnv
        withSweeper $ \deadlines -> do
            outcome <-
                planStoreMaintenance
                    (\_ _ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig)
                    passthroughTracingPort
                    deadlines
                    userAgent
                    unpacedBudget
                    noCredentialProviders
                    defaultLimits
                    backends
            fmap Map.keys outcome `shouldBe` Right (Map.keys backends)

    it "reports a refusal for every store whose client the environment cannot build" $ do
        backends <- clearedBackendsFor MirrorPruner twoStoreEnv
        withSweeper $ \deadlines -> do
            Map.keys backends `shouldBe` [Npm, PyPI]
            outcome <-
                planStoreMaintenance
                    (\_ _ _ -> throwIO NoCredentials)
                    passthroughTracingPort
                    deadlines
                    userAgent
                    unpacedBudget
                    noCredentialProviders
                    defaultLimits
                    backends
            case outcome of
                Right _ -> expectationFailure "expected both store builds to refuse"
                Left errs ->
                    map withoutBacktrace errs
                        `shouldBe` [ StoreMaintenanceUnavailable Npm (ClientBuildFailed "NoCredentials")
                                   , StoreMaintenanceUnavailable PyPI (ClientBuildFailed "NoCredentials")
                                   ]
  where
    -- 'displayException' appends GHC's backtrace, so the assertion reads the reason's own line.
    withoutBacktrace = \case
        StoreMaintenanceUnavailable eco (ClientBuildFailed detail) ->
            StoreMaintenanceUnavailable eco (ClientBuildFailed (T.takeWhile (/= '\n') detail))
        err -> err

probeSpec :: Spec
probeSpec = describe "the private upstream's answer" $ do
    it "refuses on an unsafe repository and says which connection it carries" $
        upstreamFindings [(Npm, Unsafe evidence)]
            `shouldBe` ([], Left [PrivateUpstreamUnsafe Npm evidence])

    it "advises on an open question and boots" $
        upstreamFindings [(Npm, Undecidable NoMechanism)]
            `shouldBe` ([PrivateUpstreamUndecided Npm NoMechanism], Right ())

    it "says nothing at all about a safe repository" $
        upstreamFindings [(Npm, Safe)] `shouldBe` ([], Right ())

    it "reports every mount's answer from one boot, refusals and advisories together" $
        upstreamFindings [(Npm, Unsafe evidence), (PyPI, Undecidable NetworkFailure)]
            `shouldBe` ([PrivateUpstreamUndecided PyPI NetworkFailure], Left [PrivateUpstreamUnsafe Npm evidence])

    it "refuses the mount for a throw no backend read into an answer, rather than passing it off as an open question" $ do
        (advisories, outcome) <- readUpstreamSafety [(Npm, throwIO NoCredentials)]
        advisories `shouldBe` []
        case outcome of
            Right _ -> expectationFailure "expected an unread probe exception to refuse the mount"
            Left [PrivateUpstreamProbeFailed Npm detail] -> detail `shouldSatisfy` T.isPrefixOf "NoCredentials"
            Left errs -> expectationFailure ("expected the probe's own refusal, got: " <> show errs)

    it "answers undecided for a private upstream whose backend does not report its aggregation" $
        for_ [staticEnvVars, withObservablePrivate staticEnvVars] $ \envVars -> do
            endpoint <- privateEndpointFor envVars
            buildUpstreamProbe Npm endpoint `shouldReturn` Undecidable NoMechanism

    it "answers undecided for a codeArtifact endpoint addressing no repository, which no loaded configuration carries" $
        -- 'vetPrivateRepository' refuses this URL at load, so the value is built here rather
        -- than read from a configuration, and the arm stays covered.
        buildUpstreamProbe Npm unaddressablePrivateUpstream `shouldReturn` Undecidable NoMechanism
  where
    evidence = ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs")

-- The npm mount's declared private upstream, failing the case where the fixture declares none.
privateEndpointFor :: [(String, String)] -> IO PrivateEndpoint
privateEndpointFor envVars = do
    config <- expectConfig envVars Nothing
    maybe (fail "the fixture declares no private upstream") pure $
        mntPrivateUpstream =<< Map.lookup Npm (cfgMounts (configApp config))

clearedBackendName :: ClearedBackend -> IO Text
clearedBackendName cleared = withSweeper $ \deadlines -> factBackend . obFacts <$> buildStoreObservation (anonymousPorts deadlines) defaultLimits cleared

budgetSpec :: Spec
budgetSpec = describe "the request capacity a boot resolves for a store" $ do
    it "puts two paths on one host in the same pool" $
        storeScope (unsafeRegistryUrl "https://verdaccio.example.com/one/")
            `shouldBe` storeScope (unsafeRegistryUrl "https://verdaccio.example.com/two/")

    it "derives a backend that publishes no quota from the sweep's own package pace" $ do
        let resolved = resolvedBudget unpacedBudget verdaccio undeclaredBudget
        bgQuotas resolved `shouldBe` Map.singleton StoreRequests testNominalPace
        bgOrigin resolved `shouldBe` QuotaDerived
        bgScope resolved `shouldBe` mkQuotaScope "verdaccio.example.com:443"

    it "takes an operator's declared capacity, matching the URL past case and a trailing slash" $ do
        let resolved = resolvedBudget (overriding "HTTPS://Verdaccio.Example.com" capacity) verdaccio undeclaredBudget
        bgQuotas resolved `shouldBe` Map.singleton StoreRequests 100
        bgOrigin resolved `shouldBe` QuotaDeclared

    it "joins two endpoints of one pool under the scope the operator declared" $
        bgScope (resolvedBudget (overriding key capacity{qoScope = Just "shared"}) verdaccio undeclaredBudget)
            `shouldBe` mkQuotaScope "shared"

    it "scales a request kind the backend costs by the weight the operator gave it" $ do
        let weighted = capacity{qoWeights = Map.singleton DeleteBatch 3}
            resolved = resolvedBudget (overriding key weighted) verdaccio protocolCosts
        Map.lookup DeleteBatch (bgCosts resolved) `shouldBe` Just (Map.singleton StoreRequests 3)
        Map.lookup ListingPage (bgCosts resolved) `shouldBe` Just (Map.singleton StoreRequests 1)

    it "leaves a weight naming a kind the backend costs nothing under with nothing to scale" $ do
        let weighted = capacity{qoWeights = Map.singleton CursorWrite 5}
            resolved = resolvedBudget (overriding key weighted) verdaccio noCursorCost
        Map.lookup CursorWrite (bgCosts resolved) `shouldBe` Nothing
        bgCosts resolved `shouldBe` bgCosts noCursorCost
  where
    key = "https://verdaccio.example.com/"
    verdaccio = unsafeRegistryUrl key
    overriding declaredKey override = unpacedBudget{bpOverrides = Map.singleton declaredKey override}
    capacity = QuotaOverride{qoScope = Nothing, qoQuotas = Map.singleton StoreRequests 100, qoWeights = Map.empty}
    protocolCosts = undeclaredBudget{bgCosts = Map.fromList [(kind, Map.singleton StoreRequests 1) | kind <- requestKinds]}
    noCursorCost = undeclaredBudget{bgCosts = Map.singleton ListingPage (Map.singleton StoreRequests 1)}

-- Twenty-five requests a second: the shipped chunk of fifty every two seconds.
testNominalPace :: Rational
testNominalPace = 25

absentAdapterSpec :: Spec
absentAdapterSpec = describe "vetStoreBackends" $ do
    it "clears that store a read that reports the absent adapter rather than one that invents one" $ do
        mounts <- mountsFor codeArtifactEnvVars
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            case snd (runVet MirrorPruner (vetStoreBackends noAdapter mounts)) of
                Right cleared | [backend] <- Map.elems cleared -> do
                    outcome <- cbFetchManifest backend passthroughTracingPort (nowhere exchanges) aPackage
                    leftToMaybe outcome
                        `shouldBe` Just (MetadataFetch (FetchTransport (transportFault TransportProtocol absentRead)))
                other -> expectationFailure ("expected one cleared store, got: " <> show (void other))

protocolSpec :: Spec
protocolSpec = describe "vetStoreBackends -- a store swept through the ecosystem protocol" $ do
    it "clears the deleting role a consenting Verdaccio target" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        case vetted MirrorPruner mounts of
            ([], Right cleared) -> traverse clearedBackendName (Map.elems cleared) `shouldReturn` ["verdaccio"]
            other -> expectationFailure ("expected one cleared protocol store, got: " <> show (refusalsOf other))

    it "clears the preview role a Verdaccio target carrying no deletion consent" $ do
        -- The key is the operator's own permission to delete, which a preview never exercises, so
        -- it reads the store and reports the verdict rather than refusing to boot on it.
        mounts <- mountsFor (verdaccioEnv "false")
        case vetted MirrorPreviewer mounts of
            ([], Right cleared) -> traverse clearedBackendName (Map.elems cleared) `shouldReturn` ["verdaccio"]
            other -> expectationFailure ("expected one cleared protocol store, got: " <> show (refusalsOf other))

previewCachesSpec :: Spec
previewCachesSpec = describe "vetPrivateCaches" $ do
    it "clears anonymous protocol observation before private deletion consent exists" $ do
        config <- expectConfig (withObservablePrivate (withoutPrivateAuthority codeArtifactEnvVars)) Nothing
        case privateCaches adapterFor MirrorPreviewer config of
            Right caches -> case Map.elems caches of
                [(credential, cleared)] -> withSweeper $ \deadlines -> do
                    isNothing credential `shouldBe` True
                    observation <- buildStoreObservation (anonymousPorts deadlines) defaultLimits cleared
                    obVerifyConsent observation >>= \case
                        Right (ConsentWithheld _) -> pass
                        other -> expectationFailure ("expected a withheld verdict, got: " <> show other)
                _ -> expectationFailure "expected one anonymous private protocol observation"
            Left errors -> expectationFailure (show errors)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.BootError (
    Advisory (PrivateUpstreamUndecided),
    BootError (PrivateUpstreamUnsafe, StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, DeletionNotPermitted, NoProtocolMaintenance, PrivateCacheUnavailable),
    renderBootError,
 )
import Ecluse.Composition.Credential (noCredentialProviders)
import Ecluse.Composition.Maintenance (
    BudgetPorts (BudgetPorts, bpGateFor, bpNominalPace, bpOverrides),
    ClearedBackend (cbAlphabet, cbFetchManifest),
    ResolveMaintenanceAdapter,
    StorePorts (..),
    buildStoreMaintenance,
    buildStoreObservation,
    buildUpstreamProbe,
    planStoreMaintenance,
    readUpstreamSafety,
    resolvedBudget,
    storeScope,
    upstreamFindings,
    vetPrivateCaches,
    vetStoreBackends,
 )
import Ecluse.Composition.Support (
    clearedUrl,
    codeArtifactEnvVars,
    codeArtifactMirrorUrl,
    expectConfig,
    noMaintenanceBackend,
    overrideEnv,
    privateInventoryRefusal,
    staticEnvVars,
    withObservablePrivate,
    withoutMirrorTargetToken,
    withoutMirrorTargetUrl,
    withoutPrivateUpstreamUrl,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    AppConfig (cfgMounts),
    Config (configApp, configMounts),
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    MountConfig (mntPrivateUpstream),
    MountMap,
    QuotaOverride (QuotaOverride, qoQuotas, qoScope, qoWeights),
    PrivateEndpoint,
    StoreBackend,
    StoreTag (TagVerdaccio),
    sbControl,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (AdapterMaintenance, maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete),
 )
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    NameAlphabet,
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryWorthwhile),
    StoreFacts (..),
    StoreFault (faultRetry),
    StoreMaintenance (readStoreManifest, storeFacts, verifyConsent),
    StoreObservation (obFacts, obVerifyConsent),
    noNameAlphabet,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaDimension (StoreRequests),
    QuotaOrigin (QuotaDeclared, QuotaDerived),
    RequestGate (RequestGate, gateSpend),
    RequestKind (CursorWrite, DeleteBatch, ListingPage),
    StoreBudget (bgCosts, bgOrigin, bgQuotas, bgScope),
    mkQuotaScope,
    requestKinds,
    undeclaredBudget,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection),
    RepositoryName (RepositoryName),
    UndecidabilityReason (NetworkFailure, NoMechanism),
    UnsafeReason (ConfigurationEvidence),
    UpstreamSafety (Safe, Undecidable, Unsafe),
 )
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Origin (OriginClient (OriginClient, ocBaseUrl, ocLimits, ocManager, ocToken))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (casRepository)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (unsafeRegistryUrl, unscopedNpm)
import Ecluse.Test.Port (passthroughTracingPort)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

spec :: Spec
spec = do
    boundarySpec
    passSpec
    protocolSpec
    buildSpec
    planSpec
    probeSpec
    previewCachesSpec
    budgetSpec

{- Both issuers at the boundary their refusals draw: a store a pass refused reaches no handle at
all, because every cleared value in the map below came from the pass that returned it. -}
boundarySpec :: Spec
boundarySpec = describe "the cleared backend boundary" $ do
    it "issues a mirror store's backend only where the deleting role's pass cleared it" $ do
        withheld <- mountsFor (verdaccioEnv "false")
        void (snd (vetted MirrorPruner withheld))
            `shouldBe` Left [StoreMaintenanceUnavailable Npm (DeletionNotPermitted TagVerdaccio)]
        consenting <- mountsFor (verdaccioEnv "true")
        fmap Map.keys (snd (vetted MirrorPruner consenting)) `shouldBe` Right [Npm]

    it "issues a private cache's backend only where the cache's own pass cleared it" $ do
        refused <- expectConfig staticEnvVars Nothing
        void (privateCaches adapterFor MirrorPruner refused) `shouldBe` Left [privateInventoryRefusal]
        cleared <- expectConfig codeArtifactEnvVars Nothing
        fmap Map.keys (privateCaches adapterFor MirrorPruner cleared) `shouldBe` Right [Npm]

{- The rule as the boot applies it: over the loaded mounts, under each role. The deleting role
is the one that refuses, and the checker's warning for it is what a writing role leaves behind. -}
passSpec :: Spec
passSpec = describe "vetStoreBackends" $ do
    it "clears the deleting role the backend for each mirror target this build can sweep" $ do
        mounts <- mountsFor codeArtifactEnvVars
        let (advisories, outcome) = vetted MirrorPruner mounts
        advisories `shouldBe` []
        fmap (map clearedUrl . Map.elems) outcome `shouldBe` Right [codeArtifactMirrorUrl]
        fmap Map.keys outcome `shouldBe` Right [Npm]

    it "refuses the deleting role a mirror target this build cannot sweep, naming the key" $ do
        mounts <- mountsFor staticEnvVars
        case vetted MirrorPruner mounts of
            ([], Left [err]) -> do
                err `shouldBe` noMaintenanceBackend
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"
            other -> expectationFailure ("expected the one maintenance refusal, got: " <> show (refusalsOf other))

    it "clears a writing role no backend, and neither refuses nor advises on a target it cannot sweep" $ do
        -- Only the Dredger deletes, so only its pass reads the rule. The checker still names
        -- the Dredger's refusal for this configuration, so an operator learns of it once.
        mounts <- mountsFor staticEnvVars
        clearsNothing (vetted MirrorWriter mounts) `shouldBe` True

    it "clears nothing and refuses nothing for a mount that declares no mirror target" $ do
        mounts <- mountsFor (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars))
        clearsNothing (vetted MirrorPruner mounts) `shouldBe` True

    it "clears a vendor store with no alphabet when this build ships the ecosystem no adapter" $ do
        -- Unreachable through a real boot, which refuses that mount as MissingAdapter first. The
        -- store still clears, walked as one bucket, rather than the pass inventing an alphabet.
        mounts <- mountsFor codeArtifactEnvVars
        fmap (map clearedAlphabet . Map.elems) (snd (runVet MirrorPruner (vetStoreBackends noAdapter mounts)))
            `shouldBe` Right [noNameAlphabet]

    it "clears that store a read that reports the absent adapter rather than one that invents one" $ do
        mounts <- mountsFor codeArtifactEnvVars
        manager <- newManager defaultManagerSettings
        case snd (runVet MirrorPruner (vetStoreBackends noAdapter mounts)) of
            Right cleared | [backend] <- Map.elems cleared -> do
                outcome <- cbFetchManifest backend passthroughTracingPort (nowhere manager) aPackage
                leftToMaybe outcome
                    `shouldBe` Just (MetadataFetch (FetchTransport (transportFault TransportProtocol absentRead)))
            other -> expectationFailure ("expected one cleared store, got: " <> show (void other))

{- The protocol arm: a store with no vendor control plane, swept through the ecosystem's own
verbs. Consent and the ecosystem's verbs are separate refusals, and the writing role reads neither. -}
protocolSpec :: Spec
protocolSpec = describe "vetStoreBackends -- a store swept through the ecosystem protocol" $ do
    it "clears the deleting role a consenting Verdaccio target" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        case vetted MirrorPruner mounts of
            ([], Right cleared) -> traverse clearedBackendName (Map.elems cleared) `shouldReturn` ["verdaccio"]
            other -> expectationFailure ("expected one cleared protocol store, got: " <> show (refusalsOf other))

    it "refuses the deleting role a Verdaccio target carrying no deletion consent" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        refusalsOf (vetted MirrorPruner mounts)
            `shouldBe` Just [StoreMaintenanceUnavailable Npm (DeletionNotPermitted TagVerdaccio)]

    it "names the consent key an operator must set in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        renderedRefusals (vetted MirrorPruner mounts)
            `shouldSatisfy` any (T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION")

    it "clears the preview role a Verdaccio target carrying no deletion consent" $ do
        -- The key is the operator's own permission to delete, which a preview never exercises, so
        -- it reads the store and reports the verdict rather than refusing to boot on it.
        mounts <- mountsFor (verdaccioEnv "false")
        case vetted MirrorPreviewer mounts of
            ([], Right cleared) -> traverse clearedBackendName (Map.elems cleared) `shouldReturn` ["verdaccio"]
            other -> expectationFailure ("expected one cleared protocol store, got: " <> show (refusalsOf other))

    it "refuses the preview role a target this build reaches no control plane for" $ do
        -- Everything but the consent key still stands, so a preview of a store this build cannot
        -- sweep refuses exactly as the deleting role's own boot does.
        mounts <- mountsFor (verdaccioEnv "true")
        refusalsOf (runVet MirrorPreviewer (vetStoreBackends withoutMaintenance mounts))
            `shouldBe` Just [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "refuses the deleting role an ecosystem whose protocol spells no delete" $ do
        -- The rule must not turn on which ecosystem the mount names, so the adapter is
        -- injected and the refusal is the rule's own rather than the registry's.
        mounts <- mountsFor (verdaccioEnv "true")
        refusalsOf (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldBe` Just [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "names the protocol in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        renderedRefusals (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldSatisfy` any (T.isInfixOf "npm protocol carries no package listing or version delete")

    it "boots every writing role on a configuration the Dredger refuses for either reason" $ do
        withheld <- mountsFor (verdaccioEnv "false")
        clearsNothing (vetted MirrorWriter withheld) `shouldBe` True
        consenting <- mountsFor (verdaccioEnv "true")
        clearsNothing (runVet MirrorWriter (vetStoreBackends withoutMaintenance consenting)) `shouldBe` True

{- The live build of a protocol store's handle. It opens a connection to nothing, so the facts
and the verdicts it supplies are readable without a store to dial. -}
buildSpec :: Spec
buildSpec = describe "buildStoreMaintenance -- a store swept through the ecosystem protocol" $ do
    it "supplies the backend's standing facts under the tag the store was declared with" $ do
        handle <- protocolHandleFor MirrorPruner "true"
        let facts = storeFacts handle
        factBackend facts `shouldBe` "verdaccio"
        factDeleteCeiling facts `shouldBe` AtMost 1
        factRefill facts `shouldBe` RefillPermitted
        factCompletion facts `shouldBe` CompletesOnCall

    it "reads a manifest over the store's own endpoint, not the public upstream" $ do
        -- The endpoint answers nothing, so the read reaching the network at all is what this
        -- shows: the handle carries the ecosystem's codec rather than an absent read.
        handle <- protocolHandleFor MirrorPruner "true"
        (fmap faultRetry . leftToMaybe <$> readStoreManifest handle aPackage)
            `shouldReturn` Just RetryWorthwhile

    it "grants consent on the store the pass cleared" $ do
        handle <- protocolHandleFor MirrorPruner "true"
        verifyConsent handle `shouldReturn` Right ConsentGranted

    it "withholds it, naming the key an operator sets, on a store carrying none" $ do
        -- Only the preview role's pass clears a store carrying no consent, so its boot is the
        -- one that builds the handle reporting the withheld verdict.
        handle <- protocolHandleFor MirrorPreviewer "false"
        verifyConsent handle >>= \case
            Right (ConsentWithheld descriptor) ->
                descriptor `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"
            other -> expectationFailure ("expected a withheld verdict, got: " <> show other)

{- The handle the live builder makes for the cleared Verdaccio store, under the role whose pass
clears it and the deletion consent an operator wrote. -}
protocolHandleFor :: RegistryRole -> String -> IO StoreMaintenance
protocolHandleFor role permitDeletion = do
    cleared <- clearedBackendsFor role (verdaccioEnv permitDeletion)
    case Map.elems cleared of
        [backend] -> buildStoreMaintenance anonymousPorts defaultLimits backend
        other -> fail ("expected one cleared protocol store, got " <> show (length other))

{- The ports a handle is built over when no live process supplies them: a passthrough tracing
port, and no credential, which the store's origin then presents none of. -}
anonymousPorts :: StorePorts
anonymousPorts =
    StorePorts
        { spTracing = passthroughTracingPort
        , spCredential = Nothing
        , spBudget = unpacedBudget
        }

{- Ports that count nothing and wait for nothing, for the cases about the handles a boot builds
rather than the rate they run at. -}
unpacedBudget :: BudgetPorts
unpacedBudget =
    BudgetPorts{bpGateFor = const ungatedRequests, bpOverrides = Map.empty, bpNominalPace = testNominalPace}

ungatedRequests :: RequestGate
ungatedRequests = RequestGate{gateSpend = const pass}

aPackage :: PackageName
aPackage = unscopedNpm "leftpad"

-- An origin the absent read never dials, because it answers before it forms a request.
nowhere :: Manager -> OriginClient
nowhere manager =
    OriginClient
        { ocBaseUrl = unsafeRegistryUrl "https://store.invalid/"
        , ocManager = manager
        , ocToken = Nothing
        , ocLimits = defaultLimits
        }

absentRead :: Text
absentRead = "this build serves the mount's ecosystem no metadata read"

{- The environment tier over the cleared backends. It builds one handle per store, and its
refusals accumulate rather than stopping at the first store whose client cannot be built. -}
planSpec :: Spec
planSpec = describe "planStoreMaintenance" $ do
    it "builds one handle per cleared store, keyed by the mount that declares it" $ do
        backends <- clearedBackendsFor MirrorPruner twoStoreEnv
        outcome <-
            planStoreMaintenance
                (\_ _ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig)
                passthroughTracingPort
                unpacedBudget
                noCredentialProviders
                defaultLimits
                backends
        fmap Map.keys outcome `shouldBe` Right (Map.keys backends)

    it "reports a refusal for every store whose client the environment cannot build" $ do
        backends <- clearedBackendsFor MirrorPruner twoStoreEnv
        Map.keys backends `shouldBe` [Npm, PyPI]
        outcome <-
            planStoreMaintenance
                (\_ _ _ -> throwIO NoStoreClient)
                passthroughTracingPort
                unpacedBudget
                noCredentialProviders
                defaultLimits
                backends
        case outcome of
            Right _ -> expectationFailure "expected both store builds to refuse"
            Left errs ->
                map withoutBacktrace errs
                    `shouldBe` [ StoreMaintenanceUnavailable Npm (ClientBuildFailed "NoStoreClient")
                               , StoreMaintenanceUnavailable PyPI (ClientBuildFailed "NoStoreClient")
                               ]
  where
    -- 'displayException' appends GHC's backtrace, so the assertion reads the reason's own line.
    withoutBacktrace = \case
        StoreMaintenanceUnavailable eco (ClientBuildFailed detail) ->
            StoreMaintenanceUnavailable eco (ClientBuildFailed (T.takeWhile (/= '\n') detail))
        err -> err

{- What a boot does with each backend's answer about its private upstream, and which backends
answer at all. The severity is the same for every role that asks. -}
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

    it "leaves a probe whose client could not be built undecided, rather than refusing the role" $
        readUpstreamSafety [(Npm, throwIO NoStoreClient)]
            `shouldReturn` ([PrivateUpstreamUndecided Npm NetworkFailure], Right ())

    it "answers undecided for a private upstream whose backend does not report its aggregation" $
        for_ [staticEnvVars, withObservablePrivate staticEnvVars] $ \envVars -> do
            endpoint <- privateEndpointFor envVars
            buildUpstreamProbe Npm endpoint `shouldReturn` Undecidable NoMechanism
  where
    evidence = ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs")

-- The npm mount's declared private upstream, failing the case where the fixture declares none.
privateEndpointFor :: [(String, String)] -> IO PrivateEndpoint
privateEndpointFor envVars = do
    config <- expectConfig envVars Nothing
    maybe (fail "the fixture declares no private upstream") pure $
        mntPrivateUpstream =<< Map.lookup Npm (cfgMounts (configApp config))

-- | The typed stand-in for amazonka's credential-discovery failure.
data NoStoreClient = NoStoreClient
    deriving stock (Show)

instance Exception NoStoreClient

-- The pass as the boot runs it, over this build's own adapter registry.
vetted :: RegistryRole -> MountMap -> ([Advisory], Either [BootError] (Map Ecosystem ClearedBackend))
vetted role mounts = runVet role (vetStoreBackends adapterFor mounts)

-- | An ecosystem this build ships no adapter for at all.
noAdapter :: ResolveMaintenanceAdapter
noAdapter _ = Nothing

-- | The bucket alphabet a cleared vendor store carries, and 'noNameAlphabet' for any other arm.
clearedAlphabet :: ClearedBackend -> NameAlphabet
clearedAlphabet = cbAlphabet

-- | This build's adapters with their maintenance slice emptied: an ecosystem that fills neither verb.
withoutMaintenance :: ResolveMaintenanceAdapter
withoutMaintenance eco =
    adapterFor eco <&> \adapter ->
        adapter
            { adapterMaintenance =
                AdapterMaintenance
                    { maintenanceListing = Nothing
                    , maintenanceVersionDelete = Nothing
                    , maintenanceAlphabet = noNameAlphabet
                    }
            }

{- The name a cleared store's own facts carry, which names the arm the pass cleared: a protocol
store answers under its declared tag, and a vendor store under the vendor's. -}
clearedBackendName :: ClearedBackend -> IO Text
clearedBackendName cleared = factBackend . obFacts <$> buildStoreObservation anonymousPorts defaultLimits cleared

-- A pass that logged nothing and cleared no store: what every writing role's pass looks like.
clearsNothing :: ([Advisory], Either [BootError] (Map Ecosystem ClearedBackend)) -> Bool
clearsNothing = \case
    ([], Right cleared) -> Map.null cleared
    _ -> False

refusalsOf :: ([Advisory], Either [BootError] a) -> Maybe [BootError]
refusalsOf = leftToMaybe . snd

renderedRefusals :: ([Advisory], Either [BootError] a) -> [Text]
renderedRefusals = maybe [] (map renderBootError) . refusalsOf

-- The resolved mounts an environment layer loads to: the input the rule reads.
mountsFor :: [(String, String)] -> IO MountMap
mountsFor env = configMounts <$> expectConfig env Nothing

-- The private-cache pass as a boot runs it, over the resolver the case drives it with.
privateCaches :: ResolveMaintenanceAdapter -> RegistryRole -> Config -> Either [BootError] (Map Ecosystem (Maybe StoreBackend, ClearedBackend))
privateCaches resolveAdapter role config =
    snd (runVet role (vetPrivateCaches resolveAdapter (cfgMounts (configApp config)) (configMounts config)))

-- The repository a cleared cache's own backend declaration addresses, absent on any other arm.
privateRepository :: Maybe StoreBackend -> Maybe Text
privateRepository backend = case sbControl <$> backend of
    Just (ControlCodeArtifact store) -> Just (casRepository store)
    Just ControlNone -> Nothing
    Just (ControlProtocol _ _) -> Nothing
    Nothing -> Nothing

-- The backends a role's own pass clears for an environment layer, failing the test on a refusal.
clearedBackendsFor :: RegistryRole -> [(String, String)] -> IO (Map Ecosystem ClearedBackend)
clearedBackendsFor role env = do
    mounts <- mountsFor env
    either (\errs -> fail ("the backend rule refused: " <> show errs)) pure (snd (vetted role mounts))

-- | 'codeArtifactEnvVars' with a second mirrored mount, so the plan has two stores to build.
twoStoreEnv :: [(String, String)]
twoStoreEnv =
    overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" pypiInternalEndpoint $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__URL" pypiEndpoint codeArtifactEnvVars

-- | 'staticEnvVars' mirroring to a Verdaccio store, under the written deletion consent.
verdaccioEnv :: String -> [(String, String)]
verdaccioEnv permitDeletion =
    overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION" permitDeletion $
        overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL" "https://verdaccio.example.test/" $
            overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN" "write-token" $
                withoutMirrorTargetToken (withoutMirrorTargetUrl staticEnvVars)

pypiEndpoint :: (IsString s) => s
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

pypiInternalEndpoint :: (IsString s) => s
pypiInternalEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/internal/"

-- | A private CodeArtifact cache on its own repository, distinct from the mirror target's.
retainedEndpoint :: (IsString s) => s
retainedEndpoint = "https://cache-999900001111.d.codeartifact.us-west-2.amazonaws.com/npm/retained/"

previewCachesSpec :: Spec
previewCachesSpec = describe "vetPrivateCaches" $ do
    it "clears anonymous protocol observation before private deletion consent exists" $ do
        config <- expectConfig (withObservablePrivate (withoutPrivateAuthority codeArtifactEnvVars)) Nothing
        case privateCaches adapterFor MirrorPreviewer config of
            Right caches -> case Map.elems caches of
                [(credential, cleared)] -> do
                    isNothing credential `shouldBe` True
                    observation <- buildStoreObservation anonymousPorts defaultLimits cleared
                    obVerifyConsent observation >>= \case
                        Right (ConsentWithheld _) -> pass
                        other -> expectationFailure ("expected a withheld verdict, got: " <> show other)
                _ -> expectationFailure "expected one anonymous private protocol observation"
            Left errors -> expectationFailure (show errors)

    it "refuses a generic registry without an inventory backend" $ do
        config <- expectConfig staticEnvVars Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
        void result `shouldBe` Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable "registry has no inventory control plane")]

    it "accumulates private inventory refusals across mounts" $ do
        let env =
                [ ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/pypi/")
                , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__URL", "https://test-111122223333.d.codeartifact.us-east-1.amazonaws.com/pypi/mirror/")
                ]
                    <> [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/")]
                    <> withoutPrivateUpstreamUrl codeArtifactEnvVars
        config <- expectConfig env Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
            expected = [StoreMaintenanceUnavailable eco (PrivateCacheUnavailable "registry has no inventory control plane") | eco <- [Npm, PyPI]]
        void result `shouldBe` Left expected

    it "refuses a private protocol backend with no listing capability" $ do
        config <- expectConfig (withObservablePrivate (withoutPrivateAuthority codeArtifactEnvVars)) Nothing
        let result = privateCaches withoutMaintenance MirrorPreviewer config
        void result `shouldBe` Left [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "clears the deleting role an independently consenting private store" $ do
        config <- expectConfig codeArtifactEnvVars Nothing
        let result = privateCaches adapterFor MirrorPruner config
        fmap Map.null result `shouldBe` Right False

    for_ ["TOKEN", "PERMIT_DELETION"] $ \missing ->
        it ("refuses private deletion without its own " <> missing) $ do
            let key = "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__" <> missing
                env = filter ((/= key) . fst) codeArtifactEnvVars
            config <- expectConfig env Nothing
            let result = privateCaches adapterFor MirrorPruner config
            case result of
                Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable detail)] -> detail `shouldSatisfy` T.isInfixOf (toText key)
                other -> expectationFailure ("expected the private authority refusal, got " <> show (void other))

    it "adds no private observation when the mount has no mirror target" $ do
        config <- expectConfig (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars)) Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
        fmap Map.null result `shouldBe` Right True

    it "clears the private CodeArtifact cache on the repository its own endpoint addresses" $ do
        let env =
                ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", retainedEndpoint)
                    : withoutPrivateUpstreamUrl codeArtifactEnvVars
        config <- expectConfig env Nothing
        case privateCaches adapterFor MirrorPreviewer config of
            Right caches -> case Map.elems caches of
                [(backend, cleared)] -> do
                    clearedUrl cleared `shouldBe` retainedEndpoint
                    privateRepository backend `shouldBe` Just "retained"
                _ -> expectationFailure "expected one cleared private CodeArtifact cache"
            Left errors -> expectationFailure (show errors)

    it "refuses a private CodeArtifact endpoint for a different package format" $ do
        let env =
                ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", "https://cache-999900001111.d.codeartifact.us-west-2.amazonaws.com/pypi/retained/")
                    : withoutPrivateUpstreamUrl codeArtifactEnvVars
        config <- expectConfig env Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
        void result `shouldSatisfy` isLeft

withoutPrivateAuthority :: [(String, String)] -> [(String, String)]
withoutPrivateAuthority = filter (\(key, _) -> key /= "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__TOKEN" && key /= "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__PERMIT_DELETION")

{- A store's capacity pool is its own authority, and a CodeArtifact repository's is the account
and Region that meters it. A backend publishing no quota is derived one from the sweep's own pace. -}
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

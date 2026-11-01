-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo operations for every canonical Dredger law against the running product image.
module Ecluse.E2E.Cargo.DredgerE2ESpec (
    cargoSweepDriver,
    cargoDredgerFixture,
    cargoDredgerProfile,
    cargoDredgerSafetyContract,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.E2E.Cargo.MirrorInstallE2ESpec (cargoMirrorComplete)
import Ecluse.E2E.Fixtures.Advisories (maliciousAdvisoryId)
import Ecluse.E2E.Fixtures.Cargo.Dredger
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness.Advisories (advisoryStoreEnv, publishAdvisoryExport)
import Ecluse.E2E.Harness.Cargo (CargoProject)
import Ecluse.E2E.Harness.CargoDredger qualified as Native
import Ecluse.E2E.Harness.CargoDredger.Client (cargoSweepClientOps)
import Ecluse.E2E.Harness.CargoRegistry (awaitCargoRegistry)
import Ecluse.E2E.Harness.Client (shouldSucceed)
import Ecluse.E2E.Harness.CodeArtifact (codeArtifactDeleteRequests, creditCodeArtifactDeletes, withCodeArtifactDeleteAudit)
import Ecluse.E2E.Harness.CodeArtifact.Roles (refusedAwsIdentity, refusedIdentityReports)
import Ecluse.E2E.Harness.Docker (RoleRun (..), awaitContainerLog, containerLogs, runRoleOnce, sharedMirrorQueue, withDataPlane, withMirrorRole)
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.E2E.Harness.NativeEdge (observedProxy)
import Ecluse.E2E.Harness.Proxy (awaitProxyLog)
import Ecluse.E2E.Harness.Sweep
import Ecluse.E2E.Harness.Types (GlobalDataPlane, Topology (..))
import Ecluse.E2E.Harness.Verdaccio (awaitMirrorWindow)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Sweep.Composition (SweepDriver (..))
import Ecluse.Test.Contract.Sweep.Types
import Ecluse.Test.Contract.Types
import Ecluse.Test.Osv (CorpusVersion (..))
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.Cargo.Fixtures (crateName, crateVersion, releaseCrate)

-- | Each canonical family owns a fresh pair of native stores and a remote state service.
cargoSweepDriver :: ConcreteFixture FamilyMetadata -> PhaseRecorder -> SweepDriver Native.CargoSweepPlane Native.CargoSweepStore CargoProject
cargoSweepDriver fixture recorder =
    SweepDriver
        { withIdentitySweep = acquire withIdentity
        , withAdvisorySweep = acquire withAdvisory
        , withMaliciousSweep = acquire withAdvisoryStores
        , withRecoverySweep = acquire withRecovery
        , withRolloutSweep = acquire withRollout
        , sweepIdentityFixture = identityFixture
        , sweepAdvisoryFixture = advisoryFixture
        , sweepMaliciousFixture = advisoryFixture{advisoryDenial = MaliciousDenial maliciousAdvisoryId, advisoryTally = "examined 4, deleted 2, kept 2, guard-skipped 0"}
        , sweepRecoveryFixture = recoveryFixture
        , sweepRolloutFixture = rolloutFixture
        , sweepStoreOps = storeOps
        , sweepClientOps = cargoSweepClientOps
        , sweepGuardedOps = sweepOps [("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", firstPartyName)]
        , sweepPlainOps = sweepOps []
        , sweepAdvisoryOps = advisoryOps
        , sweepMaliciousOps = advisoryOps{publishNextGeneration = \plane -> observeRun <$> publishAdvisoryExport (Native.cspGlobal plane) Cargo dredgerMaliciousExportBase}
        , sweepRecoveryOps = recoveryOps
        , sweepRolloutOps = rolloutOps
        , sweepAdmissionOutage = Nothing
        }
  where
    acquire resource action = timedFixture recorder (withFixture fixture) (timedFixtureWith recorder resource action)

-- | Acquisition starts Cargo, the isolated TLS stub and the shared SQS/S3 emulator.
cargoDredgerFixture :: ConcreteFixture FamilyMetadata
cargoDredgerFixture =
    ConcreteFixture
        { withFixture = withDataPlane (Topology [Cargo] False True)
        , fixtureMetadata = FamilyMetadata "cargo/dredger" "cargo/maintenance" capabilities prerequisites resources isolation
        }

-- | The profile names the deterministic CodeArtifact protocol fixture, never a cloud account.
cargoDredgerProfile :: Profile
cargoDredgerProfile = Profile "cargo/maintenance" ["cargo"] ["codeartifact-fixture", "sqs", "s3"] capabilities

capabilities, prerequisites, resources, isolation :: [Text]
capabilities = ["read", "mirror", "maintenance", "advisories"]
prerequisites = ["docker", "image", "openssl", "cargo", "rustc"]
resources = ["proxy", "nginx", "codeartifact-fixture", "ministack"]
isolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]

withIdentity :: ((Native.CargoSweepPlane, Native.CargoSweepStore, Native.CargoSweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withIdentity action global = Native.withCargoSweepPlane global $ \plane ->
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [deniedPackage, previewPackage, keptPackage]
        for_ ["1.0.0", "2.0.0"] $ \version -> publish (Native.directStore proxy) firstPartyName version
        action (plane, proxy, Native.cacheStore proxy)

withAdvisory :: ((Native.CargoSweepPlane, Native.CargoSweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisory action = withAdvisoryStores (\(plane, proxy, _) -> action (plane, proxy))

-- The first generation precedes boot because readiness waits for a successful advisory sync.
withAdvisoryStores :: ((Native.CargoSweepPlane, Native.CargoSweepStore, Native.CargoSweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStores action global = Native.withCargoSweepPlane global $ \plane -> do
    run <- compileGeneration plane CorpusV1
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    Native.withSweepProxy plane (advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules)]) Nothing [] (\proxy -> action (plane, proxy, Native.cacheStore proxy))

withRecovery :: ((Native.CargoSweepPlane, Native.CargoSweepStore, Native.CargoSweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRecovery action global = Native.withCargoSweepPlane global $ \plane -> do
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \seeder -> do
        seedMirror seeder recoveryPackages
        seedCache seeder recoveryPackages
    let version = recoveryDeniedVersion recoveryFixture
    let denials = (packageName siblingPackage <> "@" <> version) : map packageName [refusedPackage, readmittedPackage, missingPackage, latePackage]
    Native.withSweepProxy plane [("ECLUSE_RULES", denyingRules denials)] Nothing [] $ \proxy ->
        action (plane, proxy, Native.cacheStore proxy)

withRollout :: ((Native.CargoSweepPlane, Native.CargoSweepStore, Native.CargoSweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRollout action global = Native.withCargoSweepPlane global $ \plane ->
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [readmittedPackage, missingPackage]
        seedCache proxy [deniedPackage, readmittedPackage, missingPackage]
        action (plane, proxy, Native.cacheStore proxy)

seedMirror :: Native.CargoSweepStore -> [PackageFixture] -> IO ()
seedMirror proxy packages = do
    for_ packages $ \package -> do
        for_ (packageVersions package) $ \version ->
            void (clientInstall cargoSweepClientOps proxy (packageName package) (Just version) >>= shouldSucceed)
        storeAwaitVersions storeOps proxy (packageName package) (sort (packageVersions package)) `shouldReturn` sort (packageVersions package)
    let plane = Native.cssPlane proxy
        expected = [release | release <- Native.cspReleases plane, package <- packages, crateName (releaseCrate release) == packageName package, crateVersion (releaseCrate release) `elem` packageVersions package]
    awaitCargoRegistry (Native.cspRegistry plane) expected `shouldReturn` True

seedCache :: Native.CargoSweepStore -> [PackageFixture] -> IO ()
seedCache proxy packages = for_ packages $ \package ->
    for_ (packageVersions package) (publish (Native.cacheStore proxy) (packageName package))

publish :: Native.CargoSweepStore -> Text -> Text -> IO ()
publish store name version = void (clientPublish cargoSweepClientOps store name version >>= shouldSucceed)

permissiveEnv :: [(Text, Text)]
permissiveEnv = [("ECLUSE_RULES", permissiveRules)]

storeOps :: StoreOps Native.CargoSweepStore
storeOps =
    StoreOps
        { storeSnapshot = Native.storeInventory
        , storeListing = (`Native.storeNamesUnder` "")
        , storeNamesUnder = Native.storeNamesUnder
        , storeVersions = Native.storeVersions
        , storeHasVersion = \store name version -> elem version <$> Native.storeVersions store name
        , storeAwaitVersions = \store name wanted -> pollUntil 40 500000 (== wanted) (Native.storeVersions store name)
        , storeArtifact = \store name version -> second LBS.length <$> Native.storeArtifactBytes store name version
        , storeArtifactBytes = Native.storeArtifactBytes
        , storeAwaitListed = \store name -> do
            present <- pollUntil 40 500000 (elem name) (Native.storeNamesUnder store "")
            pure (if name `elem` present then Nothing else Just ("CodeArtifact did not list " <> name))
        }

sweepOps :: [(Text, Text)] -> SweepOps Native.CargoSweepPlane
sweepOps extra = SweepOps{runSweep = (`runNativeSweep` extra), awaitWrites = awaitMirrorWindow}

recoveryOps :: RecoveryOps Native.CargoSweepPlane Native.CargoSweepStore
recoveryOps =
    RecoveryOps
        { withRelaxedClient = \plane -> Native.withSweepProxy plane permissiveEnv Nothing []
        , withSourceWithheld = Native.withSourceWithheld
        , withDeletesRefused = Native.withDeleteRefusal
        , directPublisher = pure . Native.directStore
        , refusedDeleteTarget = "privateUpstream " <> Native.repositoryUrl "cache"
        , refusedDeleteDiagnostic = "the backend refused the delete, NOT_ALLOWED: fixture refuses cache deletion"
        }

rolloutOps :: RolloutOps Native.CargoSweepPlane Native.CargoSweepStore
rolloutOps =
    RolloutOps
        { rolloutQueue = sharedMirrorQueue . Native.cspGlobal
        , withServingRole = \plane queue policy ->
            Native.withSweepProxy
                plane
                (servingEnvironment policy <> [("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", Native.repositoryUrl "mirror")])
                (Just queue)
                ["proxy", "--no-worker"]
        , withWorkerRole = withObservedWorker
        , awaitPublished = \worker release -> do
            (name, version) <- workerRelease release
            awaitContainerLog worker (cargoMirrorComplete name version) 240
        , workerLogs = containerLogs
        , publicationCoordinate = \release -> do
            (name, version) <- workerRelease release
            pure (name <> "@" <> version)
        , creditSweep = creditDeletes
        }
  where
    servingEnvironment (IdentityPolicy names) = [("ECLUSE_RULES", denyingRules names)]
    servingEnvironment policy = policyEnvironment policy

workerRelease :: Text -> IO (Text, Text)
workerRelease release = case T.splitOn "@" release of
    [name, version] | not (T.null name) -> pure (name, version)
    _ -> fail "expected a Cargo package@version fixture coordinate"

withObservedWorker :: Native.CargoSweepPlane -> Text -> (String -> IO a) -> IO a
withObservedWorker plane queue action =
    withCodeArtifactDeleteAudit (Native.cspControl plane) (Native.cspDredgerDeletes plane) $
        withMirrorRole (Native.cspGlobal plane) environment action
  where
    environment =
        Native.proxyRoleEnv
            <> permissiveEnv
            <> [("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873"), ("ECLUSE_QUEUE__URL", queue), ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")]

creditDeletes :: Native.CargoSweepPlane -> IO SweepRun -> IO SweepRun
creditDeletes plane = creditCodeArtifactDeletes (Native.cspControl plane) (Native.cspDredgerDeletes plane)

advisoryOps :: AdvisoryOps Native.CargoSweepPlane Native.CargoSweepStore
advisoryOps =
    AdvisoryOps
        { publishNextGeneration = \plane -> observeRun <$> compileGeneration plane CorpusV2
        , awaitGenerationSwap = \store -> awaitProxyLog (observedProxy (Native.cssObserved store)) ((> 1) . length . T.breakOnAll "advisory database swapped in") 240
        }

compileGeneration :: Native.CargoSweepPlane -> CorpusVersion -> IO RoleRun
compileGeneration plane = publishAdvisoryExport (Native.cspGlobal plane) Cargo . dredgerExportBase

identityFixture :: IdentityFixture
identityFixture =
    IdentityFixture
        { identityCasePrefix = "cargo/dredger/"
        , identityDenied = deniedPackage
        , identityPreview = previewPackage
        , identitySeeded = Map.fromList ((firstPartyName, ["1.0.0", "2.0.0"]) : [(packageName p, [packageVersion p]) | p <- [deniedPackage, previewPackage, keptPackage]])
        , identityFirstPartyName = firstPartyName
        , identityFirstPartyVersions = ["1.0.0", "2.0.0"]
        , identityPublishVersion = "1.0.0"
        , identityCacheOnlyVersion = "3.0.0"
        , identityPreviewVersion = "2.0.0"
        , identityListingPrefix = "e"
        , identityAbsentPrefix = "z"
        , identityConsentDiagnostic = consentDiagnostic
        , identityConsentEvents = ["mirror sweep cycle halted: the cargo store on mirrorTarget " <> Native.repositoryUrl "mirror" <> " carries no deletion consent marker: " <> consentDiagnostic <> "; examined 0, deleted 0, kept 0, guard-skipped 0"]
        , identityMirrorDiagnostic = "cargo mirror store on mirrorTarget " <> Native.repositoryUrl "mirror"
        , identityCacheDiagnostic = "cargo private cache on privateUpstream " <> Native.repositoryUrl "cache"
        }

consentDiagnostic :: Text
consentDiagnostic = "attach the tag ecluse-dredger-consent=true to the mirror repository in CodeArtifact: the Dredger deletes nothing from a store that does not carry it"

recoveryFixture :: RecoveryFixture
recoveryFixture = RecoveryFixture "cargo/dredger/" siblingPackage refusedPackage readmittedPackage missingPackage latePackage "1.0.0"

rolloutFixture :: RolloutFixture
rolloutFixture = RolloutFixture "cargo/dredger/" deniedPackage keptPackage readmittedPackage missingPackage sentinelPackage firstPartyName "1.0.0"

advisoryFixture :: AdvisoryFixture
advisoryFixture = AdvisoryFixture "cargo/dredger/" advisoryPackage keptPackage "1.0.0" "1.2.0" (ScoredDenial dredgerAdvisoryId) "examined 2, deleted 1, kept 1, guard-skipped 0"

-- | Native backend guards complement every canonical Dredger family without a live AWS account.
cargoDredgerSafetyContract :: [DeclaredGroup]
cargoDredgerSafetyContract = [DeclaredGroup safetyGroup safetyCases]

safetyGroup :: Group
safetyGroup =
    Group
        { groupId = "cargo/dredger-native-safety"
        , groupContractId = "cargo/native-safety"
        , groupProfileId = "cargo/maintenance"
        , groupTier = E2E
        , groupPartition = "cargo"
        , groupCases = [Case ("cargo/dredger-native-safety/" <> suffix) ["cargo/dredger-native-safety"] | suffix <- ["deletion-cap", "unsafe-upstream", "native-yank-not-deletion", "authorisation"]]
        , groupCapabilities = capabilities
        , groupPrerequisites = prerequisites
        , groupResources = resources
        , groupIsolation = isolation
        }

safetyCases :: PhaseRecorder -> Spec
safetyCases recorder = aroundAll acquire $ do
    it (caseLabel "cargo/dredger-native-safety/deletion-cap" "stops physical native deletion at the configured logical version cap") $ \(plane, mirror, _) -> do
        publish (Native.directStore mirror) (packageName previewPackage) "2.0.0"
        previousVersions <- Native.storeVersions mirror (packageName previewPackage)
        previousVersions `shouldBe` ["1.0.0", "2.0.0"]
        requests <- codeArtifactDeleteRequests (Native.cspControl plane)
        run <- runNativeSweep plane [("ECLUSE_DREDGER__DELETION_CAP", "1")] Apply (IdentityPolicy [packageName previewPackage])
        sweepExit run `shouldNotBe` ExitSuccess
        sweepOutput run `shouldSatisfy` T.isInfixOf "reached its deletion cap of 1"
        remainingVersions <- Native.storeVersions mirror (packageName previewPackage)
        length remainingVersions `shouldBe` 1
        made <- drop (length requests) <$> codeArtifactDeleteRequests (Native.cspControl plane)
        length made `shouldBe` 1
    it (caseLabel "cargo/dredger-native-safety/unsafe-upstream" "refuses a refillable mirror target before any native deletion") $ \(plane, mirror, cache) -> do
        previousInventory <- (,) <$> Native.storeInventory mirror <*> Native.storeInventory cache
        requests <- codeArtifactDeleteRequests (Native.cspControl plane)
        run <- Native.withUnsafeMirror plane (runNativeSweep plane [] Apply (FullIdentity (packageName deniedPackage)))
        sweepExit run `shouldNotBe` ExitSuccess
        sweepOutput run `shouldSatisfy` T.isInfixOf "refills itself, so a delete changes nothing: the repository has an upstream (cache)"
        ((,) <$> Native.storeInventory mirror <*> Native.storeInventory cache) `shouldReturn` previousInventory
        codeArtifactDeleteRequests (Native.cspControl plane) `shouldReturn` requests
    it (caseLabel "cargo/dredger-native-safety/native-yank-not-deletion" "keeps yank separate from physical removal and observes CodeArtifact deleting the native archive") $ \(plane, mirror, _) -> do
        let name = packageName deniedPackage
            version = packageVersion deniedPackage
        Native.setStoredYank mirror name version
        storeArtifact storeOps mirror name version >>= (`shouldSatisfy` servedBytes)
        Native.storeVersions mirror name `shouldReturn` [version]
        requests <- codeArtifactDeleteRequests (Native.cspControl plane)
        run <- runNativeSweep plane [] Apply (IdentityPolicy [name])
        (sweepExit run, sweepOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        Native.storeVersions mirror name `shouldReturn` []
        (fst <$> Native.storeArtifactBytes mirror name version) `shouldReturn` 404
        made <- drop (length requests) <$> codeArtifactDeleteRequests (Native.cspControl plane)
        made `shouldBe` [("POST", "v1/package/versions/delete")]
    it (caseLabel "cargo/dredger-native-safety/authorisation" "reports CodeArtifact refusing the Dredger's identity and changes neither store") $ \(plane, mirror, cache) -> do
        let name = packageName keptPackage
        initialState <- (,) <$> Native.storeInventory mirror <*> Native.storeInventory cache
        Map.lookup name (fst initialState) `shouldBe` Just [packageVersion keptPackage]
        requests <- codeArtifactDeleteRequests (Native.cspControl plane)
        run <- runNativeSweep plane refusedAwsIdentity Apply (IdentityPolicy [name])
        sweepExit run `shouldBe` ExitFailure 2
        for_ (refusedIdentityReports Cargo) $ \report -> sweepOutput run `shouldSatisfy` T.isInfixOf report
        ((,) <$> Native.storeInventory mirror <*> Native.storeInventory cache) `shouldReturn` initialState
        codeArtifactDeleteRequests (Native.cspControl plane) `shouldReturn` requests
  where
    acquire action = timedFixture recorder (withFixture cargoDredgerFixture) (timedFixtureWith recorder withIdentity action)

runNativeSweep :: Native.CargoSweepPlane -> [(Text, Text)] -> SweepMode -> SweepPolicy -> IO SweepRun
runNativeSweep plane extra mode policy = do
    let arguments = if mode == Preview then ["--once", "--dry-run"] else ["--once"]
        run = observeRun <$> runRoleOnce (Native.cspGlobal plane) (Native.sweepRoleEnv <> policyEnvironment policy <> extra) ("dredger" : arguments)
    if mode == WithoutConsent then Native.withConsentWithheld plane run else run

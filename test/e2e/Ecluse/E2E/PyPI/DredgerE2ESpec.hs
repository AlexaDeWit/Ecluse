-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | PyPI operations for every canonical Dredger law against the running product image.
module Ecluse.E2E.PyPI.DredgerE2ESpec (
    pypiSweepDriver,
    pypiDredgerFixture,
    pypiDredgerProfile,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.Advisories (maliciousAdvisoryId)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.PyPI.Dredger
import Ecluse.E2E.Fixtures.PyPI.Mirror (Distribution (..))
import Ecluse.E2E.Harness.Advisories (advisoryStoreEnv, publishAdvisoryExport)
import Ecluse.E2E.Harness.Client (shouldSucceed)
import Ecluse.E2E.Harness.CodeArtifact (CodeArtifactFixture (cafRequests))
import Ecluse.E2E.Harness.Docker (RoleRun (..), awaitContainerLog, containerLogs, runRoleOnce, sharedMirrorQueue, withDataPlane, withMirrorRole)
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.E2E.Harness.Proxy (awaitProxyLog)
import Ecluse.E2E.Harness.PyPIDredger qualified as Native
import Ecluse.E2E.Harness.PyPIDredger.Client (pypiSweepClientOps)
import Ecluse.E2E.Harness.PyPIMirror (awaitTargetFiles)
import Ecluse.E2E.Harness.Sweep
import Ecluse.E2E.Harness.Types (GlobalDataPlane, PipProject, Topology (..))
import Ecluse.E2E.Harness.Verdaccio (awaitMirrorWindow)
import Ecluse.E2E.PyPI.MirrorEvidence (canonicalVersion, mirrorCompleted)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Sweep.Composition (SweepDriver (..))
import Ecluse.Test.Contract.Sweep.Types
import Ecluse.Test.Contract.Types
import Ecluse.Test.Osv (CorpusVersion (..))
import Ecluse.Test.Poll (pollUntil)

-- | Each canonical family owns a fresh pair of native stores and a remote state service.
pypiSweepDriver :: ConcreteFixture FamilyMetadata -> PhaseRecorder -> SweepDriver Native.PyPISweepPlane Native.PyPISweepStore PipProject
pypiSweepDriver fixture recorder =
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
        , sweepClientOps = pypiSweepClientOps
        , sweepGuardedOps = sweepOps [("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", firstPartyName)]
        , sweepPlainOps = sweepOps []
        , sweepAdvisoryOps = advisoryOps
        , sweepMaliciousOps = advisoryOps{publishNextGeneration = \plane -> observeRun <$> publishAdvisoryExport (Native.pspGlobal plane) PyPI dredgerMaliciousExportBase}
        , sweepRecoveryOps = recoveryOps
        , sweepRolloutOps = rolloutOps
        , sweepAdmissionOutage = Nothing
        }
  where
    acquire resource action = timedFixture recorder (withFixture fixture) (timedFixtureWith recorder resource action)

-- | Acquisition starts PyPI, the isolated TLS stub and the shared SQS/S3 emulator.
pypiDredgerFixture :: ConcreteFixture FamilyMetadata
pypiDredgerFixture =
    ConcreteFixture
        { withFixture = withDataPlane (Topology [PyPI] False True)
        , fixtureMetadata =
            FamilyMetadata
                "pypi/dredger"
                "pypi/maintenance"
                ["read", "mirror", "maintenance", "advisories"]
                ["docker", "image", "openssl", "tar", "python3", "pip"]
                ["proxy", "nginx", "codeartifact-fixture", "ministack"]
                ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }

-- | The profile names the deterministic CodeArtifact protocol fixture, never a cloud account.
pypiDredgerProfile :: Profile
pypiDredgerProfile = Profile "pypi/maintenance" ["pypi"] ["codeartifact-fixture", "sqs", "s3"] ["read", "mirror", "maintenance", "advisories"]

withIdentity :: ((Native.PyPISweepPlane, Native.PyPISweepStore, Native.PyPISweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withIdentity action global = Native.withPyPISweepPlane global $ \plane ->
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [deniedPackage, previewPackage, keptPackage]
        for_ ["1.0.0", "2.0.0"] $ \version -> publish (Native.directStore proxy) firstPartyName version
        action (plane, proxy, Native.cacheStore proxy)

withAdvisory :: ((Native.PyPISweepPlane, Native.PyPISweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisory action = withAdvisoryStores (\(plane, proxy, _) -> action (plane, proxy))

-- The first generation precedes boot because readiness waits for a successful advisory sync.
withAdvisoryStores :: ((Native.PyPISweepPlane, Native.PyPISweepStore, Native.PyPISweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStores action global = Native.withPyPISweepPlane global $ \plane -> do
    run <- compileGeneration plane CorpusV1
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    Native.withSweepProxy plane (advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules)]) Nothing [] (\proxy -> action (plane, proxy, Native.cacheStore proxy))

withRecovery :: ((Native.PyPISweepPlane, Native.PyPISweepStore, Native.PyPISweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRecovery action global = Native.withPyPISweepPlane global $ \plane -> do
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \seeder -> do
        seedMirror seeder recoveryPackages
        seedCache seeder recoveryPackages
    version <- canonicalVersion (recoveryDeniedVersion recoveryFixture)
    let denials = (packageName siblingPackage <> "@" <> version) : map packageName [refusedPackage, readmittedPackage, missingPackage, latePackage]
    Native.withSweepProxy plane [("ECLUSE_RULES", denyingRules denials)] Nothing [] $ \proxy ->
        action (plane, proxy, Native.cacheStore proxy)

withRollout :: ((Native.PyPISweepPlane, Native.PyPISweepStore, Native.PyPISweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRollout action global = Native.withPyPISweepPlane global $ \plane ->
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [readmittedPackage, missingPackage]
        seedCache proxy [deniedPackage, readmittedPackage, missingPackage]
        action (plane, proxy, Native.cacheStore proxy)

seedMirror :: Native.PyPISweepStore -> [PackageFixture] -> IO ()
seedMirror proxy packages = do
    for_ packages $ \package -> do
        for_ (packageVersions package) $ \version ->
            void (clientInstall pypiSweepClientOps proxy (packageName package) (Just version) >>= shouldSucceed)
        storeAwaitVersions storeOps proxy (packageName package) (sort (packageVersions package)) `shouldReturn` sort (packageVersions package)
    let plane = Native.pssPlane proxy
        expected = [distribution | distribution <- Native.pspDistributions plane, package <- packages, distributionProject distribution == packageName package, distributionVersion distribution `elem` packageVersions package]
    awaitTargetFiles (Native.pspMirror plane) expected `shouldReturn` True

seedCache :: Native.PyPISweepStore -> [PackageFixture] -> IO ()
seedCache proxy packages = for_ packages $ \package ->
    for_ (packageVersions package) (publish (Native.cacheStore proxy) (packageName package))

publish :: Native.PyPISweepStore -> Text -> Text -> IO ()
publish store name version = void (clientPublish pypiSweepClientOps store name version >>= shouldSucceed)

permissiveEnv :: [(Text, Text)]
permissiveEnv = [("ECLUSE_RULES", permissiveRules)]

storeOps :: StoreOps Native.PyPISweepStore
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

sweepOps :: [(Text, Text)] -> SweepOps Native.PyPISweepPlane
sweepOps extra =
    SweepOps
        { runSweep = \plane mode policy -> do
            let run = observeRun <$> runRoleOnce (Native.pspGlobal plane) (Native.sweepRoleEnv <> policyEnvironment policy <> extra) ("dredger" : arguments mode)
            if mode == WithoutConsent then Native.withConsentWithheld plane run else run
        , awaitWrites = awaitMirrorWindow
        }
  where
    arguments Preview = ["--once", "--dry-run"]
    arguments _ = ["--once"]

recoveryOps :: RecoveryOps Native.PyPISweepPlane Native.PyPISweepStore
recoveryOps =
    RecoveryOps
        { withRelaxedClient = \plane -> Native.withSweepProxy plane permissiveEnv Nothing []
        , withSourceWithheld = Native.withSourceWithheld
        , withDeletesRefused = Native.withDeleteRefusal
        , directPublisher = pure . Native.directStore
        , refusedDeleteTarget = "privateUpstream " <> Native.repositoryUrl "cache"
        , refusedDeleteDiagnostic = "the backend refused the delete, NOT_ALLOWED: fixture refuses cache deletion"
        }

rolloutOps :: RolloutOps Native.PyPISweepPlane Native.PyPISweepStore
rolloutOps =
    RolloutOps
        { rolloutQueue = sharedMirrorQueue . Native.pspGlobal
        , withServingRole = \plane queue policy ->
            Native.withSweepProxy
                plane
                (servingEnvironment policy <> [("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", Native.repositoryUrl "mirror")])
                (Just queue)
                ["proxy", "--no-worker"]
        , withWorkerRole = \plane queue -> withMirrorRole (Native.pspGlobal plane) (workerEnvironment queue)
        , awaitPublished = \worker release -> do
            (name, version) <- workerRelease release
            awaitContainerLog worker (mirrorCompleted name version) 240
        , workerLogs = containerLogs
        , publicationCoordinate = \release -> do
            (name, version) <- workerRelease release
            pure (name <> "@" <> version)
        , outsideSweeps = withoutDeletes
        }
  where
    servingEnvironment (IdentityPolicy names) = [("ECLUSE_RULES", denyingRules names)]
    servingEnvironment policy = policyEnvironment policy

workerRelease :: Text -> IO (Text, Text)
workerRelease release = case T.splitOn "@" release of
    [name, version] | not (T.null name) -> (name,) <$> canonicalVersion version
    _ -> fail "expected a PyPI package@version fixture coordinate"

workerEnvironment :: Text -> [(Text, Text)]
workerEnvironment queue =
    Native.proxyRoleEnv
        <> permissiveEnv
        <> [("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873"), ("ECLUSE_QUEUE__URL", queue), ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")]

-- | Fail the steps on any CodeArtifact delete, which only a Dredger may issue.
withoutDeletes :: Native.PyPISweepPlane -> IO a -> IO a
withoutDeletes plane steps = do
    prior <- deletes
    result <- steps
    deletes `shouldReturn` prior
    pure result
  where
    deletes = filter ((== "v1/package/versions/delete") . snd) <$> readTVarIO (cafRequests (Native.pspControl plane))

advisoryOps :: AdvisoryOps Native.PyPISweepPlane Native.PyPISweepStore
advisoryOps =
    AdvisoryOps
        { publishNextGeneration = \plane -> observeRun <$> compileGeneration plane CorpusV2
        , awaitGenerationSwap = \store -> awaitProxyLog (Native.pssProxy store) ((> 1) . length . T.breakOnAll "advisory database swapped in") 240
        }

compileGeneration :: Native.PyPISweepPlane -> CorpusVersion -> IO RoleRun
compileGeneration plane = publishAdvisoryExport (Native.pspGlobal plane) PyPI . dredgerExportBase

identityFixture :: IdentityFixture
identityFixture =
    IdentityFixture
        { identityCasePrefix = "pypi/dredger/"
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
        , identityConsentEvents = ["mirror sweep cycle halted: the pypi store on mirrorTarget " <> Native.repositoryUrl "mirror" <> " carries no deletion consent marker: " <> consentDiagnostic <> "; examined 0, deleted 0, kept 0, guard-skipped 0"]
        , identityMirrorDiagnostic = "pypi mirror store on mirrorTarget " <> Native.repositoryUrl "mirror"
        , identityCacheDiagnostic = "pypi private cache on privateUpstream " <> Native.repositoryUrl "cache"
        }

consentDiagnostic :: Text
consentDiagnostic = "attach the tag ecluse-dredger-consent=true to the mirror repository in CodeArtifact: the Dredger deletes nothing from a store that does not carry it"

recoveryFixture :: RecoveryFixture
recoveryFixture = RecoveryFixture "pypi/dredger/" siblingPackage refusedPackage readmittedPackage missingPackage latePackage "1.0.0"

rolloutFixture :: RolloutFixture
rolloutFixture = RolloutFixture "pypi/dredger/" deniedPackage keptPackage readmittedPackage missingPackage sentinelPackage firstPartyName "1.0.0"

advisoryFixture :: AdvisoryFixture
advisoryFixture = AdvisoryFixture "pypi/dredger/" advisoryPackage keptPackage "1.0.0" "1.2.0" (ScoredDenial dredgerAdvisoryId) "examined 2, deleted 1, kept 1, guard-skipped 0"

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm and Verdaccio providers for the maintenance business contracts.
module Ecluse.E2E.Npm.DredgerE2ESpec (npmSweepDriver) where

import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (close, execute_, open)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Poll (pollUntil)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO (bracket, bracket_)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.E2E.Fixtures.Advisories (maliciousAdvisoryId, maliciousExportBase)
import Ecluse.E2E.Fixtures.Npm (
    PkgSpec,
    corpusRevokedPkg,
    dredgerDryRunPkg,
    dredgerKeepPkg,
    dredgerPkg,
    mirrorPkg,
    psName,
    psVersion,
    psVersions,
    recoveryFaultPkg,
    recoveryLatePkg,
    recoveryLostPkg,
    recoveryPkg,
    recoveryReadmitPkg,
 )
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.E2E.Harness.Sweep
import Ecluse.Test.Contract.Sweep (listed)
import Ecluse.Test.Contract.Sweep.Composition (SweepDriver (..))
import Ecluse.Test.Contract.Sweep.Types
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2))

-- | Preserve five independent store lifetimes under the canonical Dredger composition.
npmSweepDriver :: ConcreteFixture FamilyMetadata -> PhaseRecorder -> SweepDriver GlobalDataPlane E2E NpmProject
npmSweepDriver fixture recorder =
    SweepDriver
        { withIdentitySweep = acquire withSeededStore
        , withAdvisorySweep = acquire withAdvisoryStore
        , withMaliciousSweep = acquire withAdvisoryStores
        , withRecoverySweep = acquire withRecoveryStores
        , withRolloutSweep = acquire withRolloutStores
        , sweepIdentityFixture = identityFixture
        , sweepAdvisoryFixture = advisoryFixture
        , sweepMaliciousFixture = maliciousFixture
        , sweepRecoveryFixture = recoveryFixture
        , sweepRolloutFixture = rolloutFixture
        , sweepStoreOps = storeOps
        , sweepClientOps = clientOps
        , sweepGuardedOps = guardedSweepOps
        , sweepPlainOps = sweepOps
        , sweepAdvisoryOps = advisoryOps
        , sweepMaliciousOps = maliciousAdvisoryOps
        , sweepRecoveryOps = recoveryOps
        , sweepRolloutOps = rolloutOps
        , sweepAdmissionOutage = Just outageDriver{withAdmissionOutage = acquire withOutageStores}
        }
  where
    acquire resource action = timedFixture recorder (withFixture fixture) (timedFixtureWith recorder resource action)

withSeededStore :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withSeededStore action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = publishTargetEnv} seed plane
  where
    seed e2e = do
        for_ mirroredPackages $ \pkg -> do
            verdaccioVersions e2e (psName pkg) `shouldReturn` []
            void $ npmInstall e2e (psName pkg) >>= shouldSucceed
            verdaccioHasVersion e2e (psName pkg) (psVersion pkg) `shouldReturn` True
        for_ firstPartyVersions $ \version -> do
            void $ withPublishProject e2e publishDredgerName version npmPublishIn >>= shouldSucceed
            verdaccioHasVersion e2e publishDredgerName version `shouldReturn` True
        listed storeOps e2e (Map.keys seededVersions)
        withDredgerPrivateCache plane e2e $ \cache -> action (plane, e2e, cache)

mirroredPackages :: [PkgSpec]
mirroredPackages = [dredgerPkg, dredgerKeepPkg, dredgerDryRunPkg]

seededVersions :: Map Text [Text]
seededVersions = Map.fromList ((publishDredgerName, firstPartyVersions) : [(psName pkg, [psVersion pkg]) | pkg <- mirroredPackages])

firstPartyVersions :: [Text]
firstPartyVersions = [publishVersion, "2.0.0"]

cacheOnlyVersion :: Text
cacheOnlyVersion = "3.0.0"

consentKey :: Text
consentKey = "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"

-- The first generation precedes boot because readiness waits for a successful advisory sync.
withAdvisoryStore :: ((GlobalDataPlane, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStore action = withAdvisoryStores (\(plane, proxy, _) -> action (plane, proxy))

withAdvisoryStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStores action plane = do
    createAdvisoryBucket plane
    compileGeneration plane CorpusV1
    withE2EWith defaultE2EConfig{ecExtraEnv = revocationProxyEnv} (\e2e -> withDredgerPrivateCache plane e2e (\cache -> action (plane, e2e, cache))) plane

revocationProxyEnv :: [(Text, Text)]
revocationProxyEnv = advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules), ("ECLUSE_CACHE__TTL", "1")]

compileGeneration :: GlobalDataPlane -> CorpusVersion -> IO ()
compileGeneration plane generation = do
    run <- publishAdvisoryGeneration plane generation
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)

swapMessage :: Text
swapMessage = "cve-sync[" <> show Npm <> "]: advisory database swapped in"

withRecoveryStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withRecoveryStores action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = recoveryProxyEnv} boot plane
  where
    boot proxy = withDredgerPrivateCache plane proxy $ \cache -> do
        seedMirrorCopies plane
        seedCacheCopies cache
        action (plane, proxy, cache)

-- Seed through a permissive proxy before the group uses its denying policy.
seedMirrorCopies :: GlobalDataPlane -> IO ()
seedMirrorCopies = withE2EWith defaultE2EConfig install
  where
    install seeder = do
        for_ recoveryCopies $ \(name, version) ->
            void $ npmInstall seeder (name <> "@" <> version) >>= shouldSucceed
        for_ recoveryPackages $ \pkg ->
            verdaccioAwaitVersions seeder (psName pkg) (sort (psVersions pkg)) `shouldReturn` sort (psVersions pkg)
        listed storeOps seeder (map psName recoveryPackages)

seedCacheCopies :: E2E -> IO ()
seedCacheCopies cache = do
    for_ recoveryCopies $ \(name, version) ->
        void $ withPublishProject cache name version npmPublishIn >>= shouldSucceed
    listed storeOps cache (map psName recoveryPackages)

recoveryCopies :: [(Text, Text)]
recoveryCopies = [(psName pkg, version) | pkg <- recoveryPackages, version <- sort (psVersions pkg)]

recoveryPackages :: [PkgSpec]
recoveryPackages = [recoveryPkg, recoveryFaultPkg, recoveryReadmitPkg, recoveryLostPkg, recoveryLatePkg]

deniedRecoveryVersion :: Text
deniedRecoveryVersion = "1.0.0"

recoveryProxyEnv :: [(Text, Text)]
recoveryProxyEnv = privateCacheEnv <> [("ECLUSE_RULES", recoveryRules)]

relaxedProxyEnv :: [(Text, Text)]
relaxedProxyEnv = privateCacheEnv <> [("ECLUSE_RULES", permissiveRules)]

privateCacheEnv :: [(Text, Text)]
privateCacheEnv =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl PrivateCache)
    , ("ECLUSE_CACHE__TTL", "1")
    ]

withRelaxedProxy :: GlobalDataPlane -> (E2E -> IO a) -> IO a
withRelaxedProxy plane action = withE2EWith defaultE2EConfig{ecExtraEnv = relaxedProxyEnv} action plane

recoveryRules :: Text
recoveryRules = denyingRules recoveryDenials

recoveryDenials :: [Text]
recoveryDenials =
    (psName recoveryPkg <> "@" <> deniedRecoveryVersion)
        : map psName [recoveryFaultPkg, recoveryReadmitPkg, recoveryLostPkg, recoveryLatePkg]

privateTarget :: Text
privateTarget = "privateUpstream " <> stubUrl PrivateCache

publishingDirectly :: E2E -> IO E2E
publishingDirectly e2e = do
    store <- packageStore e2e
    pure e2e{e2eClientEndpoints = Map.singleton Npm (serviceUrl store)}

withRolloutStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withRolloutStores action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = relaxedProxyEnv} boot plane
  where
    boot proxy = withDredgerPrivateCache plane proxy $ \cache -> do
        -- Fill the mirror before the private cache can answer without enqueueing.
        for_ looserPackages $ \pkg -> do
            void $ npmInstall proxy (psName pkg) >>= shouldSucceed
            verdaccioAwaitVersions proxy (psName pkg) [psVersion pkg] `shouldReturn` [psVersion pkg]
        for_ retainedCachePackages $ \pkg ->
            void $ withPublishProject cache (psName pkg) (psVersion pkg) npmPublishIn >>= shouldSucceed
        listed storeOps proxy (map psName looserPackages)
        listed storeOps cache (map psName retainedCachePackages)
        action (plane, proxy, cache)

looserPackages :: [PkgSpec]
looserPackages = [recoveryReadmitPkg, recoveryLostPkg]

retainedCachePackages :: [PkgSpec]
retainedCachePackages = dredgerPkg : looserPackages

withServeOnlyProxy :: GlobalDataPlane -> Text -> [(Text, Text)] -> (E2E -> IO a) -> IO a
withServeOnlyProxy plane queueUrl extraEnv action = withE2EWith config action plane
  where
    config =
        defaultE2EConfig
            { ecExtraEnv = extraEnv
            , ecQueueUrl = Just queueUrl
            , ecArgs = ["proxy", "--no-worker"]
            }

permissiveEnv :: [(Text, Text)]
permissiveEnv = [("ECLUSE_RULES", permissiveRules), ("ECLUSE_CACHE__TTL", "1")]

publishedLine :: Text -> Text
publishedLine published = "mirrored artifact published: " <> published

storeOps :: StoreOps E2E
storeOps =
    StoreOps
        { storeSnapshot = verdaccioSnapshot
        , storeListing = verdaccioListing
        , storeNamesUnder = verdaccioNamesUnder
        , storeVersions = verdaccioVersions
        , storeHasVersion = verdaccioHasVersion
        , storeAwaitVersions = verdaccioAwaitVersions
        , storeArtifact = verdaccioArtifact
        , storeArtifactBytes = verdaccioArtifactBytes
        , storeAwaitListed = verdaccioAwaitListed
        }

clientOps :: ClientOps E2E NpmProject
clientOps =
    ClientOps
        { clientInstall = \store name version -> npmInstall store (installCoordinate name version)
        , clientWithProject = withNpmProject
        , clientInstallIn = \project name version -> npmInstallIn project (installCoordinate name version)
        , clientInstalledVersion = installedVersion
        , clientPublish = \store name version -> withPublishProject store name version npmPublishIn
        , clientArtifact = \store name version -> proxyGet store (npmTarballPath name version)
        , clientMetadataStatus = \store name -> fst <$> proxyGet store ("/npm/" <> name)
        }

sweepOps :: SweepOps GlobalDataPlane
sweepOps = sweepOpsWith []

guardedSweepOps :: SweepOps GlobalDataPlane
guardedSweepOps = sweepOpsWith [("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)]

sweepOpsWith :: [(Text, Text)] -> SweepOps GlobalDataPlane
sweepOpsWith extra =
    SweepOps
        { runSweep = \plane mode policy -> observeRun <$> runDredgerOnce plane (arguments mode) (environment mode policy)
        , awaitWrites = awaitMirrorWindow
        }
  where
    arguments Preview = ["--once", "--dry-run"]
    arguments _ = ["--once"]
    environment mode policy = policyEnvironment policy <> extra <> [(consentKey, "false") | mode == WithoutConsent]

recoveryOps :: RecoveryOps GlobalDataPlane E2E
recoveryOps =
    RecoveryOps
        { withRelaxedClient = withRelaxedProxy
        , withSourceWithheld = withPublicArtifactWithheld
        , withDeletesRefused = withPrivateCacheDeletesRefused
        , directPublisher = publishingDirectly
        , refusedDeleteTarget = privateTarget
        , refusedDeleteDiagnostic = "the backend refused the delete, HTTP 503"
        }

rolloutOps :: RolloutOps GlobalDataPlane E2E
rolloutOps =
    RolloutOps
        { rolloutQueue = sharedMirrorQueue
        , withServingRole = \plane queue policy -> withServeOnlyProxy plane queue (servingEnvironment policy)
        , withWorkerRole = \plane queue -> withMirrorRole plane (mirrorRoleEnv queue permissiveRules)
        , awaitPublished = \worker release -> awaitContainerLog worker (T.isInfixOf (publishedLine release)) 240
        , workerLogs = containerLogs
        , publicationCoordinate = pure
        }
  where
    servingEnvironment PermissivePolicy = permissiveEnv <> publishTargetEnv
    servingEnvironment (IdentityPolicy names) = [("ECLUSE_RULES", denyingRules names), ("ECLUSE_CACHE__TTL", "1")]
    servingEnvironment policy = policyEnvironment policy <> [("ECLUSE_CACHE__TTL", "1")]

advisoryOps :: AdvisoryOps GlobalDataPlane E2E
advisoryOps =
    AdvisoryOps
        { publishNextGeneration = \plane -> observeRun <$> publishAdvisoryGeneration plane CorpusV2
        , awaitGenerationSwap = \proxy -> awaitProxyLog proxy ((> 1) . length . T.breakOnAll swapMessage) 240
        }

packageFixture :: PkgSpec -> PackageFixture
packageFixture pkg = PackageFixture (psName pkg) (psVersion pkg) (psVersions pkg)

identityFixture :: IdentityFixture
identityFixture =
    IdentityFixture
        { identityCasePrefix = "npm/dredger/"
        , identityDenied = packageFixture dredgerPkg
        , identityPreview = packageFixture dredgerDryRunPkg
        , identitySeeded = seededVersions
        , identityFirstPartyName = publishDredgerName
        , identityFirstPartyVersions = firstPartyVersions
        , identityPublishVersion = publishVersion
        , identityCacheOnlyVersion = cacheOnlyVersion
        , identityPreviewVersion = "2.0.0"
        , identityListingPrefix = "e"
        , identityAbsentPrefix = "z"
        , identityConsentDiagnostic = consentKey <> " is not set"
        , identityConsentEvents = []
        , identityMirrorDiagnostic = "npm mirror store on mirrorTarget " <> stubUrl Mirror
        , identityCacheDiagnostic = "npm private cache on " <> privateTarget
        }

recoveryFixture :: RecoveryFixture
recoveryFixture =
    RecoveryFixture
        { recoveryCasePrefix = "npm/dredger/"
        , recoverySibling = packageFixture recoveryPkg
        , recoveryRefused = packageFixture recoveryFaultPkg
        , recoveryReadmitted = packageFixture recoveryReadmitPkg
        , recoveryMissing = packageFixture recoveryLostPkg
        , recoveryLate = packageFixture recoveryLatePkg
        , recoveryDeniedVersion = deniedRecoveryVersion
        }

rolloutFixture :: RolloutFixture
rolloutFixture =
    RolloutFixture
        { rolloutCasePrefix = "npm/dredger/"
        , rolloutTarget = packageFixture dredgerPkg
        , rolloutUnaffected = packageFixture dredgerKeepPkg
        , rolloutRestored = packageFixture recoveryReadmitPkg
        , rolloutLost = packageFixture recoveryLostPkg
        , rolloutSentinel = packageFixture mirrorPkg
        , rolloutFirstPartyName = publishDredgerName
        , rolloutFirstPartyVersion = publishVersion
        }

advisoryFixture :: AdvisoryFixture
advisoryFixture =
    AdvisoryFixture
        { advisoryCasePrefix = "npm/dredger/"
        , advisoryRevoked = packageFixture corpusRevokedPkg
        , advisoryUnaffected = packageFixture dredgerKeepPkg
        , advisoryVulnerable = "1.0.0"
        , advisoryFixed = "1.2.0"
        , advisoryIdentifier = "GHSA-corpus-1002"
        }

installCoordinate :: Text -> Maybe Text -> Text
installCoordinate name = maybe name (\version -> name <> "@" <> version)

-- | The isolated host directory allows schema faults without changing the product image.
withOutageStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withOutageStores action plane = do
    createAdvisoryBucket plane
    compileGeneration plane CorpusV2
    withSystemTempDirectory "ecluse-advisory-outage" $ \directory ->
        withE2EWith
            defaultE2EConfig{ecExtraEnv = outageProxyEnv, ecAdvisoryDirectory = Just directory}
            (\proxy -> withDredgerPrivateCache plane proxy (\cache -> action (plane, proxy, cache)))
            plane

outageProxyEnv :: [(Text, Text)]
outageProxyEnv = advisoryStoreEnv <> privateCacheEnv <> [("ECLUSE_RULES", outageRules)]

outageRules :: Text
outageRules =
    renderRules
        [ "min-age" .= object ["type" .= ("AllowIfOlderThan" :: Text), "ageSeconds" .= (604800 :: Int)]
        , "deny-cve" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (8 :: Int), "onUnavailable" .= ("skip" :: Text)]
        , "deny-known-malicious" .= object ["onUnavailable" .= ("skip" :: Text)]
        ]

-- | DDL invalidates SQLite's cached schema and restores it even when an assertion fails.
withLookupFault :: E2E -> IO a -> IO a
withLookupFault proxy action = do
    directory <- maybe (fail "the outage fixture has no shared advisory directory") pure (e2eAdvisoryDirectory proxy)
    bracket (open (directory </> osvDbFileName "npm")) close $ \connection ->
        bracket_
            (execute_ connection "ALTER TABLE package_vulnerability_ranges RENAME TO outage_ranges")
            (execute_ connection "ALTER TABLE outage_ranges RENAME TO package_vulnerability_ranges")
            action

outageDriver :: AdmissionOutageDriver GlobalDataPlane E2E
outageDriver =
    AdmissionOutageDriver
        { withAdmissionOutage = withDataPlane dredgerTopology . withOutageStores
        , withLookupUnavailable = withLookupFault
        , awaitLookupRecovery = \proxy -> pollUntil 240 500000 id $ do
            void $ proxyGet proxy ("/npm/" <> psName dredgerKeepPkg)
            T.isInfixOf "advisory source outage recovered" <$> containerLogs (e2eProxyContainer proxy)
        , admissionEvidence = \proxy -> do
            awaitProxyLog proxy ((>= 2) . T.count "admitted with a check skipped for unavailability") 240 `shouldReturn` True
            containerLogs (e2eProxyContainer proxy)
        , runRecoveredSweep = \plane mode ->
            observeRun
                <$> runDredgerOnce
                    plane
                    ["--once"]
                    (advisoryStoreEnv <> privateCacheEnv <> sweepEnvUnder outageRules <> [(consentKey, "false") | mode == WithoutConsent])
        , outagePackage = PackageFixture (psName corpusRevokedPkg) "1.0.0" ["1.0.0"]
        , outageCasePrefix = "npm/dredger/"
        }

maliciousFixture :: AdvisoryFixture
maliciousFixture = advisoryFixture{advisoryIdentifier = maliciousAdvisoryId}

maliciousAdvisoryOps :: AdvisoryOps GlobalDataPlane E2E
maliciousAdvisoryOps =
    advisoryOps{publishNextGeneration = \plane -> observeRun <$> publishAdvisoryExport plane Npm maliciousExportBase}

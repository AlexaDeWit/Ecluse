-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Canonical maintenance scenarios use real Bundler, physical Ruby archives and native AWS fixture transport.
module Ecluse.E2E.RubyGems.DredgerE2ESpec (
    rubySweepDriver,
    rubyDredgerFixture,
    rubyDredgerProfile,
    rubyDredgerSafetyContract,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Server.Path (encodeComponent)
import Ecluse.E2E.Fixtures.Advisories (maliciousAdvisoryId)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness.Advisories (advisoryStoreEnv, publishAdvisoryExport)
import Ecluse.E2E.Harness.Client (shouldSucceed)
import Ecluse.E2E.Harness.CodeArtifact (CodeArtifactFixture (cafPort), codeArtifactDeleteRequests, creditCodeArtifactDeletes, withCodeArtifactDeleteAudit)
import Ecluse.E2E.Harness.Docker (RoleRun (..), awaitContainerLog, containerLogs, runRoleOnce, sharedMirrorQueue, withDataPlane, withMirrorRole)
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.E2E.Harness.Proxy (awaitProxyLog)
import Ecluse.E2E.Harness.RubyGems (RubyProject)
import Ecluse.E2E.Harness.RubyGemsCodeArtifact qualified as Native
import Ecluse.E2E.Harness.RubyGemsCodeArtifact.Client (rubySweepClientOps)
import Ecluse.E2E.Harness.RubyGemsCodeArtifact.Fixtures
import Ecluse.E2E.Harness.RubyGemsGateway (rubyProxy)
import Ecluse.E2E.Harness.RubyGemsRegistry (awaitRubyRegistry, removeRubyRegistryVersion, rubyRegistryFiles, seedRubyRegistry)
import Ecluse.E2E.Harness.Sweep
import Ecluse.E2E.Harness.Types (E2E (e2eManager), GlobalDataPlane, Topology (..))
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
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), rubyRelease)

-- | Each canonical family acquires a fresh native registry, remote state and independently observed backend.
rubySweepDriver :: ConcreteFixture FamilyMetadata -> PhaseRecorder -> SweepDriver Native.RubySweepPlane Native.RubySweepStore RubyProject
rubySweepDriver fixture recorder =
    SweepDriver
        { withIdentitySweep = acquire withIdentity
        , withAdvisorySweep = acquire withAdvisory
        , withMaliciousSweep = acquire withAdvisoryStores
        , withRecoverySweep = acquire withRecovery
        , withRolloutSweep = acquire withRollout
        , sweepIdentityFixture = identityFixture
        , sweepAdvisoryFixture = advisoryFixture
        , sweepMaliciousFixture = advisoryFixture{advisoryDenial = MaliciousDenial maliciousAdvisoryId, advisoryTally = "examined 4, deleted 2, kept 2, guard-skipped 0"}
        , sweepRecoveryFixture = RecoveryFixture "rubygems/dredger/" siblingPackage refusedPackage readmittedPackage missingPackage latePackage "1.0.0"
        , sweepRolloutFixture = RolloutFixture "rubygems/dredger/" deniedPackage keptPackage readmittedPackage missingPackage sentinelPackage firstPartyName "1.0.0"
        , sweepStoreOps = storeOps
        , sweepClientOps = rubySweepClientOps
        , sweepGuardedOps = sweepOps [("ECLUSE_MOUNTS__RUBYGEMS__FIRST_PARTY", firstPartyName)]
        , sweepPlainOps = sweepOps []
        , sweepAdvisoryOps = advisoryOps
        , sweepMaliciousOps = advisoryOps{publishNextGeneration = \plane -> observeRun <$> publishAdvisoryExport (Native.rspGlobal plane) RubyGems dredgerMaliciousExportBase}
        , sweepRecoveryOps = recoveryOps
        , sweepRolloutOps = rolloutOps
        , sweepAdmissionOutage = Nothing
        }
  where
    acquire resource action = timedFixture recorder (withFixture fixture) (timedFixtureWith recorder resource action)

-- | The fixture metadata names deterministic native backend doubles rather than AWS account access.
rubyDredgerFixture :: ConcreteFixture FamilyMetadata
rubyDredgerFixture =
    ConcreteFixture
        { withFixture = withDataPlane (Topology [RubyGems] False True)
        , fixtureMetadata = FamilyMetadata "rubygems/dredger" "rubygems/maintenance" capabilities prerequisites resources isolation
        }

-- | The native backend profile preserves exact receipt identities across every scenario family.
rubyDredgerProfile :: Profile
rubyDredgerProfile = Profile "rubygems/maintenance" ["rubygems"] ["codeartifact-fixture", "sqs", "s3"] capabilities

capabilities, prerequisites, resources, isolation :: [Text]
capabilities = ["read", "mirror", "maintenance", "advisories"]
prerequisites = ["docker", "image", "openssl", "ruby", "bundler-4.0.23"]
resources = ["proxy", "nginx", "codeartifact-fixture", "ministack"]
isolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]

withIdentity :: ((Native.RubySweepPlane, Native.RubySweepStore, Native.RubySweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withIdentity action global = Native.withRubySweepPlane global $ \plane -> do
    Native.selectSweepSource plane [deniedPackage, previewPackage{packageVersions = ["1.0.0", "2.0.0", "3.0.0"]}, keptPackage]
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [deniedPackage, previewPackage, keptPackage]
        -- Root dependency reads can populate the real cache before the identity fixture starts.
        cached <- rubyRegistryFiles (Native.rspRegistry plane) "rubygems-private"
        for_ cached $ \release -> removeRubyRegistryVersion (Native.rspRegistry plane) "rubygems-private" (releaseName release) (releaseVersion release)
        for_ ["1.0.0", "2.0.0"] (publish (Native.directStore proxy) firstPartyName)
        action (plane, proxy, Native.cacheStore proxy)

withAdvisory :: ((Native.RubySweepPlane, Native.RubySweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisory action = withAdvisoryStores (\(plane, proxy, _) -> action (plane, proxy))

withAdvisoryStores :: ((Native.RubySweepPlane, Native.RubySweepStore, Native.RubySweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStores action global = Native.withRubySweepPlane global $ \plane -> do
    Native.selectSweepSource plane [advisoryPackage, keptPackage]
    run <- compileGeneration plane CorpusV1
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    Native.withSweepProxy plane (advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules)]) Nothing [] $ \proxy -> action (plane, proxy, Native.cacheStore proxy)

withRecovery :: ((Native.RubySweepPlane, Native.RubySweepStore, Native.RubySweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRecovery action global = Native.withRubySweepPlane global $ \plane -> do
    Native.selectSweepSource plane recoveryPackages
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \seeder -> do
        seedMirror seeder recoveryPackages
        seedCache seeder recoveryPackages
    let denials = (packageName siblingPackage <> "@1.0.0") : map packageName [refusedPackage, readmittedPackage, missingPackage, latePackage]
    Native.withSweepProxy plane [("ECLUSE_RULES", denyingRules denials)] Nothing [] $ \proxy -> action (plane, proxy, Native.cacheStore proxy)

withRollout :: ((Native.RubySweepPlane, Native.RubySweepStore, Native.RubySweepStore) -> IO ()) -> GlobalDataPlane -> IO ()
withRollout action global = Native.withRubySweepPlane global $ \plane -> do
    Native.selectSweepSource plane [deniedPackage, keptPackage, readmittedPackage, missingPackage, sentinelPackage]
    Native.withSweepProxy plane permissiveEnv Nothing [] $ \proxy -> do
        seedMirror proxy [readmittedPackage, missingPackage]
        seedCache proxy [deniedPackage, readmittedPackage, missingPackage]
        Native.storeVersions proxy (packageName deniedPackage) `shouldReturn` []
        Native.storeVersions (Native.cacheStore proxy) (packageName deniedPackage) `shouldReturn` packageVersions deniedPackage
        action (plane, proxy, Native.cacheStore proxy)

seedMirror :: Native.RubySweepStore -> [PackageFixture] -> IO ()
seedMirror proxy packages = do
    for_ packages $ \package -> do
        for_ (packageVersions package) $ \version -> void (clientInstall rubySweepClientOps proxy (packageName package) (Just version) >>= shouldSucceed)
        storeAwaitVersions storeOps proxy (packageName package) (sort (packageVersions package)) `shouldReturn` sort (packageVersions package)
    let plane = Native.rssPlane proxy
        expected = [release | release <- Native.rspReleases plane, package <- packages, releaseName release == packageName package, releaseVersion release `elem` packageVersions package]
    awaitRubyRegistry (Native.rspRegistry plane) "rubygems-mirror" expected `shouldReturn` True

seedCache :: Native.RubySweepStore -> [PackageFixture] -> IO ()
seedCache proxy packages = do
    let plane = Native.rssPlane proxy
        selected release = any (\package -> releaseName release == packageName package && releaseVersion release `elem` packageVersions package) packages
    seedRubyRegistry (Native.rspRegistry plane) "rubygems-private" (filter selected (Native.rspReleases plane))
    for_ packages $ \package -> Native.storeVersions (Native.cacheStore proxy) (packageName package) `shouldReturn` sort (packageVersions package)

publish :: Native.RubySweepStore -> Text -> Text -> IO ()
publish store name version = void (clientPublish rubySweepClientOps store name version >>= shouldSucceed)

permissiveEnv :: [(Text, Text)]
permissiveEnv = [("ECLUSE_RULES", permissiveRules)]

storeOps :: StoreOps Native.RubySweepStore
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

sweepOps :: [(Text, Text)] -> SweepOps Native.RubySweepPlane
sweepOps extra = SweepOps{runSweep = (`runNativeSweep` extra), awaitWrites = awaitMirrorWindow}

recoveryOps :: RecoveryOps Native.RubySweepPlane Native.RubySweepStore
recoveryOps =
    RecoveryOps
        { withRelaxedClient = \plane -> Native.withSweepProxy plane permissiveEnv Nothing []
        , withSourceWithheld = Native.withSourceWithheld
        , withDeletesRefused = Native.withDeleteRefusal
        , directPublisher = pure . Native.directStore
        , refusedDeleteTarget = "privateUpstream " <> Native.repositoryUrl "cache"
        , refusedDeleteDiagnostic = "the backend refused the delete, NOT_ALLOWED: fixture refuses cache deletion"
        }

rolloutOps :: RolloutOps Native.RubySweepPlane Native.RubySweepStore
rolloutOps =
    RolloutOps
        { rolloutQueue = sharedMirrorQueue . Native.rspGlobal
        , withServingRole = \plane queue policy ->
            Native.withSweepProxy
                plane
                (servingEnvironment policy <> [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", Native.repositoryUrl "mirror")])
                (Just queue)
                ["proxy", "--no-worker"]
        , withWorkerRole = \plane queue action -> withCodeArtifactDeleteAudit (Native.rspControl plane) (Native.rspDredgerDeletes plane) $ withMirrorRole (Native.rspGlobal plane) (Native.proxyRoleEnv <> permissiveEnv <> [("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873"), ("ECLUSE_QUEUE__URL", queue), ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")]) action
        , awaitPublished = \worker coordinate -> awaitContainerLog worker (T.isInfixOf ("mirror version complete: " <> coordinate)) 240
        , workerLogs = containerLogs
        , publicationCoordinate = pure
        , creditSweep = \plane -> creditCodeArtifactDeletes (Native.rspControl plane) (Native.rspDredgerDeletes plane)
        }
  where
    servingEnvironment (IdentityPolicy names) = [("ECLUSE_RULES", denyingRules names)]
    servingEnvironment policy = policyEnvironment policy

advisoryOps :: AdvisoryOps Native.RubySweepPlane Native.RubySweepStore
advisoryOps =
    AdvisoryOps
        { publishNextGeneration = \plane -> observeRun <$> compileGeneration plane CorpusV2
        , awaitGenerationSwap = \store -> awaitProxyLog (rubyProxy (Native.rssObserved store)) ((> 1) . length . T.breakOnAll "advisory database swapped in") 240
        }

compileGeneration :: Native.RubySweepPlane -> CorpusVersion -> IO RoleRun
compileGeneration plane = publishAdvisoryExport (Native.rspGlobal plane) RubyGems . dredgerExportBase

identityFixture :: IdentityFixture
identityFixture =
    IdentityFixture
        { identityCasePrefix = "rubygems/dredger/"
        , identityDenied = deniedPackage
        , identityPreview = previewPackage
        , identitySeeded = Map.fromList ((firstPartyName, ["1.0.0", "2.0.0"]) : [(packageName package, [packageVersion package]) | package <- [deniedPackage, previewPackage, keptPackage]])
        , identityFirstPartyName = firstPartyName
        , identityFirstPartyVersions = ["1.0.0", "2.0.0"]
        , identityPublishVersion = "1.0.0"
        , identityCacheOnlyVersion = "3.0.0"
        , identityPreviewVersion = "2.0.0"
        , identityListingPrefix = "e"
        , identityAbsentPrefix = "z"
        , identityConsentDiagnostic = consentDiagnostic
        , identityConsentEvents = ["mirror sweep cycle halted: the rubygems store on mirrorTarget " <> Native.repositoryUrl "mirror" <> " carries no deletion consent marker: " <> consentDiagnostic <> "; examined 0, deleted 0, kept 0, guard-skipped 0"]
        , identityMirrorDiagnostic = "rubygems mirror store on mirrorTarget " <> Native.repositoryUrl "mirror"
        , identityCacheDiagnostic = "rubygems private cache on privateUpstream " <> Native.repositoryUrl "cache"
        }

consentDiagnostic :: Text
consentDiagnostic = "attach the tag ecluse-dredger-consent=true to the mirror repository in CodeArtifact: the Dredger deletes nothing from a store that does not carry it"

advisoryFixture :: AdvisoryFixture
advisoryFixture = AdvisoryFixture "rubygems/dredger/" advisoryPackage keptPackage "1.0.0" "2.0.0" (ScoredDenial dredgerAdvisoryId) "examined 2, deleted 1, kept 1, guard-skipped 0"

-- | Native backend observations supplement every canonical Dredger law without claiming live IAM coverage.
rubyDredgerSafetyContract :: [DeclaredGroup]
rubyDredgerSafetyContract = [DeclaredGroup safetyGroup safetyCases]

safetyGroup :: Group
safetyGroup =
    Group
        { groupId = "rubygems/dredger-native-safety"
        , groupContractId = "rubygems/native-safety"
        , groupProfileId = "rubygems/maintenance"
        , groupTier = E2E
        , groupPartition = "rubygems"
        , groupCases = [Case ("rubygems/dredger-native-safety/" <> suffix) ["rubygems/dredger-native-safety"] | suffix <- ["deletion-cap", "unsafe-upstream", "native-yank-not-deletion", "authorisation", "multi-platform-deletion"]]
        , groupCapabilities = capabilities
        , groupPrerequisites = prerequisites
        , groupResources = resources
        , groupIsolation = isolation
        }

safetyCases :: PhaseRecorder -> Spec
safetyCases recorder = aroundAll acquire $ do
    it (caseLabel "rubygems/dredger-native-safety/deletion-cap" "stops native physical deletion at the configured whole-version cap") $ \(plane, mirror, _) -> do
        publish (Native.directStore mirror) (packageName previewPackage) "2.0.0"
        initialState <- Native.storeVersions mirror (packageName previewPackage)
        initialState `shouldBe` ["1.0.0", "2.0.0"]
        requests <- codeArtifactDeleteRequests (Native.rspControl plane)
        run <- runNativeSweep plane [("ECLUSE_DREDGER__DELETION_CAP", "1")] Apply (IdentityPolicy [packageName previewPackage])
        sweepExit run `shouldNotBe` ExitSuccess
        sweepOutput run `shouldSatisfy` T.isInfixOf "reached its deletion cap of 1"
        remaining <- Native.storeVersions mirror (packageName previewPackage)
        length remaining `shouldBe` 1
        made <- drop (length requests) <$> codeArtifactDeleteRequests (Native.rspControl plane)
        made `shouldBe` [("POST", "v1/package/versions/delete")]
    it (caseLabel "rubygems/dredger-native-safety/unsafe-upstream" "refuses a refillable native mirror before any physical deletion") $ \(plane, mirror, cache) -> do
        initialState <- (,) <$> Native.storeInventory mirror <*> Native.storeInventory cache
        requests <- codeArtifactDeleteRequests (Native.rspControl plane)
        run <- Native.withUnsafeMirror plane (runNativeSweep plane [] Apply (FullIdentity (packageName deniedPackage)))
        sweepExit run `shouldNotBe` ExitSuccess
        T.toLower (sweepOutput run) `shouldSatisfy` T.isInfixOf "upstream"
        ((,) <$> Native.storeInventory mirror <*> Native.storeInventory cache) `shouldReturn` initialState
        codeArtifactDeleteRequests (Native.rspControl plane) `shouldReturn` requests
    it (caseLabel "rubygems/dredger-native-safety/native-yank-not-deletion" "distinguishes a native yank refusal from actual CodeArtifact archive removal") $ \(plane, mirror, _) -> do
        let name = packageName deniedPackage
            version = packageVersion deniedPackage
        initialState <- Native.storeArtifactBytes mirror name version
        second LBS.length initialState `shouldSatisfy` servedBytes
        fst <$> Native.storeRequest (Native.directStore mirror) ("api/v1/gems/yank?gem_name=" <> name <> "&version=" <> version) "DELETE" Nothing `shouldReturn` 405
        Native.storeArtifactBytes mirror name version `shouldReturn` initialState
        Native.storeVersions mirror name `shouldReturn` [version]
        requests <- codeArtifactDeleteRequests (Native.rspControl plane)
        run <- runNativeSweep plane [] Apply (IdentityPolicy [name])
        sweepExit run `shouldBe` ExitSuccess
        Native.storeVersions mirror name `shouldReturn` []
        (fst <$> Native.storeArtifactBytes mirror name version) `shouldReturn` 404
        made <- drop (length requests) <$> codeArtifactDeleteRequests (Native.rspControl plane)
        made `shouldBe` [("POST", "v1/package/versions/delete")]
    it (caseLabel "rubygems/dredger-native-safety/authorisation" "refuses unsigned control requests and incorrect native service credentials without mutation") $ \(plane, mirror, cache) -> do
        initialState <- (,) <$> Native.storeInventory mirror <*> Native.storeInventory cache
        release <- rubyRelease (Native.rspReleases plane) (packageName keptPackage) "1.0.0" "ruby"
        let base = "http://127.0.0.1:" <> show (cafPort (Native.rspControl plane))
            manager = e2eManager (rubyProxy (Native.rssObserved mirror))
        unauthorised manager (base <> "/ruby/mirror/info/" <> packageName keptPackage) "GET" "" `shouldReturn` 401
        unauthorised manager (base <> "/ruby/mirror/api/v1/gems") "POST" (releaseArchive release) `shouldReturn` 401
        unauthorised manager (base <> "/v1/package/versions/delete") "POST" "{}" `shouldReturn` 403
        ((,) <$> Native.storeInventory mirror <*> Native.storeInventory cache) `shouldReturn` initialState
    it (caseLabel "rubygems/dredger-native-safety/multi-platform-deletion" "physically removes every platform artifact in one denied native version") $ \(plane, mirror, _) -> do
        let name = packageName keptPackage
            version = packageVersion keptPackage
            variants = filter (\release -> releaseName release == name && releaseVersion release == version) (Native.rspReleases plane)
            fetch release = Native.storeRequest (Native.directStore mirror) ("gems/" <> encodeComponent (releaseFilename release)) "GET" Nothing
        length variants `shouldBe` 4
        for_ variants $ \release -> fetch release `shouldReturn` (200, LBS.fromStrict (releaseArchive release))
        requests <- codeArtifactDeleteRequests (Native.rspControl plane)
        run <- runNativeSweep plane [] Apply (IdentityPolicy [name])
        sweepExit run `shouldBe` ExitSuccess
        Native.storeVersions mirror name `shouldReturn` []
        for_ variants $ \release -> fst <$> fetch release `shouldReturn` 404
        made <- drop (length requests) <$> codeArtifactDeleteRequests (Native.rspControl plane)
        made `shouldBe` [("POST", "v1/package/versions/delete")]
  where
    acquire action = timedFixture recorder (withFixture rubyDredgerFixture) (timedFixtureWith recorder withIdentity action)

unauthorised :: Http.Manager -> Text -> ByteString -> ByteString -> IO Int
unauthorised manager url method body = do
    initial <- Http.parseRequest (toString url)
    response <- Http.httpLbs initial{Http.method = method, Http.requestBody = Http.RequestBodyBS body, Http.redirectCount = 0} manager
    pure (statusCode (Http.responseStatus response))

runNativeSweep :: Native.RubySweepPlane -> [(Text, Text)] -> SweepMode -> SweepPolicy -> IO SweepRun
runNativeSweep plane extra mode policy = do
    let arguments = if mode == Preview then ["--once", "--dry-run"] else ["--once"]
        run = observeRun <$> runRoleOnce (Native.rspGlobal plane) (Native.sweepRoleEnv <> policyEnvironment policy <> extra) ("dredger" : arguments)
    if mode == WithoutConsent then Native.withConsentWithheld plane run else run

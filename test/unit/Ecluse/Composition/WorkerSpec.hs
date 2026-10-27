-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exercise worker policies with their composed reads, credentials, and memory admission.
module Ecluse.Composition.WorkerSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Network.HTTP.Types.Status (status200, status429)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition (PublishTarget (ptEcosystem, ptMirrorUrl, ptUploadUrl), planMounts, planPublishTargets)
import Ecluse.Composition.MirrorRole (MirrorMintPlan (MintMirrorWrite))
import Ecluse.Composition.MirrorState (MirrorStateBinding (MirrorStateBinding))
import Ecluse.Composition.Support (expectConfig, expectProviders, expectValidated, fixedNow, overrideEnv, pypiMirrorEnv, scopedName, staticEnvVars, testLimits)
import Ecluse.Composition.Worker (mirrorTransportFor, workerPoliciesWithState)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Mirror.State (MirrorState (..), stateTarget)
import Ecluse.Core.Package (Artifact (artFilename, artUrl), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Admission (ArtifactAdmission (AdmissionDenied))
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails, vdRaw), VersionEvaluation (VersionCooling, VersionPresent))
import Ecluse.Core.Registry.Publish (MirrorPublish (mpProbeInventory), MirrorTransport (ptLimits))
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext))
import Ecluse.Core.Security (Limits (maxMetadataBytes, maxMirrorArtifactBytes, progressFloor), defaultLimits, mkProgressFloor)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (MeterSettings), meterSnapshot, newMemoryMeter)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache (StoreBudget (StoreBudget))
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps), PackumentDeps (pdFirstParty, pdLimits, pdMinIntegrity, pdUpstreams))
import Ecluse.Core.Server.MirrorView (newMirrorRetention, newMirrorView)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit), mountUpstreams)
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead
import Ecluse.Core.Worker (WorkerPolicy (wpArtifactLimits, wpFirstParty, wpMinIntegrity, wpMirrorState, wpNow, wpPublish, wpResolveVersion, wpRules))
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Runtime.Env (Env (envMemoryMeter, envPublicControl))
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Package (pypiVersion, requestsName, sampleDetails, thingName, unsafeFilename, v1_0_0)
import Ecluse.Test.Package.Admission (admitArtifactWithEvidence)
import Ecluse.Test.Registry.Npm (packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Public (PublicFixture (..), newPublicFixture)
import Ecluse.Test.Registry.PyPI (filesNamed, simpleIndexWith)
import Ecluse.Test.Rules (blockedBy, inertRuleDeps)
import Ecluse.Test.Stub (allCaptured, capMethod, capPath, headerValue, stubBaseUrl, withRoutedStubIO, withStub, withStubHeaders)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), expectRight)

spec :: Spec
spec = do
    pypiCapabilitiesSpec
    managedBudgetSpec
    policyInputsSpec

policyInputsSpec :: Spec
policyInputsSpec = describe "workerPoliciesWithState policy inputs" $ do
    it "reuses npm's selected release cache anonymously across worker jobs" $ do
        sourceBody <- newIORef "{}"
        withRoutedStubIO (const ((status200,[],) <$> readIORef sourceBody)) $ \source -> withSweeper $ \deadlines -> do
            fixture <- newPublicFixture
            (original, bindings, targets) <- composedFixtures deadlines
            let sourceUrl = loopbackRegistryUrl (stubBaseUrl source)
                env = original{envPublicControl = pfControl fixture}
                bind mount = mount{bindingPackumentDeps = deps{pdUpstreams = mountUpstreams [] Nothing sourceUrl (MirrorOnAdmit (loopbackRegistryUrl "http://mirror.test"))}}
                  where
                    deps = bindingPackumentDeps mount
                initial = stubBaseUrl source <> "/thing/-/thing-1.0.0.tgz"
                replacement = stubBaseUrl source <> "/changed/thing-1.0.0.tgz"
                document location = encode (packumentValue "thing" "1.0.0" [("1.0.0", versionValue (versionSpec "thing" "1.0.0" location))] ["1.0.0" .= ("2020-01-01T00:00:00Z" :: Text)] [])
            policy <- maybe (fail "missing composed npm policy") pure (Map.lookup Npm (workerPoliciesWithState env Map.empty (map bind bindings) targets testArtifactCap))
            let resolve =
                    wpResolveVersion policy (const pass) thingName v1_0_0 >>= \case
                        VersionPresent doc _ -> map artUrl (toList (pkgArtifacts (vdDetails doc))) `shouldBe` [initial]
                        other -> expectationFailure ("expected cached npm release, got " <> show other)
            writeIORef sourceBody (document initial)
            resolve
            writeIORef sourceBody (document replacement)
            resolve
            requests <- allCaptured source
            map capPath requests `shouldBe` ["/thing"]
            map (headerValue "Authorization") requests `shouldBe` [Nothing]
            pfRequests fixture `shouldReturn` [("127.0.0.1", PublicRead.WorkerMetadata, PublicRead.ResponseStatus 200)]

    it "builds one bundle per served mount, keyed by its ecosystem" $ withSweeper $ \deadlines -> do
        (env, bindings, targets) <- composedFixtures deadlines
        Map.keys (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) `shouldBe` [Npm]

    it "reuses the mount's own serve-side policy inputs on the bundle" $ withSweeper $ \deadlines -> do
        (env, bindings, targets) <- composedFixtures deadlines
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        case Map.lookup Npm (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                wpMinIntegrity policy `shouldBe` pdMinIntegrity deps
                now <- wpNow policy
                now `shouldBe` fixedNow

    it "carries the mount's own first-party predicate onto the bundle" $ withSweeper $ \deadlines -> do
        (env, bindings, targets) <- composedFixturesFrom deadlines firstPartyEnvVars testLimits
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        case Map.lookup Npm (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                map (wpFirstParty policy) names `shouldBe` [True, False, False]
                map (wpFirstParty policy) names `shouldBe` map (pdFirstParty deps) names

    it "carries a deny limited to admission onto the bundle, so ingest refuses what the gate refuses" $ withSweeper $ \deadlines -> do
        -- The pin below the deny would admit the version to a reader of the revocation rules alone.
        let rules = "{\"withdrawn\":{\"type\":\"DenyByIdentity\",\"identity\":\"thing\",\"appliesTo\":[\"admission\"]},\"pin\":{\"type\":\"AllowByIdentity\",\"identity\":\"thing\"}}"
        (env, bindings, targets) <- composedFixturesFrom deadlines (overrideEnv "ECLUSE_RULES" rules staticEnvVars) testLimits
        case Map.lookup Npm (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                now <- wpNow policy
                (admission, _) <- admitArtifactWithEvidence (EvalContext now Nothing) (wpRules policy) (wpMinIntegrity policy) (unsafeFilename "thing-1.0.0.tgz") (sampleDetails thingName v1_0_0)
                case admission of
                    AdmissionDenied decision -> blockedBy decision `shouldBe` Just "DenyByIdentity"
                    other -> expectationFailure ("expected the bundle's rules to refuse the version, got " <> show other)

    it "contributes no bundle for an ecosystem without a resolved publish target" $ withSweeper $ \deadlines -> do
        (env, bindings, _) <- composedFixtures deadlines
        Map.keys (workerPoliciesWithState env Map.empty bindings [] testArtifactCap) `shouldBe` []

    it "sizes the bundle's artifact fetch cap from the supplied plan value" $ withSweeper $ \deadlines -> do
        (env, bindings, targets) <- composedFixtures deadlines
        case Map.lookup Npm (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> maxMirrorArtifactBytes (wpArtifactLimits policy) `shouldBe` testArtifactCap

    it "reads the mirror presence probe under the mount's plan-resolved response bound, not the metadata-path default (issue #851)" $ withSweeper $ \deadlines -> do
        (env, bindings, targets) <- composedFixturesWith deadlines probeLimits
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        target <- case find ((== Npm) . ptEcosystem) targets of
            Just t -> pure t
            Nothing -> fail "expected an npm publish target"
        let transport = mirrorTransportFor env deps target
        ptLimits transport `shouldBe` pdLimits deps
        ptLimits transport `shouldNotBe` defaultLimits

    it "fetches a job's artifact under the mount's own progress floor, not the default one" $ withSweeper $ \deadlines -> do
        tightFloor <- either (fail . show) pure (mkProgressFloor 50 5 4096)
        (env, bindings, targets) <- composedFixturesWith deadlines defaultLimits{progressFloor = tightFloor}
        case Map.lookup Npm (workerPoliciesWithState env Map.empty bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                progressFloor (wpArtifactLimits policy) `shouldBe` tightFloor
                maxMirrorArtifactBytes (wpArtifactLimits policy) `shouldBe` testArtifactCap

pypiCapabilitiesSpec :: Spec
pypiCapabilitiesSpec = describe "composed PyPI worker capabilities" $ do
    it "counts worker metadata once and reuses its shared cooldown for another job" $
        withStubHeaders status429 [("Retry-After", "60")] "busy" $ \source -> withSweeper $ \deadlines -> do
            fixture <- newPublicFixture
            policy <- composedPyPIAtWith deadlines (\env -> env{envPublicControl = pfControl fixture}) (stubBaseUrl source) "http://localhost:1"
            replicateM_ 2 $
                wpResolveVersion policy (const pass) requestsName (pypiVersion "2.34.2") `shouldReturn` VersionCooling (RetryAfter 60)
            length <$> allCaptured source `shouldReturn` 1
            pfRequests fixture `shouldReturn` [("127.0.0.1", PublicRead.WorkerMetadata, PublicRead.ResponseStatus 429)]

    it "rereads every distribution and its source fields anonymously while authenticating target probes" $ do
        let initial = ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2.tar.gz"]
            added = initial <> ["requests-2.34.2-1-py3-none-any.whl"]
            document filenames = encode (simpleIndexWith "requests" [("project-status", object ["status" .= ("active" :: Text)])] (filesNamed filenames))
        sourceBody <- newIORef (document initial)
        withRoutedStubIO (const ((status200,[],) <$> readIORef sourceBody)) $ \source ->
            withStub status200 (document initial) $ \target -> withSweeper $ \deadlines -> do
                policy <- composedPyPIAt deadlines (stubBaseUrl source) (stubBaseUrl target)
                charges <- newIORef ([] :: [Int])
                let resolve expected = do
                        writeIORef charges []
                        wpResolveVersion policy (\bytes -> modifyIORef' charges (<> [bytes])) requestsName (pypiVersion "2.34.2") >>= \case
                            VersionPresent doc _ -> do
                                map artFilename (toList (pkgArtifacts (vdDetails doc))) `shouldBe` expected
                                raw <- maybe (fail "worker lost paired PyPI source") pure (vdRaw doc >>= snd pypiSimpleCached)
                                KeyMap.lookup "project-status" (simpleEnvelope raw) `shouldBe` Just (object ["status" .= ("active" :: Text)])
                                map snd (simpleFiles raw) `shouldBe` filesNamed expected
                            other -> expectationFailure ("expected complete worker source, got " <> show other)
                        readIORef charges >>= (`shouldSatisfy` any (> 0))
                resolve initial
                writeIORef sourceBody (document added)
                resolve added
                native <- mpProbeInventory (wpPublish policy) (const (pure ())) requestsName >>= expectRight
                fmap (map artFilename . toList . pkgArtifacts) (Map.lookup "2.34.2" (infoVersions native)) `shouldBe` Just initial
                publicRequests <- allCaptured source
                map capPath publicRequests `shouldBe` replicate 2 "/simple/requests/"
                map capMethod publicRequests `shouldBe` replicate 2 "GET"
                map (headerValue "Authorization") publicRequests `shouldBe` replicate 2 Nothing
                targetRequests <- allCaptured target
                map capPath targetRequests `shouldBe` ["/simple/requests/"]
                map capMethod targetRequests `shouldBe` ["GET"]
                map (headerValue "Authorization") targetRequests `shouldBe` [Just "Basic cHVibGlzaGVyOmZpeHR1cmUtdG9rZW4="]

managedBudgetSpec :: Spec
managedBudgetSpec = describe "composed managed worker budget" $ do
    it "charges overlapping regions separately and returns the ticket after success" $ withSweeper $ \deadlines -> do
        (env, binding) <- composedManagedBudget deadlines 1000
        let charged = snChargedBytes <$> meterSnapshot (envMemoryMeter env)
        wmsTarget binding `shouldBe` stateTarget "https://target.example.test/"
        wmsLimits binding `shouldBe` testLimits{maxMirrorArtifactBytes = testArtifactCap}
        wmsWithinBudget
            binding
            ( \budget -> do
                charged `shouldReturn` 1
                for_ (zip [1 ..] (budgetRegions budget)) $ \(position, region) -> do
                    region 10
                    charged `shouldReturn` position * 10
                    region 5
                    region 10
                    charged `shouldReturn` position * 10
                wsbState budget 20
                charged `shouldReturn` 80
                pure "completed"
            )
            `shouldReturn` Just ("completed" :: Text)
        charged `shouldReturn` 0
        wmsWithinBudget binding (const charged) `shouldReturn` Just 1
        charged `shouldReturn` 0

    it "returns the ticket when the managed action throws its typed failure" $ withSweeper $ \deadlines -> do
        (env, binding) <- composedManagedBudget deadlines 1000
        reached <- newIORef False
        let escaped = TestContractEscape "managed action escaped"
        wmsWithinBudget
            binding
            ( \budget -> do
                traverse_ ($ 10) (budgetRegions budget)
                snChargedBytes <$> meterSnapshot (envMemoryMeter env) `shouldReturn` 70
                writeIORef reached True
                throwIO escaped :: IO ()
            )
            `shouldThrow` (== escaped)
        readIORef reached `shouldReturn` True
        snChargedBytes <$> meterSnapshot (envMemoryMeter env) `shouldReturn` 0
        wmsWithinBudget binding (\_ -> pure ()) `shouldReturn` Just ()

    it "refuses entry without reaching the managed action when no ticket fits" $ withSweeper $ \deadlines -> do
        (env, binding) <- composedManagedBudget deadlines 0
        reached <- newIORef False
        wmsWithinBudget binding (\_ -> writeIORef reached True) `shouldReturn` Nothing
        readIORef reached `shouldReturn` False
        snChargedBytes <$> meterSnapshot (envMemoryMeter env) `shouldReturn` 0

composedPyPIAt :: Deadlines -> Text -> Text -> IO WorkerPolicy
composedPyPIAt deadlines = composedPyPIAtWith deadlines id

composedPyPIAtWith :: Deadlines -> (Env -> Env) -> Text -> Text -> IO WorkerPolicy
composedPyPIAtWith deadlines changeEnv source target = do
    (env, bindings, targets) <- composedFixturesFrom deadlines pypiMirrorEnv testLimits
    let bind mount = mount{bindingPackumentDeps = deps{pdUpstreams = mountUpstreams pypiArtifactHosts Nothing (loopbackRegistryUrl source) (MirrorOnAdmit (loopbackRegistryUrl target))}}
          where
            deps = bindingPackumentDeps mount
        retarget publish = publish{ptMirrorUrl = loopbackRegistryUrl target, ptUploadUrl = loopbackRegistryUrl (target <> "/legacy/")}
    maybe (fail "missing composed PyPI policy") pure $
        Map.lookup PyPI (workerPoliciesWithState (changeEnv env) Map.empty (map bind bindings) (map retarget targets) testArtifactCap)

composedManagedBudget :: Deadlines -> Int -> IO (Env, WorkerMirrorState)
composedManagedBudget deadlines allowance = do
    (original, bindings, targets) <- composedFixturesFrom deadlines pypiMirrorEnv testLimits
    meter <- newMemoryMeter deadlines (MeterSettings allowance 1 0 0)
    pool <- newLocalPool 1 1024
    retention <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let env = original{envMemoryMeter = meter}
        target = stateTarget "https://target.example.test/"
        unexpected :: IO a
        unexpected = throwIO (TestContractEscape "budget test reached durable storage")
        store = MirrorState (\_ _ _ -> unexpected) (\_ _ _ _ -> unexpected) (\_ _ -> unexpected) (\_ _ -> unexpected)
        view = newMirrorView testLimits target store retention
        states = Map.singleton PyPI (MirrorStateBinding store target view)
        policy = Map.lookup PyPI (workerPoliciesWithState env states bindings targets testArtifactCap)
    binding <- maybe (fail "missing composed managed PyPI policy") pure (policy >>= wpMirrorState)
    pure (env, binding)

budgetRegions :: WorkerStateBudget -> [Int -> IO ()]
budgetRegions budget =
    [ wsbState budget
    , wsbRecords budget
    , wsbCompanions budget
    , wsbArtifacts budget
    , wsbArchive budget
    , wsbSource budget
    , wsbTargets budget
    ]

firstPartyEnvVars :: [(String, String)]
firstPartyEnvVars = overrideEnv "ECLUSE_MOUNTS__NPM__FIRST_PARTY" "@acme" staticEnvVars

names :: [PackageName]
names = [scopedName "acme", scopedName "evil", thingName]

testArtifactCap :: Int
testArtifactCap = 40 * 1024 * 1024

probeLimits :: Limits
probeLimits = defaultLimits{maxMetadataBytes = 3 * 1024 * 1024}

composedFixtures :: Deadlines -> IO (Env, [MountBinding], [PublishTarget])
composedFixtures deadlines = composedFixturesWith deadlines testLimits

composedFixturesWith :: Deadlines -> Limits -> IO (Env, [MountBinding], [PublishTarget])
composedFixturesWith deadlines = composedFixturesFrom deadlines staticEnvVars

composedFixturesFrom :: Deadlines -> [(String, String)] -> Limits -> IO (Env, [MountBinding], [PublishTarget])
composedFixturesFrom deadlines envVars limits = do
    config <- expectConfig envVars Nothing
    providers <- expectProviders config
    plan <- expectValidated config
    bindings <-
        planMounts mountBindingFor (pure fixedNow) (const inertRuleDeps) MintMirrorWrite providers limits Nothing plan
            >>= either (\errs -> fail ("unexpected boot errors: " <> show errs)) pure
    targets <-
        either (\errs -> fail ("unexpected publish-target errors: " <> show errs)) pure (planPublishTargets MintMirrorWrite providers plan)
    env <- newTestEnv deadlines
    pure (env, bindings, targets)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native multi-platform mirror proofs survive lost replies, partial writes, restart and public-source loss.
module Ecluse.E2E.RubyGems.MirrorProofE2ESpec (rubyMirrorProofContract) where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_)
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO (finally)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.MirrorState (mirrorRetryAttempts, mutateMirrorStateObject)
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsGateway
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyConsumer)
import Ecluse.E2E.RubyGems.MirrorInstallE2ESpec
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.Types qualified as Contract
import Ecluse.Test.Registry.RubyGems.Fixtures
import Ecluse.Test.Registry.RubyGems.Reference (runRubyReference)

-- | Every fault starts with independent stores and durable state under the shared fixture lifetime.
rubyMirrorProofContract :: DeclaredGroup
rubyMirrorProofContract = DeclaredGroup metadata $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (label "source-roundtrip" "restores native rows, original compact specs and all platform archives after public outage") roundtrip
        it (label "lost-reply" "finishes the complete version after native uploads commit but lose their replies") lostReply
        it (label "interruption" "resumes a genuinely partial multi-platform write after process restart") interrupted
        it (label "upstream-removal" "keeps a completed version available after its public membership disappears") removed
        for_ [("missing-archive", MissingArchive), ("checksum-mutation", ChangedChecksum), ("missing-state", MissingState), ("corrupt-state", CorruptState), ("private-auth", PrivateAuthorisation)] $ \(name, fault) ->
            it (label name "refuses an incomplete or unauthorised completed-mirror view") (faultCase fault)
  where
    fixture = rubyWriteFixture "mirror-proof" ["read", "mirror", "client-install"]
    nativeMetadata = fixtureMetadata fixture
    metadata =
        Group
            "rubygems/mirror-proof"
            "rubygems/mirror-proof"
            "rubygems/write"
            Contract.E2E
            "rubygems"
            [Case ("rubygems/mirror-proof/" <> name) ["rubygems/mirror-proof"] | name <- ["source-roundtrip", "lost-reply", "interruption", "upstream-removal", "missing-archive", "checksum-mutation", "missing-state", "corrupt-state", "private-auth"]]
            (familyCapabilities nativeMetadata)
            (familyPrerequisites nativeMetadata)
            (familyResources nativeMetadata)
            (familyIsolation nativeMetadata)
    label name = caseLabel ("rubygems/mirror-proof/" <> name)

withPlatform :: GlobalDataPlane -> (RubyRegistry -> [RubyRelease] -> E2EConfig -> IO ()) -> IO ()
withPlatform plane action = withRubyMirrorFixture plane $ \registry releases -> do
    queue <- sharedMirrorQueue plane
    let platforms = filter ((== "ecluse-fixture-platform") . releaseName) releases
        config = rubyMirrorConfig{ecQueueUrl = Just queue}
    sort (map releasePlatform platforms) `shouldBe` ["arm64-darwin", "java", "ruby", "x86_64-linux-musl"]
    rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` []
    action registry platforms config

trigger :: E2E -> [RubyRelease] -> IO ()
trigger proxy releases = do
    release <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "ruby"
    proxyStatus proxy (rubyArchiveRoute release) `shouldReturn` 200

drain :: GlobalDataPlane -> RubyRegistry -> [RubyRelease] -> E2EConfig -> IO ()
drain plane registry releases config = withObservedRubyProxy config plane $ \observed -> do
    awaitRubyRegistry registry "rubygems-mirror" releases `shouldReturn` True
    queue <- maybe (fail "native mirror fixture has no durable queue") pure (ecQueueUrl config)
    awaitProxyLog (rubyProxy observed) (rubyMirrorComplete "ecluse-fixture-platform" "1.0.0") (mirrorRetryAttempts queue) `shouldReturn` True

complete :: GlobalDataPlane -> RubyRegistry -> [RubyRelease] -> E2EConfig -> IO ()
complete plane registry releases config = do
    withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> trigger (rubyProxy observed) releases
    drain plane registry releases config

roundtrip :: GlobalDataPlane -> IO ()
roundtrip plane = withPlatform plane $ \registry releases config -> do
    before <- withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
        trigger (rubyProxy observed) releases
        proxyGet (rubyProxy observed) dependencyPath
    fst before `shouldBe` 200
    drain plane registry releases config
    control <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "arm64-darwin"
    setRubyRegistryFault registry "rubygems-mirror" (RubyRegistryCompactBytes (releaseCompactPath control) "invalid target compact specification")
    let aged = config{ecArgs = ["proxy", "--no-worker"], ecExtraEnv = ecExtraEnv config <> [("ECLUSE_MOUNTS__RUBYGEMS__RULES", "{\"min-age\":{\"ageSeconds\":604800},\"remediation-fast-track\":{\"enabled\":false}}")]}
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedRubyProxy aged plane $ \observed -> do
            rubyPublicReachable (rubyProxy observed) `shouldReturn` False
            proxyGet (rubyProxy observed) dependencyPath `shouldReturn` before
            for_ releases $ \release -> proxyGet (rubyProxy observed) ("/rubygems/" <> toText (releaseCompactPath release)) `shouldReturn` (200, LBS.fromStrict (releaseCompact release))
            platformClient observed releases

lostReply :: GlobalDataPlane -> IO ()
lostReply plane = withPlatform plane $ \registry releases config -> do
    setRubyRegistryFault registry "rubygems-mirror" RubyRegistryLoseReply
    complete plane registry releases config
    uploads <- readTVarIO (rubyRegistryUploads registry)
    uploads `shouldSatisfy` (not . null)
    lost <- readTVarIO (rubyRegistryLostReplies registry)
    map fst lost `shouldBe` ["rubygems-mirror"]
    for_ lost $ \(_, filename) -> map releaseFilename releases `shouldSatisfy` elem filename
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $
            \observed -> platformClient observed releases

interrupted :: GlobalDataPlane -> IO ()
interrupted plane = withPlatform plane $ \registry releases config -> do
    setRubyRegistryFault registry "rubygems-mirror" RubyRegistryStopAfterOne
    preserved <- flip finally (releaseBlockedRubyUpload registry) $
        withObservedRubyProxy config plane $ \observed -> do
            trigger (rubyProxy observed) releases
            blocked <- awaitBlockedRubyUpload registry
            stored <- rubyRegistryFiles registry "rubygems-mirror"
            length stored `shouldBe` 1
            length stored `shouldSatisfy` (< length releases)
            map releaseFilename stored `shouldSatisfy` notElem blocked
            runProcess_ (proc "docker" ["kill", "--signal=KILL", e2eProxyContainer (rubyProxy observed)])
            pure stored
    rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` preserved
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed ->
            proxyStatus (rubyProxy observed) dependencyPath `shouldReturn` 503
    setRubyRegistryFault registry "rubygems-mirror" RubyRegistryHealthy
    drain plane registry releases config
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $
            \observed -> platformClient observed releases

removed :: GlobalDataPlane -> IO ()
removed plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    removeRubyRegistryVersion registry "rubygems-upstream" "ecluse-fixture-platform" "1.0.0"
    withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> platformClient observed releases

data MirrorFault = MissingArchive | ChangedChecksum | MissingState | CorruptState | PrivateAuthorisation

faultCase :: MirrorFault -> GlobalDataPlane -> IO ()
faultCase fault plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> platformClient observed releases
    release <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "ruby"
    case fault of
        MissingArchive -> setRubyRegistryFault registry "rubygems-mirror" (RubyRegistryMissingArchive (releaseFilename release))
        ChangedChecksum -> seedRubyRegistry registry "rubygems-mirror" [release{releaseInfoRow = T.replace (releaseChecksum release) (T.replicate 64 "0") (releaseInfoRow release)}]
        MissingState -> mutateState plane "DELETE" ""
        CorruptState -> mutateState plane "PUT" "{invalid retained RubyGems state"
        PrivateAuthorisation -> setRubyRegistryFault registry "rubygems-mirror" RubyRegistryRequireRead
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedRubyProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            let expected = case fault of PrivateAuthorisation -> 401; _ -> 503
            proxyStatus (rubyProxy observed) (if isMissing fault then rubyArchiveRoute release else dependencyPath) `shouldReturn` expected
            rubyConsumer (rubyProxy observed) currentBundler $ \project -> do
                rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
                void (rubyInstall project False >>= shouldFail)
            case fault of
                PrivateAuthorisation -> platformClientWith (Just "ruby:read") observed releases
                _ -> pass
  where
    isMissing MissingArchive = True
    isMissing _ = False

mutateState :: GlobalDataPlane -> ByteString -> ByteString -> IO ()
mutateState plane = mutateMirrorStateObject plane key
  where
    key = "rubygems-mirror/" <> projectKey (stateTarget (stubUrl RubyGemsMirror)) (mkPackageName RubyGems Nothing "ecluse-fixture-platform") <> "/project.json"

platformClient :: ObservedRubyProxy -> [RubyRelease] -> IO ()
platformClient = platformClientWith Nothing

platformClientWith :: Maybe Text -> ObservedRubyProxy -> [RubyRelease] -> IO ()
platformClientWith credentials observed releases = withRubyProject currentBundler (rubyProxy observed) (e2eBaseUrl (rubyProxy observed) <> "/rubygems/") credentials $ \project -> do
    rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
    before <- rubyTraffic observed
    void (runBundler project ["lock", "--add-platform", "ruby", "java", "x86_64-linux-musl", "arm64-darwin", "--add-checksums"] >>= shouldSucceed)
    void (runBundler project ["cache", "--all-platforms", "--no-install"] >>= shouldSucceed)
    for_ releases $ \release -> do
        bytes <- BS.readFile (rubyDirectory project </> "vendor/cache" </> toString (releaseFilename release))
        bytes `shouldBe` releaseArchive release
        native <- runRubyReference "archive" bytes
        coordinate <- either fail pure (parseEither (withObject "native archive" (\row -> (,,,) <$> row .: "name" <*> row .: "version" <*> row .: "platform" <*> row .: "sha256")) native)
        coordinate `shouldBe` (releaseName release, releaseVersion release, releasePlatform release, releaseChecksum release)
    void (rubyInstall project True >>= shouldSucceed)
    installed <- rubyEvaluate project "require 'ecluse_fixture_platform'; puts EcluseFixturePlatform::VALUE" >>= shouldSucceed
    T.strip (crStdout installed) `shouldSatisfy` T.isPrefixOf "ecluse-fixture-platform-1.0.0-"
    after <- rubyTraffic observed
    let archives = [T.takeWhileEnd (/= '/') (rubyRequestPath request) | request <- rubyClientRequests (rubyTrafficSince before after), ".gem" `T.isSuffixOf` rubyRequestPath request, rubyRequestStatus request == 200]
    for_ releases $ \release -> archives `shouldSatisfy` elem (releaseFilename release)

dependencyPath :: Text
dependencyPath = "/rubygems/api/v1/dependencies?gems=ecluse-fixture-platform"

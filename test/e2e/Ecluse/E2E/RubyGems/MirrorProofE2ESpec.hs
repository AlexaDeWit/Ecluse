-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native multi-platform mirror proofs survive lost replies, partial writes, restart and public-source loss.
module Ecluse.E2E.RubyGems.MirrorProofE2ESpec (rubyMirrorProofContract) where

import Data.Aeson (Value (Array, Null, String), withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import System.Process.Typed (proc, runProcess_)
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO (finally)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.MirrorState (mirrorRetryAttempts, mutateMirrorStateObject, readMirrorStateObject, updateMirrorStateObject)
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyConsumer)
import Ecluse.E2E.RubyGems.MirrorInstallE2ESpec
import Ecluse.E2E.RubyGems.NativeE2ESpec (assertRubyPlatformInstall)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.Types qualified as Contract
import Ecluse.Test.Json (changeAt, encodeStrict, fieldAt, textAtPath, withKeys)
import Ecluse.Test.Registry.RubyGems.Fixtures
import Ecluse.Test.Support (decodeJsonOrFail, expectRight)

-- | Every fault starts with independent stores and durable state under the shared fixture lifetime.
rubyMirrorProofContract :: DeclaredGroup
rubyMirrorProofContract = DeclaredGroup metadata $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (label "source-roundtrip" "restores native rows, original compact specs and all platform archives after public outage") roundtrip
        it (label "lost-reply" "finishes the complete version after native uploads commit but lose their replies") lostReply
        it (label "interruption" "resumes a genuinely partial multi-platform write after process restart") interrupted
        it (label "upstream-removal" "keeps a completed version available after its public membership disappears") removed
        it (label "conclusive-absence" "preserves authorised private reads when the project state is conclusively absent") absentProject
        it (label "missing-compact" "refuses the missing compact specification while preserving complete native installs") missingCompact
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
            [Case ("rubygems/mirror-proof/" <> name) ["rubygems/mirror-proof"] | name <- ["source-roundtrip", "lost-reply", "interruption", "upstream-removal", "conclusive-absence", "missing-archive", "checksum-mutation", "missing-state", "missing-compact", "corrupt-state", "private-auth"]]
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
drain plane registry releases config = withObservedNativeProxy config plane $ \observed -> do
    awaitRubyRegistry registry "rubygems-mirror" releases `shouldReturn` True
    queue <- maybe (fail "native mirror fixture has no durable queue") pure (ecQueueUrl config)
    awaitProxyLog (observedProxy observed) (rubyMirrorComplete "ecluse-fixture-platform" "1.0.0") (mirrorRetryAttempts queue) `shouldReturn` True

complete :: GlobalDataPlane -> RubyRegistry -> [RubyRelease] -> E2EConfig -> IO ()
complete plane registry releases config = do
    withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> trigger (observedProxy observed) releases
    drain plane registry releases config

roundtrip :: GlobalDataPlane -> IO ()
roundtrip plane = withPlatform plane $ \registry releases config -> do
    before <- withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
        trigger (observedProxy observed) releases
        proxyGet (observedProxy observed) infoPath
    fst before `shouldBe` 200
    drain plane registry releases config
    control <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "arm64-darwin"
    setRubyRegistryFault registry "rubygems-mirror" (RubyRegistryCompactBytes (releaseCompactPath control) "invalid target compact specification")
    let aged = config{ecArgs = ["proxy", "--no-worker"], ecExtraEnv = ecExtraEnv config <> [("ECLUSE_MOUNTS__RUBYGEMS__RULES", "{\"min-age\":{\"ageSeconds\":604800},\"remediation-fast-track\":{\"enabled\":false}}")]}
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy aged plane $ \observed -> do
            rubyPublicReachable (observedProxy observed) `shouldReturn` False
            proxyGet (observedProxy observed) infoPath `shouldReturn` before
            for_ releases $ \release -> proxyGet (observedProxy observed) ("/rubygems/" <> toText (releaseCompactPath release)) `shouldReturn` (200, LBS.fromStrict (releaseCompact release))
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
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $
            \observed -> platformClient observed releases

interrupted :: GlobalDataPlane -> IO ()
interrupted plane = withPlatform plane $ \registry releases config -> do
    setRubyRegistryFault registry "rubygems-mirror" RubyRegistryStopAfterOne
    preserved <- flip finally (releaseBlockedRubyUpload registry) $
        withObservedNativeProxy config plane $ \observed -> do
            trigger (observedProxy observed) releases
            blocked <- awaitBlockedRubyUpload registry
            stored <- rubyRegistryFiles registry "rubygems-mirror"
            length stored `shouldBe` 1
            length stored `shouldSatisfy` (< length releases)
            map releaseFilename stored `shouldSatisfy` notElem blocked
            runProcess_ (proc "docker" ["kill", "--signal=KILL", e2eProxyContainer (observedProxy observed)])
            pure stored
    rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` preserved
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed ->
            proxyStatus (observedProxy observed) infoPath `shouldReturn` 503
    setRubyRegistryFault registry "rubygems-mirror" RubyRegistryHealthy
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
        drain plane registry releases config
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $
            \observed -> platformClient observed releases

removed :: GlobalDataPlane -> IO ()
removed plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    removeRubyRegistryVersion registry "rubygems-upstream" "ecluse-fixture-platform" "1.0.0"
    withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> platformClient observed releases

absentProject :: GlobalDataPlane -> IO ()
absentProject plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    mutateState plane "DELETE" ""
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            rubyPublicReachable (observedProxy observed) `shouldReturn` False
            proxyStatus (observedProxy observed) infoPath `shouldReturn` 200
            platformClient observed releases

missingCompact :: GlobalDataPlane -> IO ()
missingCompact plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    before <- withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
        response <- proxyGet (observedProxy observed) infoPath
        fst response `shouldBe` 200
        pure response
    release <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "ruby"
    deleteCompactState plane release
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            let proxy = observedProxy observed
            rubyPublicReachable proxy `shouldReturn` False
            proxyStatus proxy ("/rubygems/" <> toText (releaseCompactPath release)) `shouldReturn` 503
            proxyGet proxy infoPath `shouldReturn` before
            for_ (filter ((/= releaseFilename release) . releaseFilename) releases) $ \sibling ->
                proxyGet proxy ("/rubygems/" <> toText (releaseCompactPath sibling)) `shouldReturn` (200, LBS.fromStrict (releaseCompact sibling))
            for_ releases $ \artifact ->
                proxyGet proxy (rubyArchiveRoute artifact) `shouldReturn` (200, LBS.fromStrict (releaseArchive artifact))
            platformClient observed releases

data MirrorFault = MissingArchive | ChangedChecksum | MissingState | CorruptState | PrivateAuthorisation

faultCase :: MirrorFault -> GlobalDataPlane -> IO ()
faultCase fault plane = withPlatform plane $ \registry releases config -> do
    complete plane registry releases config
    withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> platformClient observed releases
    release <- rubyRelease releases "ecluse-fixture-platform" "1.0.0" "ruby"
    case fault of
        MissingArchive -> setRubyRegistryFault registry "rubygems-mirror" (RubyRegistryMissingArchive (releaseFilename release))
        ChangedChecksum -> seedRubyRegistry registry "rubygems-mirror" [release{releaseInfoRow = T.replace (releaseChecksum release) (T.replicate 64 "0") (releaseInfoRow release)}]
        MissingState -> updateMirrorStateObject plane mirrorStateKey (pendingSourceState release)
        CorruptState -> mutateState plane "PUT" "{invalid retained RubyGems state"
        PrivateAuthorisation -> setRubyRegistryFault registry "rubygems-mirror" RubyRegistryRequireRead
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            let expected = case fault of
                    PrivateAuthorisation -> 401
                    _ -> 503
                path = case fault of
                    MissingArchive -> rubyArchiveRoute release
                    _ -> infoPath
            proxyStatus (observedProxy observed) path `shouldReturn` expected
            rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
                void (rubyInstall project False >>= shouldFail)
            case fault of
                PrivateAuthorisation -> platformClientWith (Just "ruby:read") observed releases
                _ -> pass

mutateState :: GlobalDataPlane -> ByteString -> ByteString -> IO ()
mutateState plane = mutateMirrorStateObject plane mirrorStateKey

mirrorStateKey :: Text
mirrorStateKey = "rubygems-mirror/" <> projectKey (stateTarget (stubUrl RubyGemsMirror)) (mkPackageName RubyGems Nothing "ecluse-fixture-platform") <> "/project.json"

pendingSourceState :: RubyRelease -> ByteString -> IO ByteString
pendingSourceState release bytes = do
    original <- decodeJsonOrFail bytes
    companion <- companionFor release original
    void (compactReference release companion)
    changed <- replaceCompanion release (withKeys [("retained", Null)] companion) original
    restored <- replaceCompanion release companion changed
    restored `shouldBe` original
    changed `shouldSatisfy` (/= original)
    let encoded = encodeStrict changed
    void (chargedDecodeProjectRecord (const pass) defaultLimits (stateTarget (stubUrl RubyGemsMirror)) (mkPackageName RubyGems Nothing (releaseName release)) encoded >>= expectRight)
    pure encoded

deleteCompactState :: GlobalDataPlane -> RubyRelease -> IO ()
deleteCompactState plane release = do
    original <- readMirrorStateObject plane mirrorStateKey >>= decodeJsonOrFail
    digest <- companionFor release original >>= compactReference release
    mutateMirrorStateObject plane ("rubygems-mirror/companions/sha256/" <> digest) "DELETE" ""
    readMirrorStateObject plane mirrorStateKey >>= decodeJsonOrFail >>= (`shouldBe` original)

companionFor :: RubyRelease -> Value -> IO Value
companionFor release value = do
    companions <- either fail pure $ parseEither (withObject "project state" rows) value
    case filter ((== Just (String (releaseFilename release))) . fieldAt "filename") companions of
        [companion] -> pure companion
        _ -> fail "expected one retained native companion for the selected archive"
  where
    rows fields = do
        versions <- fields .: "versions" :: Parser [Value]
        concat <$> traverse (withObject "version state" (.: "companions")) versions

compactReference :: RubyRelease -> Value -> IO Text
compactReference release companion = do
    textAtPath ["retained", "digest", "algorithm"] companion `shouldBe` Just "sha256"
    digest <- maybe (fail "native companion has no retained digest") pure (textAtPath ["retained", "digest", "value"] companion)
    digest `shouldBe` sha256Hex (releaseCompact release)
    pure digest

replaceCompanion :: RubyRelease -> Value -> Value -> IO Value
replaceCompanion release replacement = changeAt ["versions", "companions"] $ \case
    Array entries -> pure (Array (fmap replace entries))
    _ -> fail "expected companion state entries"
  where
    replace current
        | fieldAt "filename" current == Just (String (releaseFilename release)) = replacement
        | otherwise = current

platformClient :: ObservedNativeProxy -> [RubyRelease] -> IO ()
platformClient = platformClientWith Nothing

platformClientWith :: Maybe Text -> ObservedNativeProxy -> [RubyRelease] -> IO ()
platformClientWith credentials observed releases = withRubyProject currentBundler (observedProxy observed) (e2eBaseUrl (observedProxy observed) <> "/rubygems/") credentials $ \project -> do
    rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
    assertRubyPlatformInstall observed releases project

infoPath :: Text
infoPath = "/rubygems/info/ecluse-fixture-platform"

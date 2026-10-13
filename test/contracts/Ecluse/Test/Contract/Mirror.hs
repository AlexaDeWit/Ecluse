-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fresh public and mirror-only installations with independent origin and disk evidence.
module Ecluse.Test.Contract.Mirror (
    Observed (..),
    PublicPhase (..),
    OfflinePhase (..),
    MirrorDriver (..),
    MirrorFixture (..),
    mirrorContract,
    MirrorTagDriver (..),
    preservesLatest,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (ClientResult, TargetExpectation, TargetObservation, TargetRefusal, crossTargetInstallCase, shouldFail, shouldSucceed)
import Ecluse.Test.InstalledTree (InstalledTree, treeDifferences)

-- | Native observations recorded immediately after one isolated client invocation.
data Observed graph value = Observed
    { obGraph :: Either Text graph
    , obExport :: Either Text value
    , obExecutable :: Maybe (Either Text value)
    , obTree :: InstalledTree
    , obClient :: ClientResult
    , obLocalSources :: [FilePath]
    }

-- | Public-install evidence, including the complete destination inventory before and after.
data PublicPhase graph value = PublicPhase
    { publicObserved :: Observed graph value
    , publicTargetObserved :: TargetObservation graph
    , publicReachable :: Bool
    , publicVersionsBefore :: [(Text, [Text])]
    , publicVersionsAfter :: [(Text, [Text])]
    }

-- | Mirror-install evidence captured while the public source is unavailable.
data OfflinePhase graph value = OfflinePhase
    { offlineObserved :: Observed graph value
    , offlineTargetObserved :: TargetObservation graph
    , offlineUnsupportedTarget :: Maybe TargetRefusal
    , offlinePublicReachable :: Bool
    , offlineMirrorPaths :: Set Text
    , offlinePublicAnswers :: [Text]
    , offlineLockedSources :: [Text]
    }

-- | Each operation owns its fresh proxy and project, with the named public fault applied.
data MirrorDriver graph value = MirrorDriver
    { mirrorPublicInstall :: IO (PublicPhase graph value)
    , mirrorOfflineInstall :: IO (OfflinePhase graph value)
    , mirrorEmptyInstall :: IO (Bool, ClientResult)
    }

-- | Independent fixture expectations, including every required artifact and metadata path.
data MirrorFixture graph value = MirrorFixture
    { mirrorExpectedGraph :: graph
    , mirrorExpectedTargets :: Either Text (TargetExpectation, TargetExpectation)
    , mirrorExpectedExport :: value
    , mirrorExpectedVersions :: [(Text, [Text])]
    , mirrorRequiredPaths :: Set Text
    , mirrorExpectedSources :: Either Text [Text]
    }

-- | Compare complete native evidence and retain a no-private-store negative control.
mirrorContract :: (Eq graph, Show graph, Eq value, Show value) => Text -> MirrorFixture graph value -> SpecWith (MirrorDriver graph value)
mirrorContract prefix fixture = do
    aroundAllWith acquire $ do
        it (label "cross-target-install" "installs the declared target artifacts through the completed mirror") $ \(public, mirrored) -> do
            expected <- either (fail . toString) pure (mirrorExpectedTargets fixture)
            crossTargetInstallCase
                (mirrorExpectedGraph fixture)
                expected
                (publicTargetObserved public, offlineTargetObserved mirrored)
                (offlineUnsupportedTarget mirrored)
        it (label "dependency-graph" "resolves the same dependency graph") $ \(public, mirrored) -> do
            obGraph (publicObserved public) `shouldBe` Right (mirrorExpectedGraph fixture)
            obGraph (offlineObserved mirrored) `shouldBe` Right (mirrorExpectedGraph fixture)
        it (label "module-exports" "loads every package export") $ \(public, mirrored) -> do
            obExport (publicObserved public) `shouldBe` Right (mirrorExpectedExport fixture)
            obExport (offlineObserved mirrored) `shouldBe` Right (mirrorExpectedExport fixture)
        it (label "linked-executable" "links the executable with its expected output") $ \(public, mirrored) -> do
            obExecutable (publicObserved public) `shouldBe` Just (Right (mirrorExpectedExport fixture))
            obExecutable (offlineObserved mirrored) `shouldBe` Just (Right (mirrorExpectedExport fixture))
        it (label "origin-evidence" "reads every artifact and metadata document from the mirror during public outage") $ \(_, mirrored) -> do
            Set.toList (mirrorRequiredPaths fixture `Set.difference` offlineMirrorPaths mirrored) `shouldBe` []
            offlinePublicAnswers mirrored `shouldBe` []
            sources <- either (fail . toString) pure (mirrorExpectedSources fixture)
            offlineLockedSources mirrored `shouldMatchList` sources
        it (label "installed-tree" "leaves the same installed tree apart from the proxy address") $ \(public, mirrored) ->
            case treeDifferences ("from public", obTree (publicObserved public)) ("from the mirror", obTree (offlineObserved mirrored)) of
                [] -> pass
                differences -> expectationFailure (toString (T.unlines differences))
    it (label "empty-store-control" "fails without a private copy during public outage") $ \driver -> do
        (reachable, result) <- mirrorEmptyInstall driver
        reachable `shouldBe` False
        void (shouldFail result)
  where
    label name = caseLabel (prefix <> "/" <> name)
    acquire action driver = do
        public <- mirrorPublicInstall driver
        publicReachable public `shouldBe` True
        publicVersionsBefore public `shouldBe` [(name, []) | (name, _) <- mirrorExpectedVersions fixture]
        publicVersionsAfter public `shouldBe` mirrorExpectedVersions fixture
        checkInstall (publicObserved public)
        mirrored <- mirrorOfflineInstall driver
        offlinePublicReachable mirrored `shouldBe` False
        checkInstall (offlineObserved mirrored)
        action (public, mirrored)
    checkInstall observed = do
        obLocalSources observed `shouldBe` []
        void (shouldSucceed (obClient observed))

-- | Client requests and independent tag and version observations for one fixture package.
data MirrorTagDriver = MirrorTagDriver
    { tagInstallVersion :: Text -> IO ClientResult
    , tagInstallLatest :: IO (ClientResult, Maybe Text)
    , tagAwaitVersion :: Text -> IO [Text]
    , tagReadLatest :: IO (Maybe Text)
    , tagReadVersions :: IO [Text]
    }

-- | An older version completing later must not replace the upstream's latest tag.
preservesLatest :: Text -> Text -> MirrorTagDriver -> Expectation
preservesLatest newer older driver = do
    void (tagInstallVersion driver newer >>= shouldSucceed)
    firstVersions <- tagAwaitVersion driver newer
    (newer `elem` firstVersions) `shouldBe` True
    tagReadLatest driver `shouldReturn` Just newer
    void (tagInstallVersion driver older >>= shouldSucceed)
    secondVersions <- tagAwaitVersion driver older
    (older `elem` secondVersions) `shouldBe` True
    tagReadLatest driver `shouldReturn` Just newer
    tagReadVersions driver `shouldReturn` sort [older, newer]
    (result, installed) <- tagInstallLatest driver
    void (shouldSucceed result)
    installed `shouldBe` Just newer

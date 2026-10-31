-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Whole-version copying, physical interruption, and fresh pip installs through the running product.
module Ecluse.E2E.PyPI.MirrorInstallE2ESpec (pypiMirrorFamily, nativeMirrorContract) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Process.Typed (proc, runProcess_)
import Test.Hspec

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..), concreteFamily)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiProject)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.E2E.PyPI.MirrorEvidence
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Install (TargetRefusal (..))
import Ecluse.Test.Contract.Mirror
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Group)
import Ecluse.Test.Log (lineMessage)
import UnliftIO (finally)

-- | Native protocol and recovery obligations complement the full ecosystem laws.
nativeMirrorContract :: ConcreteFixture Group -> DeclaredGroup
nativeMirrorContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $
        describe "PyPI whole-version mirror installs" $ do
            it (caseLabel "pypi/mirror-native/read-roundtrip" "preserves every native file and supported field after restart") $ \plane ->
                withMirrorFixture plane $ \target distributions -> completeScenario plane target distributions AcceptUploads
            it (caseLabel "pypi/mirror-native/lost-reply" "recovers after a lost upload reply") $ \plane ->
                withMirrorFixture plane $ \target distributions -> completeScenario plane target distributions LoseNextReply
            it (caseLabel "pypi/mirror-native/interruption" "resumes an interrupted native copy") $ \plane ->
                withMirrorFixture plane $ \target distributions -> interruptedScenario plane target distributions
            it (caseLabel "pypi/mirror-native/yanked-greatest" "keeps a mirrored greatest release yanked, so an unpinned install selects the release below") $ \plane ->
                yankScenario plane SourceYanks
            it (caseLabel "pypi/mirror-native/yank-negative-control" "selects the same mirrored greatest release when the source does not yank it") $ \plane ->
                yankScenario plane SourceOffers

completeScenario :: GlobalDataPlane -> NativeTarget -> [Distribution] -> UploadFault -> IO ()
completeScenario plane target distributions fault = do
    queue <- sharedMirrorQueue plane
    let config = nativeMirrorConfig{ecQueueUrl = Just queue}
    control <- coldInstall plane config
    targetFiles target `shouldReturn` Map.empty
    setUploadFault target fault
    withE2EWith
        config
        ( \proxy -> do
            awaitWholeMirror proxy target distributions
            -- The worker must have seen the lost reply as one, and retried the job after it.
            when (fault == LoseNextReply) $
                awaitProxyLog proxy (any retriedAfterTransportFault . mapMaybe lineMessage . T.lines) 1 `shouldReturn` True
        )
        plane
    verifyHot plane target distributions control
  where
    retriedAfterTransportFault message =
        "leaving mirror job un-acked for retry" `T.isPrefixOf` message && "publish transport failure" `T.isInfixOf` message

interruptedScenario :: GlobalDataPlane -> NativeTarget -> [Distribution] -> IO ()
interruptedScenario plane target distributions = do
    queue <- sharedMirrorQueue plane
    let config = nativeMirrorConfig{ecQueueUrl = Just queue}
    control <- coldInstall plane config
    targetFiles target `shouldReturn` Map.empty
    setUploadFault target (HoldAfter 1)
    preserved <-
        flip finally (releaseBlockedUpload target) $
            withE2EWith
                config
                ( \proxy -> do
                    blocked <- awaitBlockedUpload target
                    files <- targetFiles target
                    Map.size files `shouldBe` 1
                    Map.member blocked files `shouldBe` False
                    readTVarIO (ntInFlight target) >>= (`shouldSatisfy` (> 0))
                    runProcess_ (proc "docker" ["kill", "--signal=KILL", e2eProxyContainer proxy])
                    pure files
                )
                plane
    targetFiles target `shouldReturn` preserved
    withE2EWith
        config{ecArgs = ["proxy", "--no-worker"]}
        ( \observer -> do
            sourceView observer `shouldReturn` fst control
            missing <-
                maybe
                    (fail "partial target has no missing distribution")
                    pure
                    (find (\distribution -> Map.notMember (distributionFilename distribution) preserved) distributions)
            entries <- listedFiles observer (distributionProject missing)
            verifyDistribution observer entries missing
            targetFiles target `shouldReturn` preserved
        )
        plane
    withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
        withE2EWith
            config{ecArgs = ["proxy", "--no-worker"]}
            (\proxy -> proxyStatus proxy ("/pypi/simple/" <> mirrorRoot) `shouldReturn` 503)
            plane
    uploaded <- Map.keys <$> targetFiles target
    attemptsBefore <- readTVarIO (ntAttempts target)
    setUploadFault target AcceptUploads
    withStubFault plane (PyPIMirrorBackend (ntPort target) False) $
        withE2EWith config (\proxy -> awaitWholeMirror proxy target distributions) plane
    attemptsAfter <- readTVarIO (ntAttempts target)
    for_ uploaded $ \filename ->
        length (filter (== filename) attemptsAfter) `shouldBe` length (filter (== filename) attemptsBefore)
    verifyHot plane target distributions control

coldInstall :: GlobalDataPlane -> E2EConfig -> IO ([(Text, Value, [(Text, LByteString)])], InstalledTree)
coldInstall plane config =
    withE2EWith
        config{ecArgs = ["proxy", "--no-worker"]}
        ( \proxy -> do
            coldView <- sourceView proxy
            tree <- installGraph proxy
            ((nativeArchitecture, nativePlatform), (_, otherPlatform)) <- either (fail . toString) pure mirrorArchitectures
            verifyPlatformInstallation proxy nativePlatform nativeArchitecture
            answeredRequests <- publicReads plane
            for_ [mirrorRoot, mirrorDependency] $ \project ->
                filter (requestedDistribution (mirrorWheelFile project otherPlatform)) answeredRequests `shouldBe` []
            counters <- publicWorkCounters proxy
            pwcRuleEvaluations counters `shouldSatisfy` (> 0)
            pwcMirrorEnqueues counters `shouldSatisfy` (> 0)
            pure (coldView, tree)
        )
        plane

installGraph :: E2E -> IO InstalledTree
installGraph proxy = do
    tree <- pipInstallGraph proxy mirrorRoot mirrorVersion
    ((architecture, _), _) <- either (fail . toString) pure mirrorArchitectures
    for_ [mirrorRoot, mirrorDependency] $ \project -> do
        Map.member (toString (pypiDistInfoFor project mirrorVersion) <> "/METADATA") tree `shouldBe` True
        Map.lookup (toString (T.replace "-" "_" project) <> "/architecture.txt") tree
            `shouldBe` Just (TreeFile False (encodeUtf8 (architecture <> "\n")))
    pure tree

verifyHot :: GlobalDataPlane -> NativeTarget -> [Distribution] -> ([(Text, Value, [(Text, LByteString)])], InstalledTree) -> IO ()
verifyHot plane target distributions (cold, tree) =
    withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
        withE2EWith
            nativeMirrorConfig
            ( \proxy -> do
                originStart <- length <$> stubReadsNow plane
                phaseStart <- length <$> publicReads plane
                countersBefore <- publicWorkCounters proxy
                hot <- sourceView proxy
                hot `shouldBe` cold
                installGraph proxy `shouldReturn` tree
                (_, (otherArchitecture, otherPlatform)) <- either (fail . toString) pure mirrorArchitectures
                verifyPlatformInstallation proxy otherPlatform otherArchitecture
                unsupported <- pipInstallForPlatform proxy mirrorRoot mirrorVersion "manylinux_2_17_riscv64"
                void (shouldFail (ptiClient unsupported))
                ptiTree unsupported `shouldBe` Map.empty
                publicAnswered <- drop phaseStart <$> publicReads plane
                filter answeredByPublic publicAnswered `shouldBe` []
                filter (not . publicProjectDiscovery) publicAnswered `shouldBe` []
                entries <- concat <$> traverse (listedFiles proxy) [mirrorRoot, mirrorDependency]
                readsBefore <- publicReads plane
                traverse_ (verifyDistribution proxy entries) distributions
                publicReads plane `shouldReturn` readsBefore
                verifyPrivateOrigins plane originStart distributions
                publicReads plane `shouldReturn` readsBefore
                publicWorkCounters proxy `shouldReturn` countersBefore
                phaseReads <- drop phaseStart <$> publicReads plane
                filter (not . publicProjectDiscovery) phaseReads `shouldBe` []
            )
            plane

-- | Whether the public source yanks the greatest release before the mirror copies it.
data SourceYank = SourceYanks | SourceOffers

-- The target serves no yank. With public down, a yank on the listed copy can come only from durable state.
yankScenario :: GlobalDataPlane -> SourceYank -> IO ()
yankScenario plane yank =
    withReleaseProject yankProject [yankKeptVersion, yankGreatestVersion] (maybe Map.empty (Map.singleton yankGreatestVersion) reason) plane $ \target distributions -> do
        let greatest = filter ((== yankGreatestVersion) . distributionVersion) distributions
            listed files = sortOn fst [(distributionFilename file, String <$> reason <* guard (distributionVersion file == yankGreatestVersion)) | file <- files]
        withE2EWith nativeMirrorConfig (pinGreatest target greatest) plane
        withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
            withE2EWith nativeMirrorConfig (\proxy -> listedYanks proxy yankProject `shouldReturn` listed greatest) plane
        withStubFault plane (PyPIMirrorBackend (ntPort target) False) $
            withE2EWith
                nativeMirrorConfig
                ( \proxy -> do
                    listedYanks proxy yankProject `shouldReturn` listed distributions
                    unpinned <- pipInstallRequirement proxy yankProject
                    installedPythonGraph unpinned `shouldBe` installedRelease (maybe yankGreatestVersion (const yankKeptVersion) reason)
                )
                plane
  where
    reason = case yank of
        SourceYanks -> Just yankReason
        SourceOffers -> Nothing

-- The pin admits the greatest release, and the worker copies its whole version to the target.
pinGreatest :: NativeTarget -> [Distribution] -> E2E -> IO ()
pinGreatest target greatest proxy = do
    pinnedInstall <- pipInstallGraph proxy yankProject yankGreatestVersion
    installedPythonGraph pinnedInstall `shouldBe` installedRelease yankGreatestVersion
    awaitProjectFiles target greatest yankProject `shouldReturn` True
    completed <- mirrorCompleted yankProject <$> canonicalVersion yankGreatestVersion
    awaitProxyLog proxy completed 120 `shouldReturn` True

installedRelease :: Text -> Either Text PythonGraph
installedRelease version = Right (Map.singleton yankProject (version, []))

-- | Supply real native installations and target observations to every shared mirror law.
pypiMirrorFamily :: ConcreteFixture FamilyMetadata -> (MirrorFixture PythonGraph Value, FamilyFixture (MirrorDriver PythonGraph Value))
pypiMirrorFamily fixture =
    ( MirrorFixture
        { mirrorExpectedGraph = mirrorGraph
        , mirrorExpectedExport = mirrorExport
        , mirrorExpectedVersions = [(project, [mirrorVersion]) | project <- projects]
        , mirrorRequiredPaths =
            Set.fromList
                ( ["/simple/" <> project <> "/" | project <- projects]
                    <> ["/simple/" <> project <> "/" <> filename | project <- projects, filename <- mirrorFileNames project]
                )
        , mirrorExpectedTargets = fmap (\((native, nativeFiles), (other, otherFiles)) -> (targetExpected native nativeFiles, targetExpected other otherFiles)) mirrorArchitectures
        , mirrorExpectedSources = fmap (\((_, platform), _) -> [redactedProxy <> "/pypi/simple/" <> project <> "/" <> mirrorWheelFile project platform | project <- projects]) mirrorArchitectures
        }
    , concreteFamily fixture $ \recorder action plane ->
        timedFixture recorder (withMirrorFixture plane . curry) $ \(target, distributions) -> do
            queue <- sharedMirrorQueue plane
            action (sharedMirrorDriver plane target distributions nativeMirrorConfig{ecQueueUrl = Just queue})
    )
  where
    projects = [mirrorRoot, mirrorDependency]

sharedMirrorDriver :: GlobalDataPlane -> NativeTarget -> [Distribution] -> E2EConfig -> MirrorDriver PythonGraph Value
sharedMirrorDriver plane target distributions config =
    MirrorDriver
        { mirrorPublicInstall = do
            versionsBefore <- observedVersions target distributions
            (observed, selected, reachable) <-
                withE2EWith
                    config{ecArgs = ["proxy", "--no-worker"]}
                    ( \proxy -> do
                        reachable <- publicProjectReachable proxy
                        (observed, _) <- pipObserveGraph proxy mirrorRoot mirrorVersion mirrorExecutable
                        ((native, platform), (_, opposite)) <- either (fail . toString) pure mirrorArchitectures
                        selected <- observeTarget proxy native platform
                        sourceReads <- publicReads plane
                        for_ [mirrorRoot, mirrorDependency] $ \project ->
                            filter (requestedDistribution (mirrorWheelFile project opposite)) sourceReads `shouldBe` []
                        counters <- publicWorkCounters proxy
                        pwcRuleEvaluations counters `shouldSatisfy` (> 0)
                        pwcMirrorEnqueues counters `shouldSatisfy` (> 0)
                        pure (observed, selected, reachable)
                    )
                    plane
            targetFiles target `shouldReturn` Map.empty
            withE2EWith config (\proxy -> awaitWholeMirror proxy target distributions) plane
            versionsAfter <- observedVersions target distributions
            pure (PublicPhase observed selected reachable versionsBefore versionsAfter)
        , mirrorOfflineInstall =
            withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
                withE2EWith nativeMirrorConfig (observeOffline plane distributions) plane
        , mirrorEmptyInstall = do
            stored <- targetFiles target
            atomically (writeTVar (ntFiles target) Map.empty)
            flip finally (atomically (writeTVar (ntFiles target) stored)) $
                withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
                    withE2EWith
                        config{ecArgs = ["proxy", "--no-worker"]}
                        ( \proxy -> do
                            reachable <- publicProjectReachable proxy
                            ((_, platform), _) <- either (fail . toString) pure mirrorArchitectures
                            attempted <- pipRefusedForPlatform proxy mirrorRoot mirrorVersion platform
                            ptiTree attempted `shouldBe` Map.empty
                            pure (reachable, ptiClient attempted)
                        )
                        plane
        }

observeOffline :: GlobalDataPlane -> [Distribution] -> E2E -> IO (OfflinePhase PythonGraph Value)
observeOffline plane distributions proxy = do
    reachable <- publicProjectReachable proxy
    start <- length <$> stubReadsNow plane
    counters <- publicWorkCounters proxy
    (observed, report) <- pipObserveGraph proxy mirrorRoot mirrorVersion mirrorExecutable
    (_, (architecture, platform)) <- either (fail . toString) pure mirrorArchitectures
    selected <- observeTarget proxy architecture platform
    refused <- pipInstallForPlatform proxy mirrorRoot mirrorVersion "manylinux_2_17_riscv64"
    entries <- concat <$> traverse (listedFiles proxy) [mirrorRoot, mirrorDependency]
    baseline <- publicReads plane
    traverse_ (verifyDistribution proxy entries) distributions
    verifyPrivateOrigins plane start distributions
    publicReads plane `shouldReturn` baseline
    publicWorkCounters proxy `shouldReturn` counters
    answered <- drop start <$> stubReadsNow plane
    sources <- either (fail . toString) pure (normaliseSources proxy report)
    pure
        OfflinePhase
            { offlineObserved = observed
            , offlineTargetObserved = selected
            , offlineUnsupportedTarget = Just (TargetRefusal (ptiClient refused) (ptiTree refused))
            , offlinePublicReachable = reachable
            , offlineMirrorPaths = Set.fromList [srPath request | request <- answered, srRoute request == PyPIMirror, srMethod request == "GET", srStatus request == 200]
            , offlinePublicAnswers = [show request | request <- answered, srRoute request == PyPIPublic, answeredByPublic request || not (publicProjectDiscovery request)]
            , offlineLockedSources = sources
            }

observedVersions :: NativeTarget -> [Distribution] -> IO [(Text, [Text])]
observedVersions target distributions = do
    files <- targetFiles target
    pure
        [ ( project
          , ordNub
                [ distributionVersion distribution
                | distribution <- distributions
                , distributionProject distribution == project
                , Map.member (distributionFilename distribution) files
                ]
          )
        | project <- [mirrorRoot, mirrorDependency]
        ]

publicProjectReachable :: E2E -> IO Bool
publicProjectReachable proxy = (== 200) <$> proxyStatus proxy ("/pypi/simple/" <> pypiProject)

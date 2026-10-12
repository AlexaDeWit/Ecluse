-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Whole-version copying, physical interruption, and fresh pip installs through the running product.
module Ecluse.E2E.PyPI.MirrorInstallE2ESpec (spec) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Process.Typed (proc, runProcess_)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import UnliftIO (finally)

-- | These cases run in the recurring image tier against native HTTP and emulated durable services.
spec :: Spec
spec = whenE2EAvailable . around withGlobalDataPlane $
    describe "PyPI whole-version mirror installs" $ do
        it "copies every wheel and sdist and preserves reads after public outage and proxy restart" $ \plane ->
            withMirrorFixture plane $ \target distributions -> completeScenario plane target distributions AcceptUploads
        it "recovers after a lost upload reply without changing installed bytes" $ \plane ->
            withMirrorFixture plane $ \target distributions -> completeScenario plane target distributions LoseNextReply
        it "keeps a partial target incomplete and resumes after the worker process is killed" $ \plane ->
            withMirrorFixture plane $ \target distributions -> interruptedScenario plane target distributions

completeScenario :: GlobalDataPlane -> NativeTarget -> [Distribution] -> UploadFault -> IO ()
completeScenario plane target distributions fault = do
    queue <- sharedMirrorQueue plane
    let config = nativeMirrorConfig{ecQueueUrl = Just queue}
    control <- coldInstall plane config
    targetFiles target `shouldReturn` Map.empty
    setUploadFault target fault
    withE2EWith config (\proxy -> awaitWholeMirror proxy target distributions) plane
    verifyHot plane target distributions control

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
        nativeMirrorConfig{ecArgs = ["proxy", "--no-worker"]}
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

publicReads :: GlobalDataPlane -> IO [StubRead]
publicReads plane = filter ((== PyPIPublic) . srRoute) <$> stubReadsNow plane

sourceView :: E2E -> IO [(Text, Value, [(Text, LByteString)])]
sourceView proxy = forM [mirrorRoot, mirrorDependency] $ \project -> do
    (status, bytes) <- proxyGet proxy ("/pypi/simple/" <> project)
    status `shouldBe` 200
    document <- either (fail . ("invalid Simple JSON: " <>)) pure (Aeson.eitherDecode bytes)
    companions <- forM (filter (T.isSuffixOf ".whl" . fst) (fileUrls document)) $ \(filename, url) -> do
        (companionStatus, companion) <- getUrl proxy (url <> ".metadata")
        companionStatus `shouldBe` 200
        pure (filename, companion)
    pure (project, normaliseUrls document, sortOn fst companions)

listedFiles :: E2E -> Text -> IO [(Text, Text)]
listedFiles proxy project = do
    (status, bytes) <- proxyGet proxy ("/pypi/simple/" <> project)
    status `shouldBe` 200
    document <- either fail pure (Aeson.eitherDecode bytes)
    pure (fileUrls document)

fileUrls :: Value -> [(Text, Text)]
fileUrls (Object document)
    | Just (Array entries) <- KeyMap.lookup "files" document =
        [(filename, url) | Object entry <- toList entries, Just (String filename) <- [KeyMap.lookup "filename" entry], Just (String url) <- [KeyMap.lookup "url" entry]]
fileUrls _ = []

normaliseUrls :: Value -> Value
normaliseUrls (Object document) = Object (KeyMap.mapWithKey (\key value -> if key == "files" then normaliseFiles value else value) document)
  where
    normaliseFiles (Array entries) = Aeson.toJSON (sortOn (\value -> show value :: Text) (map normaliseEntry (toList entries)))
    normaliseFiles value = value
    normaliseEntry (Object entry) = Object (KeyMap.insert "url" (String "recorded source URL") entry)
    normaliseEntry value = value
normaliseUrls value = value

requireFileUrl :: Text -> [(Text, Text)] -> IO Text
requireFileUrl filename entries = maybe (fail ("missing " <> toString filename)) pure (lookup filename entries)

getUrl :: E2E -> Text -> IO (Int, LByteString)
getUrl proxy url = case T.stripPrefix (e2eBaseUrl proxy) url of
    Just path -> proxyGet proxy path
    Nothing | "/" `T.isPrefixOf` url -> proxyGet proxy url
    Nothing -> fail ("proxy returned a foreign artifact URL: " <> toString url)

awaitWholeMirror :: E2E -> NativeTarget -> [Distribution] -> IO ()
awaitWholeMirror proxy target distributions = do
    awaitTargetFiles target distributions `shouldReturn` True
    for_ [mirrorRoot, mirrorDependency] $ \project ->
        awaitProxyLog proxy (T.isInfixOf ("mirror version complete: " <> project <> "@" <> canonicalVersion)) 120 `shouldReturn` True

canonicalVersion :: Text
canonicalVersion = renderVersion (mkVersion PyPI mirrorVersion)

verifyDistribution :: E2E -> [(Text, Text)] -> Distribution -> IO ()
verifyDistribution proxy entries distribution = do
    url <- requireFileUrl (distributionFilename distribution) entries
    (status, bytes) <- getUrl proxy url
    status `shouldBe` 200
    LBS.toStrict bytes `shouldBe` distributionBytes distribution
    path <- maybe (fail "artifact URL lacks the proxy origin") pure (T.stripPrefix (e2eBaseUrl proxy) url)
    (headStatus, headLength, headBytes) <- proxyHead proxy path
    headStatus `shouldBe` 200
    headLength `shouldBe` Just (fromIntegral (LBS.length bytes))
    headBytes `shouldBe` 0
    when (".whl" `T.isSuffixOf` distributionFilename distribution) $ do
        (companionStatus, companion) <- getUrl proxy (url <> ".metadata")
        companionStatus `shouldBe` 200
        LBS.toStrict companion `shouldBe` distributionMetadata distribution
        proxyHead proxy (path <> ".metadata") `shouldReturn` (200, Just (fromIntegral (LBS.length companion)), 0)

publicProjectDiscovery :: StubRead -> Bool
publicProjectDiscovery request =
    srMethod request == "GET"
        && srPath request `elem` ["/simple/" <> project <> suffix | project <- [mirrorRoot, mirrorDependency], suffix <- ["", "/"]]

verifyPlatformInstallation :: E2E -> Text -> Text -> IO ()
verifyPlatformInstallation proxy platform architecture = do
    installed <- pipInstallForPlatform proxy mirrorRoot mirrorVersion platform
    void (shouldSucceedThroughProxy proxy (ptiClient installed))
    sources <- either (fail . toString) pure (ptiSources installed)
    map fst sources `shouldMatchList` [mirrorRoot, mirrorDependency]
    for_ [mirrorRoot, mirrorDependency] $ \project -> do
        let marker = toString (T.replace "-" "_" project) <> "/architecture.txt"
        Map.lookup marker (ptiTree installed) `shouldBe` Just (TreeFile False (encodeUtf8 (architecture <> "\n")))
        source <- maybe (fail "pip omitted the installed dependency source") pure (lookup project sources)
        source `shouldSatisfy` T.isSuffixOf ("/" <> mirrorWheelFile project platform)

requestedDistribution :: Text -> StubRead -> Bool
requestedDistribution filename request =
    srMethod request == "GET"
        && ("/" <> filename) `T.isSuffixOf` T.takeWhile (/= '?') (srPath request)

verifyPrivateOrigins :: GlobalDataPlane -> Int -> [Distribution] -> IO ()
verifyPrivateOrigins plane start distributions = do
    answeredRequests <- awaitStubReads plane start complete
    complete answeredRequests `shouldBe` True
  where
    expected =
        [ (method, "/simple/" <> distributionProject distribution <> "/" <> distributionFilename distribution)
        | distribution <- distributions
        , method <- ["GET", "HEAD"]
        ]
    complete answeredRequests =
        all
            ( \(method, path) ->
                any
                    ( \request ->
                        srRoute request == PyPIMirror && srMethod request == method && srPath request == path && srStatus request == 200
                    )
                    answeredRequests
            )
            expected

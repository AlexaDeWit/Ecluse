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
import Ecluse.Test.Poll (pollUntil)

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
    setUploadFault target (RejectAfter 1)
    withE2EWith
        config
        ( \proxy -> do
            (Map.size <$> pollUntil 120 500000 ((== 1) . Map.size) (targetFiles target)) `shouldReturn` 1
            runProcess_ (proc "docker" ["kill", "--signal=KILL", e2eProxyContainer proxy])
        )
        plane
    withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
        withE2EWith
            config{ecArgs = ["proxy", "--no-worker"]}
            (\proxy -> proxyStatus proxy ("/pypi/simple/" <> mirrorRoot) `shouldReturn` 503)
            plane
    uploaded <- Map.keys <$> targetFiles target
    setUploadFault target AcceptUploads
    withStubFault plane (PyPIMirrorBackend (ntPort target) False) $
        withE2EWith config (\proxy -> awaitWholeMirror proxy target distributions) plane
    uploads <- readTVarIO (ntUploads target)
    for_ uploaded $ \filename -> length (filter ((== filename) . fst) uploads) `shouldBe` 1
    verifyHot plane target distributions control

coldInstall :: GlobalDataPlane -> E2EConfig -> IO ([(Text, Value, [(Text, LByteString)])], InstalledTree)
coldInstall plane config =
    withE2EWith
        config{ecArgs = ["proxy", "--no-worker"]}
        ( \proxy -> do
            coldView <- sourceView proxy
            tree <- installGraph proxy
            pure (coldView, tree)
        )
        plane

installGraph :: E2E -> IO InstalledTree
installGraph proxy = do
    tree <- pipInstallGraph proxy mirrorRoot mirrorVersion
    for_ [mirrorRoot, mirrorDependency] $ \project ->
        Map.member (toString (pypiDistInfoFor project mirrorVersion) <> "/METADATA") tree `shouldBe` True
    pure tree

verifyHot :: GlobalDataPlane -> NativeTarget -> [Distribution] -> ([(Text, Value, [(Text, LByteString)])], InstalledTree) -> IO ()
verifyHot plane target distributions (cold, tree) =
    withStubFault plane (PyPIMirrorBackend (ntPort target) True) $
        withE2EWith
            nativeMirrorConfig{ecCollector = True, ecExtraEnv = ecExtraEnv nativeMirrorConfig <> otlpCollectorEnv}
            ( \proxy -> do
                phaseStart <- length <$> publicReads plane
                hot <- sourceView proxy
                hot `shouldBe` cold
                installGraph proxy `shouldReturn` tree
                downloaded <- pipDownloadPlatform proxy mirrorRoot mirrorVersion
                downloaded `shouldBe` [(toString (distributionFilename d), distributionBytes d) | d <- distributions, distributionProject d == mirrorRoot, "win_amd64.whl" `T.isSuffixOf` distributionFilename d]
                publicAnswered <- drop phaseStart <$> publicReads plane
                filter answeredByPublic publicAnswered `shouldBe` []
                entries <- concat <$> traverse (listedFiles proxy) [mirrorRoot, mirrorDependency]
                readsBefore <- publicReads plane
                traverse_ (verifyDistribution proxy entries) distributions
                publicReads plane `shouldReturn` readsBefore
                verifyNoAdmissions proxy
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
    (headStatus, _, headBytes) <- proxyHead proxy path
    headStatus `shouldBe` 200
    headBytes `shouldBe` 0
    when (".whl" `T.isSuffixOf` distributionFilename distribution) $ do
        (companionStatus, companion) <- getUrl proxy (url <> ".metadata")
        companionStatus `shouldBe` 200
        LBS.toStrict companion `shouldBe` distributionMetadata distribution

verifyNoAdmissions :: E2E -> IO ()
verifyNoAdmissions proxy = do
    let finalPath = "/simple/" <> mirrorDependency <> "/" <> mirrorDependency <> "-" <> mirrorVersion <> ".zip"
        targetAnswers = privateArtifactAnswers "pypi-mirror" finalPath
    spans <- awaitCollectorSpans proxy (elem (IntValue 200) . targetAnswers) 80
    targetAnswers spans `shouldContain` [IntValue 200]
    for_ [mirrorRoot, mirrorDependency] $ \project ->
        for_ ["ecluse.rule.eval", "ecluse.mirror.enqueue"] $ \name ->
            filter (spanFor project canonicalVersion name) spans `shouldBe` []

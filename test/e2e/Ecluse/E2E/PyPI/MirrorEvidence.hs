-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native PyPI read, target-selection, completion, and origin evidence for contract drivers.
module Ecluse.E2E.PyPI.MirrorEvidence (
    publicReads,
    sourceView,
    listedFiles,
    requireFileUrl,
    getUrl,
    awaitWholeMirror,
    canonicalVersion,
    verifyDistribution,
    publicProjectDiscovery,
    verifyPlatformInstallation,
    requestedDistribution,
    verifyPrivateOrigins,
    targetExpected,
    observeTarget,
    normaliseSources,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.Test.Contract.Install
import Ecluse.Test.Contract.Serving qualified as Serving
import Test.Hspec

-- | Public origin records, including failed requests.
publicReads :: GlobalDataPlane -> IO [StubRead]
publicReads plane = filter ((== PyPIPublic) . srRoute) <$> stubReadsNow plane

-- | Capture supported listing fields and the exact declared companion bytes.
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

-- | Read the proxy listing independently from the worker inventory.
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

-- | Require the declared filename in the observed listing.
requireFileUrl :: Text -> [(Text, Text)] -> IO Text
requireFileUrl filename entries = maybe (fail ("missing " <> toString filename)) pure (lookup filename entries)

-- | Read only a URL served through this proxy instance.
getUrl :: E2E -> Text -> IO (Int, LByteString)
getUrl proxy url = case T.stripPrefix (e2eBaseUrl proxy) url of
    Just path -> proxyGet proxy path
    Nothing | "/" `T.isPrefixOf` url -> proxyGet proxy url
    Nothing -> fail ("proxy returned a foreign artifact URL: " <> toString url)

-- | Wait for physical completeness and the final durable worker proof.
awaitWholeMirror :: E2E -> NativeTarget -> [Distribution] -> IO ()
awaitWholeMirror proxy target distributions = do
    awaitTargetFiles target distributions `shouldReturn` True
    for_ [mirrorRoot, mirrorDependency] $ \project ->
        awaitProxyLog proxy (T.isInfixOf ("mirror version complete: " <> project <> "@" <> canonicalVersion)) 120 `shouldReturn` True

-- | The version spelling used by worker logs and domain spans.
canonicalVersion :: Text
canonicalVersion = renderVersion (mkVersion PyPI mirrorVersion)

-- | Require exact distribution bytes and complete body-free HEAD responses.
verifyDistribution :: E2E -> [(Text, Text)] -> Distribution -> IO ()
verifyDistribution proxy entries distribution = do
    url <- requireFileUrl (distributionFilename distribution) entries
    (status, bytes) <- getUrl proxy url
    status `shouldBe` 200
    LBS.toStrict bytes `shouldBe` distributionBytes distribution
    path <- maybe (fail "artifact URL lacks the proxy origin") pure (T.stripPrefix (e2eBaseUrl proxy) url)
    headResponse <- proxyHead proxy path
    Serving.artifactHeadCase headResponse
    lookup "Content-Length" (Serving.responseHeaders headResponse) `shouldBe` Just (encodeUtf8 (show (LBS.length bytes) :: Text))
    when (".whl" `T.isSuffixOf` distributionFilename distribution) $ do
        (companionStatus, companion) <- getUrl proxy (url <> ".metadata")
        companionStatus `shouldBe` 200
        LBS.toStrict companion `shouldBe` distributionMetadata distribution
        companionHead <- proxyHead proxy (path <> ".metadata")
        Serving.artifactHeadCase companionHead
        lookup "Content-Length" (Serving.responseHeaders companionHead) `shouldBe` Just (encodeUtf8 (show (LBS.length companion) :: Text))

-- | Classify allowed public project discovery separately from every file request.
publicProjectDiscovery :: StubRead -> Bool
publicProjectDiscovery request =
    srMethod request == "GET"
        && srPath request `elem` ["/simple/" <> project <> suffix | project <- [mirrorRoot, mirrorDependency], suffix <- ["", "/"]]

-- | Apply the shared target installation law to an actual pip invocation.
verifyPlatformInstallation :: E2E -> Text -> Text -> IO ()
verifyPlatformInstallation proxy platform architecture =
    observeTarget proxy architecture platform >>= targetInstallCase mirrorGraph (targetExpected architecture platform)

-- | Independent platform filenames and installed architecture markers for both projects.
targetExpected :: Text -> Text -> TargetExpectation
targetExpected architecture platform =
    TargetExpectation
        (ArchitectureSpecific architecture)
        [mirrorWheelFile project platform | project <- [mirrorRoot, mirrorDependency]]
        [(toString (T.replace "-" "_" project) <> "/architecture.txt", encodeUtf8 (architecture <> "\n")) | project <- [mirrorRoot, mirrorDependency]]

-- | Read a real target installation's metadata, selected sources, and complete destination tree.
observeTarget :: E2E -> Text -> Text -> IO (TargetObservation PythonGraph)
observeTarget proxy architecture platform = do
    installed <- pipInstallForPlatform proxy mirrorRoot mirrorVersion platform
    pure
        TargetObservation
            { targetPortability = ArchitectureSpecific architecture
            , targetClient = ptiClient installed
            , targetGraph = installedPythonGraph (ptiTree installed)
            , targetArtifacts = map (T.takeWhileEnd (/= '/')) <$> normaliseSources proxy (ptiSources installed)
            , targetTree = ptiTree installed
            }

-- | Normalise only source URL origins in pip's report, refusing foreign origins.
normaliseSources :: E2E -> Either Text [(Text, Text)] -> Either Text [Text]
normaliseSources proxy sources = sources >>= traverse (normalise . snd)
  where
    normalise url = maybe (Left ("pip used a foreign source: " <> url)) (Right . (redactedProxy <>)) (T.stripPrefix (e2eBaseUrl proxy) url)

-- | Identify an exact distribution GET without confusing its metadata companion.
requestedDistribution :: Text -> StubRead -> Bool
requestedDistribution filename request =
    srMethod request == "GET"
        && ("/" <> filename) `T.isSuffixOf` T.takeWhile (/= '?') (srPath request)

-- | Require successful private GET and HEAD records for every expected distribution.
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

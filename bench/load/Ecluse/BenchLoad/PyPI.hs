-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI HTTP reads and whole-version worker load scenarios over native local fixtures.
module Ecluse.BenchLoad.PyPI (
    pypiFixture,
    pypiPrivateCopy,
    pypiLoadNotes,
    selectWarmArtifacts,
    indexStubWithArtifacts,
    wheelStubWith,
) where

import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, rawPathInfo, responseLBS)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (saProxyPath), selectedArtifact)
import Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (absentUpstream, artifactBytes, delayUpstream, fetchChecked, httpTarget, longCacheTtl, selectPinnedArtifacts, selfHosted, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveBurst, DriveRamp), LoadKnobs (..), Scenario (..), UpstreamFixture, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.Listing (ListingSetup (..), cacheEvictsLoad, cacheFitsLoad, listingLoad, listingWorkingSet, revalidateLoad, slowReaderScenario, withListingStubs)
import Ecluse.BenchLoad.ManagedMirror (managedMirrorScenarios)
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..))
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.BenchLoad.PyPI.Worker (pypiWorkerScenario)
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.BenchLoad.Stress (WarmLoadDriver (..), herdSize, pairedLoad, rampSteps)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Registry.PyPI.Metadata (pypiRead)
import Ecluse.Core.Registry.PyPI.Route (distributionPath)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (..), SimpleIndex (..), simpleIndexMediaType)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (canonicalPep440, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Subset (newestPyPIShare)
import Ecluse.Test.Json (withKeys)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Wai (localhost, selfBaseUrl)

-- | Recurring PyPI reads and whole-version worker measurements through native upstream fixtures.
pypiFixture :: UpstreamFixture
pypiFixture = readLoadContract pypiReadDriver (managedMirrorScenarios <> [pypiWorkerScenario])

pypiReadDriver :: LoadReadDriver
pypiReadDriver =
    LoadReadDriver
        { lrdPrivateCopy = pypiPrivateCopy
        , lrdColdMetadata = indexColdScenario
        , lrdSlowReaders = slowReaderScenario pypiListing "index-cold"
        , lrdAssembledHit = indexScenario "assembled-response-hit" "GET the weighted Simple-index corpus with retained assembled responses. Full public and private indexes are fetched per request, except overlapping public reads share active work." longCacheTtl
        , lrdRevalidate = revalidateScenario
        , lrdCacheFits = cacheFitsScenario
        , lrdCacheEvicts = cacheEvictsScenario
        , lrdPrivateArtifact = wheelScenario PrivateWheel
        , lrdPublicArtifact = wheelScenario PublicOnboarding
        , lrdArtifactCeiling = wheelCeilingScenario
        , lrdHerd = herdScenario
        , lrdWarmUnderCold = warmUnderColdScenario
        , lrdRamp = rampScenario
        , lrdPatternProvider =
            PatternProvider
                { ppCaptureAuthority = "https://files.pythonhosted.org"
                , ppRootUrl = (`indexUrl` "")
                , ppListingSuffix = id
                , ppDiscoveryPath = Nothing
                , ppCapturedRead = const (Right pypiRead)
                , -- Selected replay remains unsupported. Warm reads use native pins separately.
                  ppSelectedArtifacts = \_ _ _ -> pure Map.empty
                }
        , lrdPatternPrivate = lsPrivate pypiListing
        , lrdPatternPublic = \knobs bodies _artifacts -> do
            rewritten <- newIORef mempty
            pure (indexStub rewritten (lkUpstreamLatencyMicros knobs) bodies)
        }

pypiListing :: ListingSetup
pypiListing =
    ListingSetup
        { lsCopy = pypiPrivateCopy
        , lsPrivate = \knobs -> wheelStub (lkUpstreamLatencyMicros knobs) (artifactBytes (lkPayloadBytes knobs))
        , lsCapturePackages = id
        , lsPreflight = preflightListings
        , lsEvictionEntries = evictionEntries . lkCacheMaxEntries
        , lsRevalidateError = "pypi/revalidate-not-modified: no URL to drive"
        }

-- | State the effective eviction size and the limits of the service-time attribution.
pypiLoadNotes :: LoadKnobs -> Text
pypiLoadNotes knobs =
    "Service attribution subtracts one public baseline. Extra private or public hops remain in reported overhead, which is not intrinsic proxy CPU time.\n\n"
        <> "Cache working set: "
        <> show count
        <> " projects. Eviction entry bound: "
        <> either id show (evictionEntries (lkCacheMaxEntries knobs) count)
        <> " (configured "
        <> show (lkCacheMaxEntries knobs)
        <> ").\n"
  where
    count = length (listingWorkingSet pypiListing knobs)

indexColdScenario :: Scenario
indexColdScenario =
    indexScenario "index-cold" "GET the weighted Simple-index corpus with public cache TTL 0. Each request merges a live private overlay and filters files. Concurrent public misses share an in-flight fetch and decode." 0

indexScenario :: Text -> Text -> Int -> Scenario
indexScenario name description ttl =
    scenario name description (listingLoad pypiListing ttl)

-- | The PyPI parts of the private-copy scenarios.
pypiPrivateCopy :: PrivateCopy
pypiPrivateCopy =
    PrivateCopy
        { pcEcosystem = PyPI
        , pcListing = "GET the weighted Simple-index corpus"
        , pcRegistry = "a private index that proxies PyPI"
        , pcPackages = pypiCorpusPackages
        , pcCut = newestPyPIShare
        , pcStub = \latency bodies -> do
            rewritten <- newIORef mempty
            pure (indexStub rewritten latency bodies)
        , pcUrl = indexUrl
        , pcPreflight = traverse_ (void . checkedIndex) . ordNub
        , pcHerdPackage = herdPackage
        }

revalidateScenario :: Scenario
revalidateScenario =
    scenario
        "revalidate-not-modified"
        "GET the heaviest Simple index with a primed If-None-Match validator. The private index and admission plan remain live, but 304 avoids assembly and encoding."
        (revalidateLoad pypiListing)

cacheFitsScenario :: Scenario
cacheFitsScenario =
    scenario
        "cache-fits-large"
        "GET a uniform Simple-index working set with one cache slot per project, so assembled responses fit by entry count. Full public metadata is always fetched."
        (cacheFitsLoad pypiListing)

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    scenario
        "cache-evicts-large"
        "GET the same uniform Simple-index working set with min(configured entries, project count - 1) cache slots, forcing repeated assembly. Full public metadata is always fetched."
        (cacheEvictsLoad pypiListing)

data WheelSource = PrivateWheel | PublicOnboarding

wheelScenario :: WheelSource -> Scenario
wheelScenario source =
    scenario name description $ \knobs k -> do
        let bytes = artifactBytes (lkPayloadBytes knobs)
            latency = lkUpstreamLatencyMicros knobs
            (privateApp, publicApp, ttl) = case source of
                PrivateWheel -> (wheelStub latency bytes, absentUpstream latency, longCacheTtl)
                PublicOnboarding -> (absentUpstream latency, wheelStub latency bytes, 0)
        withProxyOverStubs PyPI knobs ttl Nothing privateApp publicApp wheelMix $ \proxy urls -> do
            for_ (zip pypiCorpusPackages urls) $ \(package, url) -> checkWheel bytes (cpName package) url
            httpTarget k proxy urls
  where
    (name, description) = case source of
        PrivateWheel ->
            ( "wheel-hot-path"
            , "GET a wheel through the private index and relay its bytes. The private index lookup and artifact stream incur two sequential upstream waits. The public upstream serves nothing."
            )
        PublicOnboarding ->
            ( "wheel-onboarding"
            , "GET a wheel after a private index 404, public index admission, and public artifact fetch. Public cache TTL 0 preserves admission work under load, with concurrent misses coalesced. No mirror job runs."
            )

wheelCeilingScenario :: Scenario
wheelCeilingScenario =
    (wheelScenario PrivateWheel)
        { scenarioName = "wheel-ceiling"
        , scenarioDescription = "Measure the private wheel relay at four times the base concurrency under load, one connection for service time, and 2 ms upstream latency. The payload knob sets the streamed body size."
        , scenarioConcurrencyScale = 4
        , scenarioUpstreamLatencyMicros = Just 2_000
        , scenarioPublicLegUnavailableReason = Just "the private-hit artifact relay has no public leg"
        }

herdScenario :: Scenario
herdScenario =
    ( scenario
        "herd"
        ("Send " <> show herdSize <> " simultaneous cold GET /pypi/simple/" <> herdPackage <> " to an idle proxy, once, with public cache TTL 0 and no warm-up. Any admission that reacts to measured memory sees this burst late.")
        ( \knobs k ->
            withUnprimedIndexProxy knobs 0 Nothing pypiCorpusPackages (\port -> [indexUrl port herdPackage]) $ \proxy -> \case
                url : _ -> k (proxied proxy (DriveBurst herdSize url))
                [] -> benchFail "pypi/herd: no URL to drive"
        )
    )
        { scenarioServiceTime = False
        }

herdPackage :: Text
herdPackage = "boto3"

warmUnderColdScenario :: Scenario
warmUnderColdScenario =
    ( scenario
        "warm-under-cold"
        "With cache TTL 3600, measure assembled-response hits and retained selected-version reads for the corpus outside the heavy tier, while a second generator drives listings of the heavy tier at the same concurrency. The heavy tier's private indexes change on every request, so those listings never reuse an assembled response."
        ( pairedLoad
            WarmLoadDriver
                { wldEcosystem = PyPI
                , wldPackages = pypiCorpusPackages
                , wldListingUrl = indexUrl
                , wldArtifactUrl = \port artifact -> localhost port <> "/pypi/" <> saProxyPath artifact
                , wldSelect = selectWarmArtifacts
                , wldPrivate = wheelStubWith
                , wldPublic = indexStubWithArtifacts
                }
        )
    )
        { scenarioServiceTime = False
        }

-- | Resolve warm selected reads from captured PyPI pins through the production projection.
selectWarmArtifacts :: [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
selectWarmArtifacts = selectPinnedArtifacts "pypiPins" $ \package version bytes -> pure $ do
    canonical <- maybe (Left ("invalid captured PyPI pin: " <> version)) (Right . renderVersion) (canonicalPep440 version)
    (info, _) <- first show (projectPyPIIndex defaultLimits (cpPackage package) bytes)
    let route = maybe (Left "captured artifact filename has no PyPI route") Right . distributionPath (cpPackage package)
    selectedArtifact canonical ("captured PyPI version is absent: " <> version) route info

rampScenario :: Scenario
rampScenario =
    ( scenario
        "ramp"
        ("GET the weighted Simple-index corpus with public cache TTL 0, holding " <> T.intercalate ", " (map show rampSteps) <> " connections in turn for the configured duration each. Successes should level off past saturation, not fall.")
        ( \knobs k ->
            withUnprimedIndexProxy knobs 0 Nothing pypiCorpusPackages (weightedMix cpWeight indexUrl pypiCorpusPackages) $ \proxy urls ->
                k (proxied proxy (DriveRamp rampSteps (urlLoad urls)))
        )
    )
        { scenarioServiceTime = False
        }

preflightListings :: [Text] -> IO ()
preflightListings urls =
    for_ (ordNub urls) $ \url -> do
        index <- checkedIndex url
        let filenames = map ifFilename (siFiles index)
        unless (wheelFilename (siName index) `elem` filenames && length filenames > 1) $
            benchFail ("pypi index preflight did not merge public files and the private overlay: " <> url)

withUnprimedIndexProxy :: LoadKnobs -> Int -> Maybe Int -> [CorpusPackage] -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withUnprimedIndexProxy = withListingStubs pypiListing

pypiMountBase :: Text
pypiMountBase = "https://bench.proxy/pypi"

indexUrl :: Int -> Text -> Text
indexUrl port name = localhost port <> "/pypi/simple/" <> name

wheelMix :: Int -> [Text]
wheelMix port = [indexUrl port (cpName package) <> "/" <> wheelFilename (cpName package) | package <- pypiCorpusPackages]

wheelFilename :: Text -> Text
wheelFilename name = name <> "-9999.0.2-py3-none-any.whl"

indexStub :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Application
indexStub rewritten latency captures request respond = do
    delayUpstream latency
    bodies <- selfHosted "https://files.pythonhosted.org" rewritten (selfBaseUrl request) captures
    respond $ case requestPath request of
        ["simple", project] | Just bytes <- Map.lookup project bodies -> responseLBS status200 [(hContentType, simpleIndexMediaType)] bytes
        _ -> responseLBS status404 [] ""

wheelStub :: Int -> LByteString -> Application
wheelStub latency bytes = \request respond -> do
    delayUpstream latency
    respond $ case requestPath request of
        ["simple", project] | knownProject project -> responseLBS status200 [(hContentType, simpleIndexMediaType)] (encode (wheelIndex (selfBaseUrl request) digest size project))
        ["simple", project, filename] | knownProject project && filename == wheelFilename project -> responseLBS status200 [(hContentType, "application/octet-stream")] bytes
        _ -> responseLBS status404 [] ""
  where
    digest = hexSha256Of (LBS.toStrict bytes)
    size = fromIntegral (LBS.length bytes)
    knownProject project = project `elem` map cpName pypiCorpusPackages

-- | Serve only selected capture artifacts, leaving the unmodified index stub as the fallback.
indexStubWithArtifacts :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
indexStubWithArtifacts rewritten latency captures artifacts request respond =
    case Map.lookup (rawPathInfo request) artifacts of
        Just bytes -> do
            delayUpstream latency
            respond (responseLBS status200 [(hContentType, "application/octet-stream")] bytes)
        Nothing -> indexStub rewritten latency captures request respond

-- | Vary private listing bytes per request without changing the trusted wheel.
wheelStubWith :: (Text -> IO [Pair]) -> Int -> LByteString -> Application
wheelStubWith extraFields latency bytes request respond =
    case requestPath request of
        ["simple", project] | project `elem` map cpName pypiCorpusPackages -> do
            delayUpstream latency
            extra <- extraFields project
            respond (responseLBS status200 [(hContentType, simpleIndexMediaType)] (encode (withKeys extra (wheelIndex (selfBaseUrl request) digest size project))))
        _ -> wheelStub latency bytes request respond
  where
    digest = hexSha256Of (LBS.toStrict bytes)
    size = fromIntegral (LBS.length bytes)

requestPath :: Request -> [Text]
requestPath = dropWhileEnd T.null . pathInfo

wheelIndex :: Text -> Text -> Int -> Text -> Value
wheelIndex authority digest size project =
    object
        [ "name" .= project
        , "meta" .= object ["api-version" .= ("1.4" :: Text)]
        , "files"
            .= [ withFileKeys
                    [ "url" .= (authority <> "/simple/" <> project <> "/" <> wheelFilename project)
                    , "hashes" .= object ["sha256" .= digest]
                    , "size" .= size
                    , "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)
                    ]
                    (simpleFile (wheelFilename project))
               ]
        ]

checkedIndex :: Text -> IO SimpleIndex
checkedIndex url = do
    response <- fetchChecked status200 [("Accept", simpleIndexMediaType)] url
    unless (List.lookup hContentType (HTTP.responseHeaders response) == Just simpleIndexMediaType) $
        benchFail ("pypi index preflight returned the wrong media type: " <> url)
    index <- either (benchFail . toText) pure (eitherDecode (HTTP.responseBody response))
    let project = siName index
        expectedPrefix = pypiMountBase <> "/simple/" <> project <> "/"
    unless (("/simple/" <> project) `T.isSuffixOf` url && not (null (siFiles index)) && null (siInvalidEntries index) && all (T.isPrefixOf expectedPrefix . ifUrl) (siFiles index)) $
        benchFail ("pypi index preflight returned an empty or malformed listing, the wrong project, or incorrect artifact URLs: " <> url)
    pure index

checkWheel :: LByteString -> Text -> Text -> IO ()
checkWheel bytes project url = do
    -- Request the artifact first, so onboarding also proves admission without a prior index GET.
    response <- fetchChecked status200 [] url
    unless (HTTP.responseBody response == bytes) (benchFail ("pypi wheel preflight returned different bytes: " <> url))
    index <- checkedIndex (T.dropEnd (T.length (wheelFilename project) + 1) url)
    case siFiles index of
        [file]
            | ifFilename file == wheelFilename project
            , Map.lookup "sha256" (ifHashes file) == Just (hexSha256Of (LBS.toStrict bytes))
            , ifSize file == Just (fromIntegral (LBS.length bytes)) ->
                pass
        _ -> benchFail ("pypi wheel preflight advertised a different filename, digest, or size: " <> url)

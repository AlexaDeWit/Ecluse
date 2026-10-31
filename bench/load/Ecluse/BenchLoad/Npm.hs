-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm load scenarios covering serving, admission, and the mirror worker.
Private reads stay live, while public requests can share a fetch at zero cache TTL.
-}
module Ecluse.BenchLoad.Npm (
    npmFixture,
    npmPrivateCopy,
    corpusPublicStub,
    privateOverlayStub,
    privateOverlayStubWith,
    preflightNpmListing,
) where

import Data.Aeson (Value (..), encode, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (saProxyPath))
import Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, delayUpstream, fetchChecked, httpTarget, longCacheTtl, selfHosted, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (..), LoadKnobs (..), Scenario (..), Target (Target), UpstreamFixture, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.Listing (ListingSetup (..), cacheEvictsLoad, cacheFitsLoad, listingLoad, revalidateLoad, slowReaderScenario, withListingStubs)
import Ecluse.BenchLoad.NpmArtifact (selectedNpmArtifact)
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..))
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.BenchLoad.Stress (WarmLoadDriver (..), herdSize, pairedLoad, rampSteps)
import Ecluse.BenchLoad.Worker (runWorkerLoad)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Hash, HashAlg (SHA1, SRI), PackageDetails (pkgVersion), PackageInfo (..), PackageName, artUrl, infoName, infoVersions, mkPackageName, pkgArtifacts, renderPackageName, unscopedName)
import Ecluse.Core.Queue (
    MirrorJob (
        MirrorJob,
        jobPackage,
        jobTraceContext,
        jobTriggerFilename,
        jobVersion
    ),
    MirrorQueue (receive),
    enqueue,
 )
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent))
import Ecluse.Core.Registry.MirrorUpload (inventoryDetails)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Publish (prepareNpmSource)
import Ecluse.Core.Registry.Public (newPublicControl)
import Ecluse.Core.Registry.Publish.Internal (MirrorPublish (..), PreparedMirrorUpload (..), PreparedMirrorVersion (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Core.Worker (
    WorkerRuntime (
        WorkerRuntime,
        wrClient,
        wrHeartbeat,
        wrInjectTraceContext,
        wrMetrics,
        wrPolicies,
        wrPublicControl,
        wrQueue,
        wrTracing
    ),
    newWorkerHeartbeat,
    processBatch,
    runWorkerM,
 )
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), corpusPackages, cpName, readCorpusPins)
import Ecluse.Test.Corpus.Subset (newestNpmShare)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (hexSha1OfLazy, sriSha512OfLazy, unsafeFilename, unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Wai (localhost, selfBaseUrl)
import Ecluse.Test.Worker (admitAllPolicies, workerSourceAt)

-- | The npm load fixture covers metadata, artifact relays, cache churn, and mirror jobs.
npmFixture :: UpstreamFixture
npmFixture =
    readLoadContract
        npmReadDriver
        [workerScenario]

npmReadDriver :: LoadReadDriver
npmReadDriver =
    LoadReadDriver
        { lrdPrivateCopy = npmPrivateCopy
        , lrdColdMetadata = mergeScenario
        , lrdSlowReaders = slowReaderScenario npmListing "merge-cold"
        , lrdAssembledHit = assembledHitScenario
        , lrdRevalidate = revalidateScenario
        , lrdCacheFits = cacheFitsScenario
        , lrdCacheEvicts = cacheEvictsScenario
        , lrdPrivateArtifact = tarballScenario
        , lrdPublicArtifact = tarballOnboardingScenario
        , lrdArtifactCeiling = tarballCeilingScenario
        , lrdHerd = herdScenario
        , lrdWarmUnderCold = warmUnderColdScenario
        , lrdRamp = rampScenario
        , lrdPatternProvider =
            PatternProvider
                { ppCaptureAuthority = "https://registry.npmjs.org"
                , ppRootUrl = (`packageUrl` "")
                , ppListingSuffix = id
                , ppDiscoveryPath = Nothing
                , ppCapturedRead = const (Right npmRead)
                , ppSelectedArtifacts = \selected packages captures -> do
                    pins <- loadPins
                    either benchFail pure (selectArtifacts selected pins packages captures)
                }
        , lrdPatternPrivate = lsPrivate npmListing
        , lrdPatternPublic = \knobs bodies artifacts -> do
            rewritten <- newIORef mempty
            pure (corpusPublicStub rewritten (lkUpstreamLatencyMicros knobs) bodies artifacts)
        }

npmListing :: ListingSetup
npmListing =
    ListingSetup
        { lsCopy = npmPrivateCopy
        , lsPrivate = \knobs -> privateOverlayStub (lkUpstreamLatencyMicros knobs) (artifactBytes (lkPayloadBytes knobs))
        , lsCapturePackages = const corpusPackages
        , lsPreflight = traverse_ (preflightNpmListing True) . ordNub
        , lsEvictionEntries = evictionEntries . lkCacheMaxEntries
        , lsRevalidateError = "revalidate-not-modified: no URL to drive"
        }

mergeScenario :: Scenario
mergeScenario =
    scenario
        "merge-cold"
        "GET /npm/{pkg} over the weighted corpus with public cache TTL 0. Concurrent public misses share one fetch and decode. Every request reads private metadata, merges, filters, rewrites URLs, and serialises."
        (listingLoad npmListing 0)

-- | The npm parts of the private-copy scenarios.
npmPrivateCopy :: PrivateCopy
npmPrivateCopy =
    PrivateCopy
        { pcEcosystem = Npm
        , pcListing = "GET /npm/{pkg} over the weighted corpus"
        , pcRegistry = "a private registry that proxies npmjs"
        , pcPackages = corpusPackages
        , pcCut = newestNpmShare
        , pcStub = \latency bodies -> do
            rewritten <- newIORef mempty
            pure (corpusPublicStub rewritten latency bodies Map.empty)
        , pcUrl = packageUrl
        , pcPreflight = traverse_ (preflightNpmListing False) . ordNub
        , pcHerdPackage = herdPackage
        }

assembledHitScenario :: Scenario
assembledHitScenario =
    scenario
        "assembled-response-hit"
        "GET /npm/{pkg} over the weighted corpus with retained assembled responses. Each request fetches full public and private metadata, except overlapping public reads share active work."
        (listingLoad npmListing longCacheTtl)

revalidateScenario :: Scenario
revalidateScenario =
    scenario
        "revalidate-not-modified"
        "GET the heaviest corpus packument with a primed If-None-Match validator. Each request reads private metadata and computes the plan, then returns 304 without assembly or encoding."
        (revalidateLoad npmListing)

cacheFitsScenario :: Scenario
cacheFitsScenario =
    scenario
        "cache-fits-large"
        "GET a uniform corpus working set with enough assembled-response slots for every project. Full public metadata is always fetched. Compare with cache-evicts-large for assembly reuse."
        (cacheFitsLoad npmListing)

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    scenario
        "cache-evicts-large"
        "GET the same uniform corpus working set with min(configured entries, project count - 1) slots, forcing repeated assembly. Full public metadata is always fetched."
        (cacheEvictsLoad npmListing)

tarballScenario :: Scenario
tarballScenario =
    scenario
        "tarball-hot-path"
        "GET /npm/{pkg}/-/{unscoped-pkg}-9999.0.2.tgz. Resolve the private artifact and stream its bytes from the local upstream."
        (\knobs k -> withNpmProxy knobs longCacheTtl Nothing tarballMix (httpTarget k))

tarballOnboardingScenario :: Scenario
tarballOnboardingScenario =
    scenario
        "tarball-onboarding"
        "GET /npm/{pkg}/-/{unscoped-pkg}-1.0.0.tgz. A private 404 precedes public admission and artifact streaming. The private probe and public artifact add two sequential upstream waits. The mount mirrors nothing, so no mirror job is enqueued."
        ( \knobs k ->
            let latency = lkUpstreamLatencyMicros knobs
                bytes = artifactBytes (lkPayloadBytes knobs)
             in withProxyOverStubs
                    Npm
                    knobs
                    longCacheTtl
                    Nothing
                    (onboardingPrivateStub latency)
                    (onboardingPublicStub latency bytes)
                    onboardingMix
                    (httpTarget k)
        )

tarballCeilingScenario :: Scenario
tarballCeilingScenario =
    ( scenario
        "tarball-ceiling"
        "Measure the private artifact relay at four times the base concurrency under load, one connection for service time, and 2 ms upstream latency. The payload knob sets the streamed body size."
        (\knobs k -> withNpmProxy knobs longCacheTtl Nothing tarballMix (httpTarget k))
    )
        { scenarioConcurrencyScale = 4
        , scenarioUpstreamLatencyMicros = Just 2_000
        , scenarioPublicLegUnavailableReason = Just "the private-hit artifact relay has no public leg"
        }

herdScenario :: Scenario
herdScenario =
    ( scenario
        "herd"
        ("Send " <> show herdSize <> " simultaneous cold GET /npm/" <> herdPackage <> " to an idle proxy, once, with public cache TTL 0 and no warm-up. Any admission that reacts to measured memory sees this burst late.")
        ( \knobs k ->
            withNpmProxy knobs 0 Nothing (\port -> [packageUrl port herdPackage]) $ \proxy -> \case
                url : _ -> k (proxied proxy (DriveBurst herdSize url))
                [] -> benchFail "herd: no URL to drive"
        )
    )
        { scenarioServiceTime = False
        }

herdPackage :: Text
herdPackage = "typescript"

warmUnderColdScenario :: Scenario
warmUnderColdScenario =
    ( scenario
        "warm-under-cold"
        "With cache TTL 3600, measure assembled-response hits and retained selected-version reads for the corpus outside the heavy tier, while a second generator drives listings of the heavy tier at the same concurrency. The heavy tier's private documents change on every request, so those listings never reuse an assembled response."
        warmUnderCold
    )
        { scenarioServiceTime = False
        }

warmUnderCold :: LoadKnobs -> (Target -> IO a) -> IO a
warmUnderCold =
    pairedLoad
        WarmLoadDriver
            { wldEcosystem = Npm
            , wldPackages = corpusPackages
            , wldListingUrl = packageUrl
            , wldArtifactUrl = \port -> packageUrl port . saProxyPath
            , wldSelect = \packages bodies -> do
                pins <- loadPins
                either benchFail pure (selectArtifacts (Just "pinned") pins packages bodies)
            , wldPrivate = privateOverlayStubWith
            , wldPublic = corpusPublicStub
            }

loadPins :: IO (Map Text Text)
loadPins = readCorpusPins (.: "pins") >>= either (benchFail . toText) pure

selectArtifacts :: Maybe String -> Map Text Text -> [CorpusPackage] -> Map Text LByteString -> Either Text (Map Text SelectedArtifact)
selectArtifacts selected pins packages captures =
    case selected of
        Just choice -> Map.fromList <$> traverse (select choice) packages
        _ -> Right Map.empty
  where
    select choice package = do
        let name = cpName package
        version <- if choice == "pinned" then maybe (Left ("missing captured pin: " <> name)) Right (Map.lookup name pins) else Right (toText choice)
        bytes <- maybe (Left ("missing captured body: " <> name)) Right (Map.lookup name captures)
        artifact <- selectedNpmArtifact (cpPackage package) version (LBS.toStrict bytes)
        pure (name, artifact)

rampScenario :: Scenario
rampScenario =
    ( scenario
        "ramp"
        ("GET /npm/{pkg} over the weighted corpus with public cache TTL 0, holding " <> T.intercalate ", " (map show rampSteps) <> " connections in turn for the configured duration each. Successes should level off past saturation, not fall.")
        (\knobs k -> withNpmProxy knobs 0 Nothing serveMix (\proxy urls -> k (proxied proxy (DriveRamp rampSteps (urlLoad urls)))))
    )
        { scenarioServiceTime = False
        }

withNpmProxy :: LoadKnobs -> Int -> Maybe Int -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withNpmProxy knobs ttl maxEntries = withListingStubs npmListing knobs ttl maxEntries corpusPackages

packageUrl :: Int -> Text -> Text
packageUrl port name = localhost port <> "/npm/" <> name

serveMix :: Int -> [Text]
serveMix = weightedMix cpWeight packageUrl corpusPackages

tarballMix :: Int -> [Text]
tarballMix port =
    concatMap (\cp -> replicate (cpWeight cp) (localhost port <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpPackage cp) <> "-9999.0.2.tgz")) corpusPackages

workerScenario :: Scenario
workerScenario =
    ( scenario
        "worker-mirroring"
        "Run the mirror worker fetch, integrity check, publish, and acknowledgement loop in the harness process. The initial presence probe reports absent and the final probe confirms the write, so every job exercises the complete pipeline."
        $ \knobs k -> do
            counter <- newIORef (0 :: Int)
            publish <- succeedingPublishClient counter
            let bytes = artifactBytes (lkPayloadBytes knobs)
            testWithApplication (pure (stubUpstream octetContentType (lkUpstreamLatencyMicros knobs) bytes)) $ \artPort -> do
                withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    publicControl <- newPublicControl (\_ _ _ -> pass) (\_ _ -> pass)
                    queue <-
                        newBoundedInMemoryQueue
                            (defaultMemoryQueueConfig 16)
                            (\n -> benchFail ("worker scenario: the in-memory mirror queue dropped a job (running total " <> show n <> "); the enqueue-receive cadence broke"))
                    heartbeat <- newWorkerHeartbeat
                    logEnv <- newTestLogEnv
                    let runtime =
                            WorkerRuntime
                                { wrQueue = queue
                                , wrClient = client
                                , wrPublicControl = publicControl
                                , wrHeartbeat = heartbeat
                                , wrMetrics = noopWorkerMetricsPort
                                , wrTracing = passthroughWorkerTracingPort
                                , wrInjectTraceContext = id
                                , wrPolicies = workerSourceAt artUrl (admitAllPolicies publish (jobHashes bytes))
                                }
                        artUrl = localhost artPort <> "/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
                        job = mirrorJob
                    k (Target Nothing (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter)))
    )
        { scenarioInProcess = True
        }

runWorkerLoop :: LoadKnobs -> LogEnv -> WorkerRuntime -> MirrorQueue -> MirrorJob -> IORef Int -> IO [Double]
runWorkerLoop knobs logEnv runtime queue job counter = do
    latencies <- runWorkerLoad knobs prepare (runWorkerM logEnv mempty runtime . processBatch) (const pass)
    published <- readIORef counter
    when (published /= length latencies) $
        benchFail
            ( "worker scenario: "
                <> show published
                <> " of "
                <> show (length latencies)
                <> " jobs published: a harness wiring failure (fetch, verify, or publish broke)"
            )
    pure latencies
  where
    prepare = do
        enqueue queue job >>= either (\f -> fail ("bench enqueue faulted: " <> show f)) pure
        receive queue >>= either (\f -> fail ("bench receive faulted: " <> show f)) pure

mirrorJob :: MirrorJob
mirrorJob =
    MirrorJob
        { jobPackage = packageName
        , jobVersion = mkVersion Npm "1.0.0"
        , jobTriggerFilename = Just (unsafeFilename (packageText <> "-1.0.0.tgz"))
        , jobTraceContext = Nothing
        }

succeedingPublishClient :: IORef Int -> IO MirrorPublish
succeedingPublishClient counter = do
    committed <- newIORef Nothing
    pure
        MirrorPublish
            { mpMirrorCompanion = Nothing
            , mpPrepareVersion = \name version doc -> do
                (inventory, _) <- prepareNpmSource name version doc
                pure $ PreparedMirrorVersion inventory $ \_ _ _ -> pure . Right $ PreparedMirrorUpload $ \_ -> do
                    atomicModifyIORef' counter (\n -> (n + 1, ()))
                    let details = inventoryDetails inventory
                    writeIORef committed (Just (PackageInfo name (Map.singleton (renderVersion (pkgVersion details)) details) mempty []))
                    pure (Right ())
            , mpNativeComplete = \_ _ _ -> False
            , mpProbeInventory = \_ _ -> maybe (Left MetadataAbsent) Right <$> atomicModifyIORef' committed (Nothing,)
            }

jobHashes :: LByteString -> NonEmpty Hash
jobHashes bytes = unsafeHash SRI (sriSha512OfLazy bytes) :| [unsafeHash SHA1 (hexSha1OfLazy bytes)]

stubUpstream :: ByteString -> Int -> LByteString -> Application
stubUpstream contentType latency body _request respond = do
    delayUpstream latency
    respond (responseLBS status200 [(hContentType, contentType)] body)

jsonContentType, octetContentType :: ByteString
jsonContentType = "application/json"
octetContentType = "application/octet-stream"

-- | Serve complete metadata captures and only the selected known artifact paths.
corpusPublicStub :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
corpusPublicStub rewritten latency bodies artifacts request respond = do
    delayUpstream latency
    served <- selfHosted "https://registry.npmjs.org" rewritten (selfBaseUrl request) bodies
    respond $ case Map.lookup (rawPathInfo request) artifacts of
        Just bytes -> responseLBS status200 [(hContentType, octetContentType)] bytes
        Nothing -> case requestedPackage request >>= (`Map.lookup` served) of
            Just packument -> responseLBS status200 [(hContentType, jsonContentType)] packument
            Nothing -> responseLBS status404 [(hContentType, jsonContentType)] "{}"

-- | Only the trusted overlay artifact exists privately. Captured public artifacts miss here.
privateOverlayStub :: Int -> LByteString -> Application
privateOverlayStub = privateOverlayStubWith (const (pure []))

-- | 'privateOverlayStub' with extra top-level fields per package name, drawn on every request.
privateOverlayStubWith :: (Text -> IO [Pair]) -> Int -> LByteString -> Application
privateOverlayStubWith extraFields latency bytes request respond = do
    delayUpstream latency
    let mPkg = requestedPackage request
    case mPkg of
        Just pkg
            | "/-/" `T.isInfixOf` pkg ->
                let (name, file) = T.breakOn "/-/" pkg
                 in if file == "/-/" <> tarballStem name <> "-9999.0.2.tgz"
                        then respond (responseLBS status200 [(hContentType, octetContentType)] bytes)
                        else respond (responseLBS status404 [] "")
        Just pkg -> do
            extra <- extraFields pkg
            respond (responseLBS status200 [(hContentType, jsonContentType)] (encode (privateOverlay (selfBaseUrl request) pkg extra)))
        Nothing ->
            respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

requestedPackage :: Request -> Maybe Text
requestedPackage request = case pathInfo request of
    [] -> Nothing
    segments -> Just (T.intercalate "/" segments)

onboardingPrivateStub :: Int -> Application
onboardingPrivateStub latency _request respond = do
    delayUpstream latency
    respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

onboardingPublicStub :: Int -> LByteString -> Application
onboardingPublicStub latency bytes request respond = do
    delayUpstream latency
    case requestedPackage request of
        Just pkg
            | "/-/" `T.isInfixOf` pkg ->
                respond (responseLBS status200 [(hContentType, octetContentType)] bytes)
        Just pkg ->
            respond (responseLBS status200 [(hContentType, jsonContentType)] (encode (onboardingPackument (selfBaseUrl request) pkg)))
        Nothing ->
            respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

onboardingPackument :: Text -> Text -> Value
onboardingPackument base name =
    packumentValue
        name
        onboardingVersion
        [(onboardingVersion, versionObj)]
        ["created" .= publishedLongAgo, Key.fromText onboardingVersion .= publishedLongAgo]
        ["_id" .= name]
  where
    versionObj =
        versionValue
            ( (versionSpec name onboardingVersion (base <> "/" <> name <> "/-/" <> tarballStem name <> "-" <> onboardingVersion <> ".tgz"))
                { vsIntegrity = Just validSha512Sri
                , vsShasum = Just validSha1
                }
            )

onboardingVersion :: Text
onboardingVersion = "1.0.0"

onboardingMix :: Int -> [Text]
onboardingMix port =
    [localhost port <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpPackage cp) <> "-" <> onboardingVersion <> ".tgz" | cp <- corpusPackages]

privateOverlay :: Text -> Text -> [Pair] -> Value
privateOverlay authority name extra =
    packumentValue
        name
        "9999.0.2"
        [(version, overlayVersionObject authority name version) | version <- overlayVersions]
        (("created" .= publishedLongAgo) : [Key.fromText version .= publishedLongAgo | version <- overlayVersions])
        (("_id" .= name) : extra)
  where
    overlayVersions :: [Text]
    overlayVersions = ["9999.0.0", "9999.0.1", "9999.0.2"]

overlayVersionObject :: Text -> Text -> Text -> Value
overlayVersionObject authority name version =
    versionValue
        ( (versionSpec name version (authority <> "/" <> name <> "/-/" <> unscoped <> "-" <> version <> ".tgz"))
            { vsIntegrity = Just validSha512Sri
            , vsShasum = Just validSha1
            }
        )
  where
    unscoped = tarballStem name

tarballStem :: Text -> Text
tarballStem name = case T.breakOn "/" name of
    (scope, base)
        | "@" `T.isPrefixOf` scope && not (T.null base) -> T.drop 1 base
    _ -> name

packageText :: Text
packageText = "bench-pkg"

packageName :: PackageName
packageName = mkPackageName Npm Nothing packageText

publishedLongAgo :: Text
publishedLongAgo = toText (iso8601Show (addUTCTime (negate (400 * nominalDay)) benchNow))

-- | Require native metadata, proxy artifact locations, and the overlay when the scenario merges one.
preflightNpmListing :: Bool -> Text -> IO ()
preflightNpmListing overlay url = do
    response <- fetchChecked status200 [] url
    unless (List.lookup hContentType (HTTP.responseHeaders response) == Just jsonContentType) $
        benchFail ("npm listing preflight returned the wrong media type: " <> url)
    name <- maybe (benchFail ("npm listing preflight has no package path: " <> url)) pure (nonEmptyPath (snd (T.breakOnEnd "/npm/" url)))
    package <- either (benchFail . show) pure (projectName name)
    (info, value) <- either (benchFail . show) pure (projectNpmManifest defaultLimits package (toStrict (HTTP.responseBody response)))
    let versions = infoVersions info
        expectedPrefix = "https://bench.proxy/npm/" <> name <> "/-/"
        artifacts = concatMap (toList . pkgArtifacts) (Map.elems versions)
        namesMatch =
            renderPackageName (infoName info) == name && case value of
                Object document -> KM.lookup "name" document == Just (String name)
                _ -> False
    unless (namesMatch && not (Map.null versions) && null (infoInvalidEntries info) && not (null artifacts) && all (T.isPrefixOf expectedPrefix . artUrl) artifacts) $
        benchFail ("npm listing preflight returned an empty or malformed listing, the wrong package, or incorrect artifact URLs: " <> url)
    when (overlay && (not (all (`Map.member` versions) ["9999.0.0", "9999.0.1", "9999.0.2"]) || Map.size versions <= 3)) $
        benchFail ("npm listing preflight did not merge public versions and the private overlay: " <> url)
  where
    nonEmptyPath name = if T.null name then Nothing else Just name

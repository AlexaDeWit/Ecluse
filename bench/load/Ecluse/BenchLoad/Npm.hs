-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Local npm load scenarios for metadata, private copies of public versions, artifacts, caches,
advisory databases, admission under memory pressure, and the mirror worker. Private reads stay
live. Public requests can share one in-flight fetch even at zero cache TTL.
-}
module Ecluse.BenchLoad.Npm (
    npmFixture,
    corpusPublicStub,
    privateOverlayStub,
    privateOverlayStubWith,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, encode, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Time (addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTime)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Advisories (allRulesAdvisories, shippedAdvisories)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, fetchChecked, httpTarget, loadCorpusBodies, loadCorpusCuts, longCacheTtl, primeETag, selfHosted, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (..), Load (Load), LoadKnobs (..), Scenario (..), Target (Target), UpstreamFixture (..), proxied, scenario, urlLoad)
import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (saProxyPath, saUpstreamUrl))
import Ecluse.BenchLoad.PatternScenario (loadPins, patternScenarios, selectArtifacts)
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess, proxyPort)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Hash, HashAlg (SHA1, SRI), PackageName, mkPackageName, unscopedName)
import Ecluse.Core.Queue (
    MirrorJob (
        MirrorJob,
        jobArtifactFilename,
        jobArtifactUrl,
        jobPackage,
        jobTraceContext,
        jobVersion
    ),
    MirrorQueue (receive),
    enqueue,
 )
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Registry (BodyOutcome (UnreadStatus))
import Ecluse.Core.Registry.Publish (MirrorPublish (..))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker (
    WorkerRuntime (
        WorkerRuntime,
        wrHeartbeat,
        wrInjectTraceContext,
        wrManager,
        wrMetrics,
        wrPolicies,
        wrQueue,
        wrTracing
    ),
    newWorkerHeartbeat,
    processBatch,
    runWorkerM,
 )
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpTier, cpWeight), CorpusTier (Heavy), corpusPackages, cpName)
import Ecluse.Test.Corpus.Subset (newestNpmShare)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (hexSha1OfLazy, sriSha512OfLazy, unsafeFilename, unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Wai (localhost, selfBaseUrl)
import Ecluse.Test.Worker (admitAllPolicies)

-- | The npm load fixture covers metadata, artifact relays, cache churn, and mirror jobs.
npmFixture :: UpstreamFixture
npmFixture =
    UpstreamFixture
        { fixtureEcosystem = Npm
        , fixtureScenarios =
            [ mergeScenario
            , shippedAdvisories Npm mergeScenario
            , allRulesAdvisories Npm mergeScenario
            , privateShareScenario 5
            , privateShareScenario 25
            , heavyPrivateScenario
            , assembledHitScenario
            , revalidateScenario
            , shippedAdvisories Npm revalidateScenario
            , cacheFitsScenario
            , cacheEvictsScenario
            , tarballScenario
            , tarballOnboardingScenario
            , tarballCeilingScenario
            , herdScenario
            , warmUnderColdScenario
            , rampScenario
            , workerScenario
            ]
                <> patternScenarios
                    Npm
                    corpusPackages
                    (\knobs -> privateOverlayStub (lkUpstreamLatencyMicros knobs) (artifactBytes (lkPayloadBytes knobs)))
                    ( \knobs bodies artifacts -> do
                        rewritten <- newIORef mempty
                        pure (corpusPublicStub rewritten (lkUpstreamLatencyMicros knobs) bodies artifacts)
                    )
                    packageUrl
        }

mergeScenario :: Scenario
mergeScenario =
    scenario
        "merge-cold"
        "GET /npm/{pkg} over the weighted corpus with public cache TTL 0. Concurrent public misses share one fetch and decode. Every request reads private metadata, merges, filters, rewrites URLs, and serialises."
        (\knobs k -> withNpmProxy knobs 0 Nothing serveMix (httpTarget k))

heavyPrivateScenario :: Scenario
heavyPrivateScenario =
    scenario
        "heavy-private"
        "GET /npm/{pkg} over the weighted corpus with public cache TTL 0, while the private upstream returns the complete public capture, as a private registry that proxies npmjs does. Each request decodes its own private copy, which single-flight cannot share across callers."
        (withPrivateCopy loadCorpusBodies)

privateShareScenario :: Integer -> Scenario
privateShareScenario percent =
    scenario
        ("heavy-private-" <> show percent <> "pct")
        ("GET /npm/{pkg} over the weighted corpus with public cache TTL 0, while the private upstream returns each capture cut to its newest " <> show percent <> "% of versions by publish time, at least one, as a mirror target that has mirrored those versions does. The private copy stays fixed for the run. Each request decodes its own private copy, which single-flight cannot share across callers.")
        (withPrivateCopy (loadCorpusCuts (newestNpmShare (percent % 100))))

-- The private upstream serves the corpus as the loader reads it, and the public upstream serves it whole.
withPrivateCopy :: ([CorpusPackage] -> IO (Map Text LByteString)) -> LoadKnobs -> (Target -> IO a) -> IO a
withPrivateCopy loadPrivate knobs k = do
    bodies <- loadCorpusBodies corpusPackages
    private <- loadPrivate corpusPackages
    privateRewritten <- newIORef mempty
    publicRewritten <- newIORef mempty
    let latency = lkUpstreamLatencyMicros knobs
    withProxyOverStubs
        Npm
        knobs
        0
        Nothing
        (corpusPublicStub privateRewritten latency private Map.empty)
        (corpusPublicStub publicRewritten latency bodies Map.empty)
        serveMix
        (httpTarget k)

assembledHitScenario :: Scenario
assembledHitScenario =
    scenario
        "assembled-response-hit"
        "GET /npm/{pkg} over the weighted corpus with retained assembled responses. Each request fetches full public and private metadata, except overlapping public reads share active work."
        (\knobs k -> withNpmProxy knobs longCacheTtl Nothing serveMix (httpTarget k))

revalidateScenario :: Scenario
revalidateScenario =
    scenario
        "revalidate-not-modified"
        "GET the heaviest corpus packument with a primed If-None-Match validator. Each request reads private metadata and computes the plan, then returns 304 without assembly or encoding."
        ( \knobs k ->
            withNpmProxy knobs longCacheTtl Nothing (uniformMix (take 1 (workingSet knobs))) $ \proxy -> \case
                url : _ -> do
                    etag <- primeETag url
                    k (proxied proxy (DriveHttp (Load [("If-None-Match", etag)] [url])))
                [] -> benchFail "revalidate-not-modified: no URL to drive"
        )

cacheFitsScenario :: Scenario
cacheFitsScenario =
    scenario
        "cache-fits-large"
        "GET a uniform corpus working set with enough assembled-response slots for every project. Full public metadata is always fetched. Compare with cache-evicts-large for assembly reuse."
        ( \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (Just (length pkgs)) (uniformMix pkgs) (httpTarget k)
        )

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    scenario
        "cache-evicts-large"
        "GET the same uniform corpus working set with BENCH_LOAD_CACHE_MAX_ENTRIES slots. A bound below the working set forces repeated assembly. Full public metadata is always fetched."
        (\knobs k -> withNpmProxy knobs longCacheTtl (Just (lkCacheMaxEntries knobs)) (uniformMix (workingSet knobs)) (httpTarget k))

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
        "Measure the private artifact relay at four times the base concurrency and 2 ms upstream latency. The payload knob sets the streamed body size."
        (\knobs k -> withNpmProxy knobs{lkUpstreamLatencyMicros = 2_000} longCacheTtl Nothing tarballMix (httpTarget k))
    )
        { scenarioConcurrencyScale = 4
        }

herdScenario :: Scenario
herdScenario =
    ( scenario
        "herd"
        ("Send " <> show herdSize <> " simultaneous cold GET /npm/typescript to an idle proxy, once, with public cache TTL 0 and no warm-up. Any admission that reacts to measured memory sees this burst late.")
        ( \knobs k ->
            withNpmProxy knobs 0 Nothing (\port -> [packageUrl port "typescript"]) $ \proxy -> \case
                url : _ -> k (proxied proxy (DriveBurst herdSize url))
                [] -> benchFail "herd: no URL to drive"
        )
    )
        { scenarioServiceTime = False
        }

herdSize :: Int
herdSize = 100

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
warmUnderCold knobs k = do
    bodies <- loadCorpusBodies corpusPackages
    pins <- loadPins
    selected <- either benchFail pure (selectArtifacts Npm (Just "pinned") pins warmPackages bodies)
    artifactRequests <- traverse (HTTP.parseRequest . toString . saUpstreamUrl) (Map.elems selected)
    served <- newIORef (0 :: Int)
    rewritten <- newIORef mempty
    let latency = lkUpstreamLatencyMicros knobs
        bytes = artifactBytes (lkPayloadBytes knobs)
        artifacts = Map.fromList [(HTTP.path request, bytes) | request <- artifactRequests]
        nonce name
            | name `elem` map cpName coldPackages = do
                n <- atomicModifyIORef' served (\count -> (count + 1, count))
                pure ["description" .= ("bench nonce " <> show n :: Text)]
            | otherwise = pure []
    withProxyOverStubs
        Npm
        knobs
        longCacheTtl
        Nothing
        (privateOverlayStubWith nonce latency bytes)
        (corpusPublicStub rewritten latency bodies artifacts)
        (const [])
        $ \proxy _ -> do
            let port = proxyPort proxy
                listings = concatMap (\cp -> replicate (cpWeight cp) (packageUrl port (cpName cp)))
                warmUrls = listings warmPackages <> [packageUrl port (saProxyPath artifact) | artifact <- Map.elems selected]
            -- Prime the assembled and selected-version stores, and prove every warm path serves.
            for_ (ordNub warmUrls) (void . fetchChecked status200 [])
            k (proxied proxy (DriveUnder (urlLoad warmUrls) (urlLoad (listings coldPackages))))

rampScenario :: Scenario
rampScenario =
    ( scenario
        "ramp"
        ("GET /npm/{pkg} over the weighted corpus with public cache TTL 0, holding " <> T.intercalate ", " (map show rampSteps) <> " connections in turn for the configured duration each. Successes should level off past saturation, not fall.")
        (\knobs k -> withNpmProxy knobs 0 Nothing serveMix (\proxy urls -> k (proxied proxy (DriveRamp rampSteps (urlLoad urls)))))
    )
        { scenarioServiceTime = False
        }

rampSteps :: [Int]
rampSteps = [10, 25, 50, 100, 200, 400]

-- The heavy tier drives the cold side of the paired scenario, and the rest the warm side.
coldPackages, warmPackages :: [CorpusPackage]
(coldPackages, warmPackages) = partition ((== Heavy) . cpTier) corpusPackages

withNpmProxy :: LoadKnobs -> Int -> Maybe Int -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withNpmProxy knobs ttl maxEntries mkMix body = do
    bodies <- loadCorpusBodies corpusPackages
    rewritten <- newIORef mempty
    let bytes = artifactBytes (lkPayloadBytes knobs)
        latency = lkUpstreamLatencyMicros knobs
    withProxyOverStubs
        Npm
        knobs
        ttl
        maxEntries
        (privateOverlayStub latency bytes)
        (corpusPublicStub rewritten latency bodies Map.empty)
        mkMix
        body

packageUrl :: Int -> Text -> Text
packageUrl port name = localhost port <> "/npm/" <> name

serveMix :: Int -> [Text]
serveMix port =
    concatMap (\cp -> replicate (cpWeight cp) (packageUrl port (cpName cp))) corpusPackages

uniformMix :: [CorpusPackage] -> Int -> [Text]
uniformMix pkgs port = map (packageUrl port . cpName) pkgs

tarballMix :: Int -> [Text]
tarballMix port =
    concatMap (\cp -> replicate (cpWeight cp) (localhost port <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpPackage cp) <> "-9999.0.2.tgz")) corpusPackages

workingSet :: LoadKnobs -> [CorpusPackage]
workingSet knobs = take (max 1 (lkWorkingSet knobs)) corpusPackages

workerScenario :: Scenario
workerScenario =
    ( scenario
        "worker-mirroring"
        "Run the mirror worker fetch, integrity check, publish, and acknowledgement loop in the harness process. The presence probe reports absent, so every job exercises the complete pipeline."
        $ \knobs k -> do
            counter <- newIORef (0 :: Int)
            let bytes = artifactBytes (lkPayloadBytes knobs)
            testWithApplication (pure (stubUpstream octetContentType (lkUpstreamLatencyMicros knobs) bytes)) $ \artPort -> do
                manager <- newManager defaultManagerSettings
                queue <-
                    newBoundedInMemoryQueue
                        (defaultMemoryQueueConfig 16)
                        (\n -> benchFail ("worker scenario: the in-memory mirror queue dropped a job (running total " <> show n <> "); the enqueue-receive cadence broke"))
                heartbeat <- newWorkerHeartbeat
                logEnv <- newTestLogEnv
                let runtime =
                        WorkerRuntime
                            { wrQueue = queue
                            , wrManager = manager
                            , wrHeartbeat = heartbeat
                            , wrMetrics = noopWorkerMetricsPort
                            , wrTracing = passthroughWorkerTracingPort
                            , wrInjectTraceContext = id
                            , wrPolicies = admitAllPolicies (succeedingPublishClient counter) (jobHashes bytes)
                            }
                    artUrl = localhost artPort <> "/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
                    job = mirrorJob artUrl
                k (Target Nothing (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter)))
    )
        { scenarioInProcess = True
        }

runWorkerLoop :: LoadKnobs -> LogEnv -> WorkerRuntime -> MirrorQueue -> MirrorJob -> IORef Int -> IO [Double]
runWorkerLoop knobs logEnv runtime queue job counter = do
    deadline <- (+ fromIntegral (lkDurationSeconds knobs)) <$> getMonotonicTime
    latencies <- go deadline []
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
    go :: Double -> [Double] -> IO [Double]
    go deadline acc = do
        nowT <- getMonotonicTime
        if nowT >= deadline
            then pure (reverse acc)
            else do
                enqueue queue job >>= either (\f -> fail ("bench enqueue faulted: " <> show f)) pure
                messages <- receive queue >>= either (\f -> fail ("bench receive faulted: " <> show f)) pure
                t0 <- getMonotonicTime
                runWorkerM logEnv mempty runtime (processBatch messages)
                t1 <- getMonotonicTime
                go deadline ((t1 - t0) : acc)

mirrorJob :: Text -> MirrorJob
mirrorJob url =
    MirrorJob
        { jobPackage = packageName
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = loopbackRegistryUrl url
        , jobArtifactFilename = unsafeFilename (packageText <> "-1.0.0.tgz")
        , jobTraceContext = Nothing
        }

succeedingPublishClient :: IORef Int -> MirrorPublish
succeedingPublishClient counter =
    MirrorPublish
        { mpPublishArtifact = \_ _ _ _ -> do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure (Right ())
        , mpProbeMetadata = const (pure (Right (UnreadStatus 404)))
        }

jobHashes :: LByteString -> NonEmpty Hash
jobHashes bytes = unsafeHash SRI (sriSha512OfLazy bytes) :| [unsafeHash SHA1 (hexSha1OfLazy bytes)]

stubUpstream :: ByteString -> Int -> LByteString -> Application
stubUpstream contentType latency body _request respond = do
    when (latency > 0) (threadDelay latency)
    respond (responseLBS status200 [(hContentType, contentType)] body)

jsonContentType, octetContentType :: ByteString
jsonContentType = "application/json"
octetContentType = "application/octet-stream"

-- | Serve complete metadata captures and only the selected known artifact paths.
corpusPublicStub :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
corpusPublicStub rewritten latency bodies artifacts request respond = do
    when (latency > 0) (threadDelay latency)
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
    when (latency > 0) (threadDelay latency)
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
    when (latency > 0) (threadDelay latency)
    respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

onboardingPublicStub :: Int -> LByteString -> Application
onboardingPublicStub latency bytes request respond = do
    when (latency > 0) (threadDelay latency)
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

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Local npm load scenarios for metadata, artifacts, caches, and the mirror worker.
Private reads stay live. Public requests can share one in-flight fetch even at zero cache TTL.
The harness uses loopback upstreams and the production composition defaults.
-}
module Ecluse.BenchLoad.Npm (
    npmFixture,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, encode, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTime)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, defaultCacheEntries, loadCorpusBodies, longCacheTtl, primeETag, selfHosted, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveHttpHeaders, DriveHttpUrls, DriveInProcess), LoadKnobs (..), Scenario (..), UpstreamFixture (..))

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
import Ecluse.Core.Registry (ParseError (ParseError), RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Publish (MirrorPublish (..))
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
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
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), corpusPackages, cpName, permissiveAgeRules)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (hexSha1OfLazy, sriSha512OfLazy, unsafeFilename, unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (inertRuleDeps)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Wai (localhost, selfBaseUrl)
import Ecluse.Test.Worker (admitAllPolicies)

-- | The npm load fixture covers metadata, artifact relays, cache churn, and mirror jobs.
npmFixture :: UpstreamFixture
npmFixture =
    UpstreamFixture
        { fixtureEcosystem = Npm
        , fixtureScenarios =
            [ mergeScenario
            , cacheHitScenario
            , revalidateScenario
            , cacheFitsScenario
            , cacheEvictsScenario
            , tarballScenario
            , tarballOnboardingScenario
            , tarballCeilingScenario
            , workerScenario
            ]
        }

mergeScenario :: Scenario
mergeScenario =
    Scenario
        { scenarioName = "merge-cold"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET /npm/{pkg} over the weighted corpus with public cache TTL 0. Concurrent public misses share one fetch and decode. Every request reads private metadata, merges, filters, rewrites URLs, and serialises."
        , scenarioBoot = \knobs k -> withNpmProxy knobs 0 defaultCacheEntries serveMix (k . DriveHttpUrls)
        }

cacheHitScenario :: Scenario
cacheHitScenario =
    Scenario
        { scenarioName = "cached-public-hit"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET /npm/{pkg} over the weighted corpus with a warm public metadata cache. Each request still reads and merges private metadata."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl defaultCacheEntries serveMix (k . DriveHttpUrls)
        }

revalidateScenario :: Scenario
revalidateScenario =
    Scenario
        { scenarioName = "revalidate-not-modified"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET the heaviest corpus packument with a primed If-None-Match validator. Each request reads private metadata and computes the plan, then returns 304 without assembly or encoding."
        , scenarioBoot = \knobs k ->
            let pkgs = take 1 (workingSet knobs)
             in withNpmProxy knobs longCacheTtl defaultCacheEntries (uniformMix pkgs) $ \case
                    url : _ -> do
                        etag <- primeETag url
                        k (DriveHttpHeaders [("If-None-Match", etag)] [url])
                    [] -> benchFail "revalidate-not-modified: no URL to drive"
        }

cacheFitsScenario :: Scenario
cacheFitsScenario =
    Scenario
        { scenarioName = "cache-fits-large"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET a uniform corpus working set with a cache that holds every project after warm-up. Compare with cache-evicts-large to measure eviction cost."
        , scenarioBoot = \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (length pkgs) (uniformMix pkgs) (k . DriveHttpUrls)
        }

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    Scenario
        { scenarioName = "cache-evicts-large"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET the same uniform corpus working set with BENCH_LOAD_CACHE_MAX_ENTRIES slots. A bound below the working set forces repeated public fetch, decode, and projection."
        , scenarioBoot = \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (lkCacheMaxEntries knobs) (uniformMix pkgs) (k . DriveHttpUrls)
        }

tarballScenario :: Scenario
tarballScenario =
    Scenario
        { scenarioName = "tarball-hot-path"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET /npm/{pkg}/-/{unscoped-pkg}-9999.0.2.tgz. Resolve the private artifact and stream its bytes from the local upstream."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl defaultCacheEntries tarballMix (k . DriveHttpUrls)
        }

tarballOnboardingScenario :: Scenario
tarballOnboardingScenario =
    Scenario
        { scenarioName = "tarball-onboarding"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "GET /npm/{pkg}/-/{unscoped-pkg}-1.0.0.tgz. A private 404 precedes public admission, artifact streaming, and mirror enqueue. The private probe and public artifact add two sequential upstream waits."
        , scenarioBoot = \knobs k ->
            let latency = lkUpstreamLatencyMicros knobs
                bytes = artifactBytes (lkPayloadBytes knobs)
             in withProxyOverStubs
                    Npm
                    npmDeps
                    knobs
                    longCacheTtl
                    defaultCacheEntries
                    (onboardingPrivateStub latency)
                    (onboardingPublicStub latency bytes)
                    onboardingMix
                    (k . DriveHttpUrls)
        }

tarballCeilingScenario :: Scenario
tarballCeilingScenario =
    Scenario
        { scenarioName = "tarball-ceiling"
        , scenarioConcurrencyScale = 4
        , scenarioDescription =
            "Measure the private artifact relay at four times the base concurrency and 2 ms upstream latency. The payload knob sets the streamed body size."
        , scenarioBoot = \knobs k ->
            withNpmProxy knobs{lkUpstreamLatencyMicros = 2_000} longCacheTtl defaultCacheEntries tarballMix (k . DriveHttpUrls)
        }

withNpmProxy :: LoadKnobs -> NominalDiffTime -> Int -> (Int -> [Text]) -> ([Text] -> IO a) -> IO a
withNpmProxy knobs ttl maxEntries mkMix body = do
    bodies <- loadCorpusBodies corpusPackages
    rewritten <- newIORef mempty
    let bytes = artifactBytes (lkPayloadBytes knobs)
        latency = lkUpstreamLatencyMicros knobs
    withProxyOverStubs
        Npm
        npmDeps
        knobs
        ttl
        maxEntries
        (privateOverlayStub latency bytes)
        (corpusPublicStub rewritten latency bodies)
        mkMix
        body

packageUrl :: Int -> Text -> Text
packageUrl proxyPort name = localhost proxyPort <> "/npm/" <> name

serveMix :: Int -> [Text]
serveMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (packageUrl proxyPort (cpName cp))) corpusPackages

uniformMix :: [CorpusPackage] -> Int -> [Text]
uniformMix pkgs proxyPort = map (packageUrl proxyPort . cpName) pkgs

tarballMix :: Int -> [Text]
tarballMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (localhost proxyPort <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpPackage cp) <> "-9999.0.2.tgz")) corpusPackages

workingSet :: LoadKnobs -> [CorpusPackage]
workingSet knobs = take (max 1 (lkWorkingSet knobs)) corpusPackages

npmDeps :: Int -> Int -> IO PackumentDeps
npmDeps privatePort publicPort = do
    prepared <- prepare inertRuleDeps permissiveAgeRules
    pure
        ( npmServeDeps
            (Just (loopbackRegistryUrl (localhost privatePort)))
            (loopbackRegistryUrl (localhost publicPort))
            (MirrorOnAdmit (loopbackRegistryUrl "https://mirror.bench"))
            prepared
            (pure benchNow)
        )
            { pdMountBaseUrl = "https://bench.proxy"
            , pdEgressUrl = Right . loopbackRegistryUrl
            }

workerScenario :: Scenario
workerScenario =
    Scenario
        { scenarioName = "worker-mirroring"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "Run the mirror worker fetch, integrity check, publish, and acknowledgement loop. The presence probe reports absent, so every job exercises the complete pipeline."
        , scenarioBoot = \knobs k -> do
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
                k (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter))
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
                <> " jobs published -- a harness wiring failure (fetch/verify/publish broke)"
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
        , mpProbeMetadata = const (pure (Right (RegistryResponse 404 "")))
        , mpParseVersionList = const (Left (ParseError "bench mirror: nothing mirrored yet"))
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

corpusPublicStub :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Application
corpusPublicStub rewritten latency bodies request respond = do
    when (latency > 0) (threadDelay latency)
    served <- selfHosted "https://registry.npmjs.org" rewritten (selfBaseUrl request) bodies
    respond $ case requestedPackage request >>= (`Map.lookup` served) of
        Just packument -> responseLBS status200 [(hContentType, jsonContentType)] packument
        Nothing -> responseLBS status404 [(hContentType, jsonContentType)] "{}"

privateOverlayStub :: Int -> LByteString -> Application
privateOverlayStub latency bytes request respond = do
    when (latency > 0) (threadDelay latency)
    let mPkg = requestedPackage request
    case mPkg of
        Just pkg
            | "/-/" `T.isInfixOf` pkg ->
                respond (responseLBS status200 [(hContentType, octetContentType)] bytes)
        Just pkg ->
            respond (responseLBS status200 [(hContentType, jsonContentType)] (encode (privateOverlay (selfBaseUrl request) pkg)))
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
onboardingMix proxyPort =
    [localhost proxyPort <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpPackage cp) <> "-" <> onboardingVersion <> ".tgz" | cp <- corpusPackages]

privateOverlay :: Text -> Text -> Value
privateOverlay authority name =
    packumentValue
        name
        "9999.0.2"
        [(version, overlayVersionObject authority name version) | version <- overlayVersions]
        (("created" .= publishedLongAgo) : [Key.fromText version .= publishedLongAgo | version <- overlayVersions])
        ["_id" .= name]
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

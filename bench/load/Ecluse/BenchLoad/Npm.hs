{- | The npm 'UpstreamFixture' -- the first and (today) only concrete instance of the
ecosystem interface the Layer B load harness drives ("Ecluse.BenchLoad.Harness").

It implements npm's traffic scenarios over stub upstreams and the real composed
'Ecluse.Server.application':

  1. __merge (cold)__ -- a packument @GET@ that fans to both upstreams, merges, gates,
     rewrites, and re-serialises, with the public metadata cache disabled (a zero TTL).
     Every request pays the live private fetch, the merge, the rule sweep, and the
     re-serialise; the public leg's fetch and ~40 ms decode are __single-flight-amortised__
     under concurrency (see below), not paid per request;
  2. __cached-public hit__ -- the same @GET@ with the public origin served from the warm
     metadata cache (no public fetch or decode), the cheap high-throughput path;
  3. __cache-fits-large__ / __cache-evicts-large__ -- a uniform working set of large
     packuments served at a positive TTL, with the cache bound holding the whole set
     (fits, the baseline) versus held below it (evicts, continual eviction and
     re-derivation): the cache-eviction-under-large-datasets comparison;
  4. __worker mirroring__ -- the @fetch → verify → publish → ack@ loop, driven in-process
     (it has no HTTP surface) over a stub artifact upstream.

== The serve mix: a real-world corpus, large-emphasis

The packument scenarios serve the __curated real-world corpus__ of substantial,
many-version packages, not one synthetic payload (trivial few-version packages stress
nothing): the public upstream serves each package's real captured packument by the
requested name (the full Layer A corpus, scoped packages included -- 'requestedPackage'
recovers @\@scope\/name@ from the request path; captured under @bench\/corpus\/npm\/@; see
"Ecluse.Bench.Corpus" and @docs\/architecture\/performance.md@). The @merge-cold@ and
@cached-public-hit@ scenarios drive a __weighted mix__ ('serveMix') with the heavy
many-version packuments as the __primary drivers__ -- a deliberate stress emphasis, not a
traffic-realism model. The two @cache-*-large@ scenarios drive a __uniform__ working set
('uniformMix') so a too-small bound thrashes. The private upstream serves a small disjoint
overlay per package so every request still merges a genuine cross-upstream union. The
worker scenario keeps its synthetic, payload-sized artifact (it mirrors a tarball, not a
packument).

Everything here is npm-specific setup and teardown: the corpus serve mix, the stub
upstreams, the worker's artifact payload, and the proxy wiring (the npm classifier,
renderer, and mount prefix). The harness structure -- the @oha@ driver, the
runtime-statistics capture, and the reporting -- is reused unchanged; a second ecosystem
(PyPI, RubyGems) is a second 'UpstreamFixture' beside this one, not a change to the
harness.

== Cache strategy and the scenarios

The proxy's default @passthrough@ posture caches only the __anonymous public__ origin;
the trusted private origin is the per-client authority and is fetched per request, never
cached (see "Ecluse.Core.Server.Pipeline"). So the packument scenarios differ in the cache
TTL and entry bound: the cold merge uses a zero TTL (cache off); the cached-public hit a
long TTL with a bound holding the whole set; and the two @cache-*-large@ scenarios a long
TTL with the bound at or below the working set, so entries are removed by __eviction__
(not expiry) and re-derived on the next request.

A zero TTL does __not__ mean the public fetch+decode is paid once per request, though.
The public leg resolves through the metadata cache's __single-flight__ path
('Ecluse.Core.Server.Cache.resolveMetadata'): even at a zero TTL, concurrent misses
coalesce onto one in-flight fetch and share the leader's parsed packument, so followers
skip both the fetch and the ~40 ms decode. At the default concurrency the public
fetch+decode is therefore __amortised across followers__, not paid per request -- which
narrows the contrast with the cached-public hit (both amortise the public fetch: one via
the cache, one via single-flight). The cold merge's per-request cost is the live private
leg, the merge, the rule sweep, and the re-serialise. This is real production behaviour;
the scenario does not defeat coalescing.

A literal "private-only cache hit" is not a shape the passthrough model has; the
cached-public hit is its faithful realisation of the issue's cheap, no-public-fetch path.

== Hermeticity

All upstreams are in-process @warp@ stubs on loopback; the proxy and the worker fetch
them over plain (no-TLS, unguarded) managers with @127.0.0.1@ opted into the
internal-range allowance, exactly as the integration suite does -- so the harness opens no
external socket and needs no Docker.
-}
module Ecluse.BenchLoad.Npm (
    npmFixture,
) where

import Control.Concurrent (threadDelay)
import Crypto.Hash qualified as Crypto
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTime)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Driver (DriveHttpUrls, DriveInProcess), LoadKnobs (..), Scenario (..), UpstreamFixture (..))
import Ecluse.Composition (connectionPoolSettings)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Hash, HashAlg (SHA1, SRI), PackageName, mkPackageName)
import Ecluse.Core.Package.Integrity (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Core.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (
        MirrorJob,
        jobArtifact,
        jobArtifactUrl,
        jobMirrorTarget,
        jobPackage,
        jobTraceContext,
        jobVersion
    ),
    MirrorQueue (receive),
    enqueue,
    newInMemoryQueue,
 )
import Ecluse.Core.Registry (ParseError (ParseError), RegistryClient (..))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Registry.Npm.Filter (applyFilterPlan, rewriteTarballUrls)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxEntries, cacheTtl), defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Metadata (newNpmMetadataClient)
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
        wrRegistry,
        wrTracing
    ),
    newWorkerHeartbeat,
    processBatch,
    runWorkerM,
 )
import Ecluse.Env (newEnvWithAdmission)
import Ecluse.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Worker (admitAllPolicies)

-- | The npm load-test fixture: the packument traffic scenarios plus the worker loop.
npmFixture :: UpstreamFixture
npmFixture =
    UpstreamFixture
        { fixtureEcosystem = Npm
        , fixtureScenarios =
            [ mergeScenario
            , cacheHitScenario
            , cacheFitsScenario
            , cacheEvictsScenario
            , workerScenario
            ]
        }

{- | The expensive headline path: a packument @GET@ that fans to both upstreams and
merges, gated, rewritten, and re-serialised. The public metadata cache is disabled (a zero
TTL). Each request pays the live private fetch, the merge, the rule sweep, and the
re-serialise; the public leg's fetch and decode are single-flight-amortised under
concurrency (concurrent misses coalesce onto one in-flight fetch), not paid per request --
so this narrows the contrast with the cached-public hit rather than being a strict
per-request worst case.
-}
mergeScenario :: Scenario
mergeScenario =
    Scenario
        { scenarioName = "merge-cold"
        , scenarioDescription =
            "Public download path with the private + public packument merge in the loop, over a large-emphasis mix drawn from the curated real-world corpus (the heavy many-version packuments are the primary drivers): GET /{pkg} fans to both upstreams -> merge -> rule-filter -> URL-rewrite -> ETag -> re-serialise, with the public metadata cache disabled (TTL 0). The public leg is single-flight, so concurrent misses coalesce onto one in-flight fetch+decode and followers share the leader's parsed packument: the public fetch+decode is amortised under load, not paid per request. Every request still pays the live private fetch, the merge, the rule sweep, and the re-serialise."
        , scenarioBoot = \knobs k -> withNpmProxy knobs 0 (cacheMaxEntries defaultCacheConfig) serveMix (k . DriveHttpUrls)
        }

{- | The cheap, common high-throughput path: the same packument @GET@, but with the
public origin served from a warm metadata cache (a long TTL plus the harness warm-up) and
a bound large enough to hold the whole working set, so the public fetch and decode are
elided and only the live private leg and the cache-served merge run.
-}
cacheHitScenario :: Scenario
cacheHitScenario =
    Scenario
        { scenarioName = "cached-public-hit"
        , scenarioDescription =
            "The cheap cache-served path over the same large-emphasis corpus mix: GET /{pkg} with the anonymous public origin served from the warm metadata cache (no public fetch or decode), the live private leg merged in. The passthrough model caches the public origin, not the per-client private one, so this is the faithful no-public-fetch shape."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl (cacheMaxEntries defaultCacheConfig) serveMix (k . DriveHttpUrls)
        }

{- | The cache-eviction baseline: a working set of several large packuments cycled
__uniformly__ against a cache whose bound __holds the whole set__ (TTL > 0). After warm-up
every entry stays resident, so the public leg is cache-served with no re-derivation -- the
@fits-in-cache@ half of the eviction comparison. Its residency (≈ the whole working set
held at once) and its low alloc-per-request are the baseline the eviction scenario is read
against.
-}
cacheFitsScenario :: Scenario
cacheFitsScenario =
    Scenario
        { scenarioName = "cache-fits-large"
        , scenarioDescription =
            "Cache-eviction baseline (fits): a uniform working set of large packuments served with TTL > 0 and a cache bound that holds the whole set, so after warm-up every entry stays resident and the public leg is cache-served with no re-derivation. Residency reflects the whole working set held at once; alloc/request is the cheap warm-served floor. Read against cache-evicts-large to isolate the eviction cost."
        , scenarioBoot = \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (length pkgs) (uniformMix pkgs) (k . DriveHttpUrls)
        }

{- | The cache-eviction stress: the __same__ uniform working set of large packuments
(TTL > 0), but a cache bound __smaller than the working set__, so the cache cannot hold it
all and continually evicts least-room entries and re-derives them on the next request. It
isolates eviction cost -- throughput and latency under churn, the alloc-per-request of
re-deriving (re-fetch + decode + project) each evicted large packument on its miss, and a
peak residency bounded by @cacheMaxEntries@ large entries plus the transient re-derivation
-- against the @cache-fits-large@ baseline. The bound and working-set size are the
@BENCH_LOAD_CACHE_MAX_ENTRIES@ and @BENCH_LOAD_WORKING_SET@ knobs.
-}
cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    Scenario
        { scenarioName = "cache-evicts-large"
        , scenarioDescription =
            "Cache-eviction stress (exceeds): the same uniform large working set with TTL > 0, but a cache bound smaller than the working set, so entries are continually evicted and re-derived. Isolates the eviction cost against cache-fits-large: throughput/latency under churn, the alloc/request of re-deriving (re-fetch + decode + project) each evicted large packument on a miss, and a peak residency bounded by the cache bound rather than the whole set. Bound = BENCH_LOAD_CACHE_MAX_ENTRIES, working set = BENCH_LOAD_WORKING_SET."
        , scenarioBoot = \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (lkCacheMaxEntries knobs) (uniformMix pkgs) (k . DriveHttpUrls)
        }

-- A cache TTL comfortably longer than any single scenario's warm-up plus measured
-- window, so a warmed public entry is evicted (when the bound is exceeded) rather than
-- expiring, and the cache-fits baseline stays resident for the whole run.
longCacheTtl :: NominalDiffTime
longCacheTtl = 3600

{- | Boot the two packument upstreams over the real-world corpus -- a path-aware public
upstream serving each package's real captured packument by the requested name, and a
path-aware trusted-private upstream serving a small disjoint overlay per package -- and
the real composed proxy over them, with the given metadata-cache TTL and entry bound.
Yields the serve mix the caller builds (a weighted distribution, or a uniform working set)
to the body. All sockets are loopback @warp@ stubs torn down on exit.
-}
withNpmProxy :: LoadKnobs -> NominalDiffTime -> Int -> (Int -> [Text]) -> ([Text] -> IO a) -> IO a
withNpmProxy knobs ttl maxEntries mkMix body = do
    bodies <- loadServeBodies
    testWithApplication (pure (privateOverlayStub latency)) $ \privatePort ->
        testWithApplication (pure (corpusPublicStub latency bodies)) $ \publicPort -> do
            publicManager <- newManager (connectionPoolSettings (lkPublicConnectionsPerHost knobs) defaultManagerSettings)
            privateManager <- newManager (connectionPoolSettings (lkPrivateConnectionsPerHost knobs) defaultManagerSettings)
            admission <- newServeAdmission (lkServeMaxInFlight knobs)
            cache <- newMetadataCache defaultCacheConfig{cacheTtl = ttl, cacheMaxEntries = max 1 maxEntries}
            logEnv <- benchLogEnv
            heartbeat <- newWorkerHeartbeat
            queue <- newInMemoryQueue
            -- The same plain manager serves the private and public legs; the serve path
            -- never touches the publish-side registry handle, so it is the refusing
            -- placeholder.
            env <- newEnvWithAdmission admission refusingRegistry queue publicManager privateManager cache logEnv telemetryDisabled heartbeat
            deps <- npmDeps privatePort publicPort
            let cfg = mkServerConfig [npmMount deps]
            testWithApplication (pure (application cfg env)) $ \proxyPort ->
                body (mkMix proxyPort)
  where
    latency = lkUpstreamLatencyMicros knobs

-- The proxy URL for one corpus package's packument GET.
packageUrl :: Int -> Text -> Text
packageUrl proxyPort name = localhost proxyPort <> "/npm/" <> name

-- The weighted serve mix: each corpus package's proxy URL repeated by its serve weight,
-- so oha (driven via --urls-from-file) spreads requests across the corpus in the
-- large-emphasis proportion 'serveCorpus' encodes (the heavy packuments dominate).
serveMix :: Int -> [Text]
serveMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (packageUrl proxyPort (cpName cp))) serveCorpus

-- A uniform mix over the working set: each package once, so oha cycles them evenly -- the
-- access pattern that thrashes a too-small cache (every package is reused, so an evicted
-- one is requested again and re-derived).
uniformMix :: [CorpusPackage] -> Int -> [Text]
uniformMix pkgs proxyPort = map (packageUrl proxyPort . cpName) pkgs

-- The cache-eviction working set: the leading 'lkWorkingSet' large corpus packages (in
-- 'serveCorpus' order, heaviest first), so a bound below its length forces eviction.
workingSet :: LoadKnobs -> [CorpusPackage]
workingSet knobs = take (max 1 (lkWorkingSet knobs)) serveCorpus

-- The packument-serve dependencies for the npm mount, addressing the two stub ports.
-- 127.0.0.1 is opted into the internal-range allowance for the public leg's honoured
-- tarball gate (the in-process analogue of an operator opting an internal public host
-- in), exactly as the integration suite does.
npmDeps :: Int -> Int -> IO PackumentDeps
npmDeps privatePort publicPort = do
    prepared <- prepare benchPolicy
    pure
        PackumentDeps
            { pdPrivateBaseUrl = localhost privatePort
            , pdPublicBaseUrl = localhost publicPort
            , pdMountBaseUrl = "https://bench.proxy"
            , pdMirrorTarget = "https://mirror.bench"
            , pdRules = prepared
            , pdTarballHostPolicy = SameHostAsPackument
            , pdAllowedInternalHosts = lowerCaseHosts (Set.singleton "127.0.0.1")
            , pdLimits = defaultLimits
            , pdInboundToken = Nothing
            , pdNow = pure benchNow
            , pdHelp = Nothing
            , pdMinIntegrity = defaultMinIntegrity
            , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
            , pdNewMetadataClient = \t p u c f1 f2 f3 l m b s -> newNpmMetadataClient t p u c f1 f2 f3 (NpmClientConfig b m s l)
            , pdBuildArtifactRequestByFile = \_ _ t s -> artifactRequestByFile t s
            , pdBuildArtifactRequestByUrl = \_ _ t s -> artifactRequestByUrl t s
            , pdApplyFilter = applyFilterPlan
            , pdRewriteUrls = rewriteTarballUrls
            }

-- The npm mount binding: the shared npm classifier, renderer, and /npm prefix, carrying
-- the packument-serve dependencies (no publish path).
npmMount :: PackumentDeps -> MountBinding
npmMount deps =
    MountBinding
        { bindingPrefix = "npm" :| []
        , bindingClassifier = Npm.classify
        , bindingPackumentDeps = Just deps
        , bindingPublishDeps = Nothing
        , bindingRenderer = npmRenderer
        }

{- | A permissive rule set: every version old enough to clear a one-day quarantine is
admitted, so the merge and rewrite run over the whole packument rather than
short-circuiting to a denial -- the full serve-path cost the scenario means to measure.
-}
benchPolicy :: [PrecededRule]
benchPolicy = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]

{- | The mirror worker's hot loop: @fetch → verify → publish → ack@, driven in-process
against a stub artifact upstream for the configured duration. The artifact is fetched
over loopback and verified against its real digest each iteration (the per-job work);
the publish goes to a succeeding in-memory client (the publish target is not part of the
hot path being characterised). It has no HTTP surface, so the harness times the loop
directly rather than driving it with @oha@.
-}
workerScenario :: Scenario
workerScenario =
    Scenario
        { scenarioName = "worker-mirroring"
        , scenarioDescription =
            "The mirror worker's fetch -> verify -> publish -> ack loop, driven in-process over a stub artifact upstream: each job fetches the artifact over loopback, recomputes and verifies its integrity digest, and publishes through a succeeding in-memory client."
        , scenarioBoot = \knobs k -> do
            counter <- newIORef (0 :: Int)
            let bytes = artifactBytes (lkPayloadBytes knobs)
                size = fromIntegral (LBS.length bytes)
            testWithApplication (pure (stubUpstream octetContentType (lkUpstreamLatencyMicros knobs) bytes)) $ \artPort -> do
                manager <- newManager defaultManagerSettings
                queue <- newInMemoryQueue
                heartbeat <- newWorkerHeartbeat
                logEnv <- benchLogEnv
                let runtime =
                        WorkerRuntime
                            { wrQueue = queue
                            , wrRegistry = succeedingPublishClient counter
                            , wrManager = manager
                            , wrHeartbeat = heartbeat
                            , wrMetrics = noopWorkerMetricsPort
                            , wrTracing = passthroughWorkerTracingPort
                            , wrInjectTraceContext = id
                            , wrPolicies = admitAllPolicies
                            }
                    artUrl = localhost artPort <> "/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
                    job = mirrorJob artUrl (jobHashes bytes) size
                k (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter))
        }

{- | Run the worker loop for the configured duration, timing each job's
fetch → verify → publish → ack, and return the per-job latencies in seconds. After the
loop, every processed job must have published exactly once -- a shortfall means the
fetch\/verify\/publish wiring broke, a literal harness failure rather than a result.
-}
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
                enqueue queue job
                messages <- receive queue
                t0 <- getMonotonicTime
                runWorkerM logEnv mempty runtime (processBatch messages)
                t1 <- getMonotonicTime
                go deadline ((t1 - t0) : acc)

-- A mirror job for the canned artifact at the given upstream URL, carrying its real
-- integrity digests so the worker's verify gate passes.
mirrorJob :: Text -> NonEmpty Hash -> Int -> MirrorJob
mirrorJob url hashes size =
    MirrorJob
        { jobPackage = packageName
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = url
        , jobMirrorTarget = "https://mirror.bench/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = packageText <> "-1.0.0.tgz"
                , maHashes = hashes
                , maSize = Just size
                }
        , jobTraceContext = Nothing
        }

-- A publish client that records each publish and reports success, for the worker hot
-- loop; its read/parse fields refuse loudly, since the worker only ever publishes.
succeedingPublishClient :: IORef Int -> RegistryClient
succeedingPublishClient counter =
    refusingRegistry
        { publishArtifact = \_ _ _ _ -> do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure (Right ())
        }

-- The canned artifact bytes for the worker scenario: a payload-sized buffer (the verify
-- step hashes the whole body, so its size is the per-job work).
artifactBytes :: Int -> LByteString
artifactBytes size = LBS.replicate (fromIntegral (max 1 size)) 0x61

-- The artifact's real integrity digests (an SRI sha512 and a hex sha1), so the worker's
-- recompute-and-compare verify gate admits exactly these bytes.
jobHashes :: LByteString -> NonEmpty Hash
jobHashes bytes = unsafeHash SRI (sriOf bytes) :| [unsafeHash SHA1 (sha1HexOf bytes)]

-- The sha512 SRI of the given bytes (@sha512-<base64>@).
sriOf :: LByteString -> Text
sriOf bytes = "sha512-" <> decodeUtf8 (convertToBase Base64 (Crypto.hashlazy bytes :: Crypto.Digest Crypto.SHA512) :: ByteString)

-- The lower-cased hex sha1 of the given bytes.
sha1HexOf :: LByteString -> Text
sha1HexOf bytes = decodeUtf8 (convertToBase Base16 (Crypto.hashlazy bytes :: Crypto.Digest Crypto.SHA1) :: ByteString)

-- A stub upstream that injects the configured latency then serves a fixed body -- used by
-- the worker scenario's artifact upstream, which answers any path the same way.
stubUpstream :: ByteString -> Int -> LByteString -> Application
stubUpstream contentType latency body _request respond = do
    when (latency > 0) (threadDelay latency)
    respond (responseLBS status200 [(hContentType, contentType)] body)

jsonContentType, octetContentType :: ByteString
jsonContentType = "application/json"
octetContentType = "application/octet-stream"

-- The public upstream over the corpus: inject the latency, then serve the real captured
-- packument for the requested package (the path's package segment), so each request in
-- the mix decodes, merges, gates, rewrites, and re-serialises a genuinely heterogeneous
-- real document. An unrequested package 404s (the mix only ever asks for corpus ones).
corpusPublicStub :: Int -> Map Text LByteString -> Application
corpusPublicStub latency bodies request respond = do
    when (latency > 0) (threadDelay latency)
    respond $ case requestedPackage request >>= (`Map.lookup` bodies) of
        Just packument -> responseLBS status200 [(hContentType, jsonContentType)] packument
        Nothing -> responseLBS status404 [(hContentType, jsonContentType)] "{}"

-- The trusted-private upstream over the corpus: inject the latency, then serve a small
-- overlay of disjoint versions named for the requested package, so the merge serves a
-- genuine cross-upstream union for every package in the mix rather than the public set
-- alone.
privateOverlayStub :: Int -> Application
privateOverlayStub latency request respond = do
    when (latency > 0) (threadDelay latency)
    respond $ case requestedPackage request of
        Just name -> responseLBS status200 [(hContentType, jsonContentType)] (encode (privateOverlay name))
        Nothing -> responseLBS status404 [(hContentType, jsonContentType)] "{}"

-- The package name a packument GET addresses: the request path segments rejoined with
-- @/@. An unscoped fetch is @{base}/{pkg}@ (one segment); a scoped fetch is
-- @{base}/@scope/name@, which the client may send as one percent-encoded segment or two
-- raw ones -- rejoining recovers @\@scope\/name@ either way. Empty path -> Nothing.
requestedPackage :: Request -> Maybe Text
requestedPackage request = case pathInfo request of
    [] -> Nothing
    segments -> Just (T.intercalate "/" segments)

{- | One package in the Layer B serve mix: the package name (both the request path and
the body's self-reported name, scoped or not), the capture file read at boot, and the
serve weight (its URL's multiplicity in the large-emphasis weighted mix).
-}
data CorpusPackage = CorpusPackage
    { cpName :: Text
    , cpFile :: FilePath
    , cpWeight :: Int
    }

{- | The curated serve mix: the Layer A corpus of substantial, many-version packages
(trivial few-version ones are excluded -- they stress nothing), ordered __heaviest first__
and weighted __large-emphasis__ so the heavy many-version packuments are the primary
drivers of the merge/cache scenarios, not a trivial path. The leading entries are also
the cache-eviction working set ('workingSet'). The pins and captures are shared with
Layer A (@bench\/corpus\/pins.json@, @bench\/corpus\/npm\/*.full.json@); @express@ is
the reused in-place anchor. See @docs\/architecture\/performance.md@.
-}
serveCorpus :: [CorpusPackage]
serveCorpus =
    [ CorpusPackage "@types/node" "bench/corpus/npm/types-node.full.json" 8
    , CorpusPackage "webpack" "bench/corpus/npm/webpack.full.json" 8
    , CorpusPackage "@aws-sdk/client-s3" "bench/corpus/npm/aws-sdk-client-s3.full.json" 6
    , CorpusPackage "express" "core/test/unit/fixtures/npm/express.full.json" 4
    , CorpusPackage "typescript" "bench/corpus/npm/typescript.full.json" 4
    , CorpusPackage "@babel/core" "bench/corpus/npm/babel-core.full.json" 3
    , CorpusPackage "react" "bench/corpus/npm/react.full.json" 2
    , CorpusPackage "request" "bench/corpus/npm/request.full.json" 2
    , CorpusPackage "lodash" "bench/corpus/npm/lodash.full.json" 2
    ]

-- Read each corpus package's captured packument into a name-to-body map at boot, failing
-- loudly on a missing or empty capture (a literal harness failure, not a result).
loadServeBodies :: IO (Map Text LByteString)
loadServeBodies = Map.fromList <$> traverse load serveCorpus
  where
    load cp = do
        packument <- readFileLBS (cpFile cp)
        when (LBS.null packument) (benchFail ("bench-load: corpus capture is empty: " <> toText (cpFile cp)))
        pure (cpName cp, packument)

{- | A small trusted-private overlay for the requested package: three versions disjoint
from any real version, named for that package and old enough to clear the quarantine, so
the merge serves a genuine union for every package in the mix.
-}
privateOverlay :: Text -> Value
privateOverlay name =
    object
        [ "name" .= name
        , "dist-tags" .= object ["latest" .= ("9999.0.2" :: Text)]
        , "versions" .= object [Key.fromText v .= overlayVersionObject name v | v <- overlayVersions]
        , "time" .= object (("created" .= publishedLongAgo) : [Key.fromText v .= publishedLongAgo | v <- overlayVersions])
        , "_id" .= name
        ]
  where
    overlayVersions :: [Text]
    overlayVersions = ["9999.0.0", "9999.0.1", "9999.0.2"]

-- One overlay version manifest, named for the package, with a rewritable dist.tarball and
-- floor-meeting integrity digests so the version is admitted and the serve-time rewrite runs.
overlayVersionObject :: Text -> Text -> Value
overlayVersionObject name version =
    object
        [ "name" .= name
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= ("https://registry.bench/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz")
                , "integrity" .= validSha512Sri
                , "shasum" .= validSha1
                ]
        ]

packageText :: Text
packageText = "bench-pkg"

packageName :: PackageName
packageName = mkPackageName Npm Nothing packageText

localhost :: Int -> Text
localhost port = "http://127.0.0.1:" <> show port

-- A fixed wall clock, so the age-based admission is deterministic across runs.
benchNow :: UTCTime
benchNow = UTCTime (fromGregorian 2026 6 1) 0

-- An ISO-8601 instant 400 days before 'benchNow' -- comfortably past any short
-- quarantine, so every canned version is admitted.
publishedLongAgo :: Text
publishedLongAgo = toText (iso8601Show (addUTCTime (negate (400 * nominalDay)) benchNow))

-- A scribe-less katip log environment: the bench harness wants no log output competing
-- with its machine-readable per-scenario report on stdout.
benchLogEnv :: IO LogEnv
benchLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "bench-load")

-- A registry handle whose every field refuses loudly: the serve path never reads the
-- publish-side handle, and the worker scenario overrides only its publish field.
refusingRegistry :: RegistryClient
refusingRegistry =
    RegistryClient
        { fetchMetadata = const (refuse "fetchMetadata")
        , fetchArtifact = \_ _ -> refuse "fetchArtifact"
        , publishArtifact = \_ _ _ _ -> refuse "publishArtifact"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }
  where
    refuse :: Text -> IO a
    refuse field = benchFail ("bench-load: the serve path must not use the registry handle field " <> field)

-- A static credential provider with a placeholder token: the serve path strips the
-- public-leg credential and forwards the client's to the private leg, so this is unused.

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm 'UpstreamFixture': the first and (today) only concrete instance of the
ecosystem interface the load benchmarks harness drives ("Ecluse.BenchLoad.Harness").

It runs npm's traffic scenarios over stub upstreams and the real composed
'Ecluse.Server.application':

  1. __merge (cold)__: a packument @GET@ that fans to both upstreams, merges, gates,
     rewrites, and re-serialises, with the public metadata cache disabled (a zero TTL).
     Every request pays the live private fetch, the merge, the rule sweep, and the
     re-serialise. The public leg's fetch and ~40 ms decode are
     __single-flight-amortised__ under concurrency (see below), not paid per request.
  2. __cached-public hit__: the same @GET@ with the public origin served from the warm
     metadata cache (no public fetch or decode), the cheap high-throughput path.
  3. __cache-fits-large__ / __cache-evicts-large__: a uniform working set of large
     packuments served at a positive TTL, with the cache bound holding the whole set
     (fits, the baseline) against the bound held below it (evicts, continual eviction
     and re-derivation). That is the cache-eviction-under-large-datasets comparison.
  4. __worker mirroring__: the @fetch → verify → publish → ack@ loop, driven in-process
     (it has no HTTP surface) over a stub artifact upstream.

== The serve mix: a real-world corpus, large-emphasis

The packument scenarios serve the __curated real-world corpus__ of substantial,
many-version packages rather than one synthetic payload, because a trivial few-version
package stresses nothing. The public upstream serves each package's real captured
packument by the requested name: the full work-per-request corpus, scoped packages
included. 'requestedPackage' recovers @\@scope\/name@ from the request path, and the
captures live under @bench\/corpus\/npm\/@ (see "Ecluse.Bench.Corpus" and
@docs\/architecture\/performance.md@).

The @merge-cold@ and @cached-public-hit@ scenarios drive a __weighted mix__ ('serveMix')
with the heavy many-version packuments as the __primary drivers__. That is a deliberate
stress emphasis rather than a traffic-realism model. The two @cache-*-large@ scenarios drive a
__uniform__ working set ('uniformMix') so a too-small bound thrashes. The private upstream
serves a small disjoint overlay per package, so every request still merges a genuine
cross-upstream union. The worker scenario keeps its synthetic, payload-sized artifact,
since it mirrors a tarball rather than a packument.

Everything here is npm-specific setup and teardown: the corpus serve mix, the stub
upstreams, the worker's artifact payload, and the proxy wiring (the npm classifier,
renderer, and mount prefix). Every ecosystem reuses the harness structure unchanged: the
@oha@ driver, the runtime-statistics capture, and the reporting. A second ecosystem (PyPI,
RubyGems) is a second 'UpstreamFixture' beside this one, never a change to the harness.

== Cache strategy and the scenarios

The proxy's default @passthrough@ posture caches only the __anonymous public__ origin. The
trusted private origin is the per-client authority, so the proxy fetches it per request
and never caches it (see "Ecluse.Core.Server.Pipeline"). The packument scenarios therefore
differ in the cache TTL and entry bound. The cold merge uses a zero TTL, so the cache is
off. The cached-public hit uses a long TTL with a bound that holds the whole set. The two
@cache-*-large@ scenarios use a long TTL with the bound at or below the working set.
Entries then leave by __eviction__ rather than by expiry, and the next request re-derives
them.

A zero TTL does __not__ mean every request pays the public fetch and decode. The public
leg resolves through the metadata cache's __single-flight__ path
('Ecluse.Core.Server.Cache.resolveMetadata'). Even at a zero TTL, concurrent misses
coalesce onto one in-flight fetch and share the leader's parsed packument. Followers
therefore skip both the fetch and the ~40 ms decode. At the default concurrency the public fetch and
decode are therefore __amortised across followers__, which narrows the contrast with the
cached-public hit. Both amortise the public fetch, one through the cache and one through
single-flight.

The cold merge's per-request cost is the live private leg, the merge, the rule sweep, and
the re-serialise. This is real production behaviour, and the scenario does not defeat
coalescing. A literal "private-only cache hit" is not a shape the passthrough model has.
The cached-public hit is its faithful realisation of the cheap, no-public-fetch path.

== Hermeticity

All upstreams are in-process @warp@ stubs on loopback, addressed by the @localhost@ DNS
name rather than a bare IP literal. The internal-range block only recognises a literal,
never a name, so the fetch lands with no opt-in. The proxy and the worker fetch the stubs
over plain (no-TLS) managers, exactly as the integration suite does. The harness therefore
opens no external socket and needs no Docker.
-}
module Ecluse.BenchLoad.Npm (
    npmFixture,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, encode, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.HTTP.Types.Header (hETag, hHost)
import Network.Wai (Application, Request, pathInfo, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse (mountBindingFor)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Driver (DriveHttpHeaders, DriveHttpUrls, DriveInProcess), LoadKnobs (..), Scenario (..), UpstreamFixture (..))
import Ecluse.Composition.Sizing (connectionPoolSettings, openFileSoftLimit, resolvePrivateConnections, resolvePublicConnections, resolveServeAdmission)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Hash, HashAlg (SHA1, SRI), PackageName, mkPackageName)
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
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..), newMetadataCache)
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
import Ecluse.Runtime.Env (newEnvWithAdmission)
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (hexSha1OfLazy, sriSha512OfLazy, unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Worker (admitAllPolicies)

-- | The npm load-test fixture: the packument traffic scenarios plus the worker loop.
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

{- | The expensive headline path: a packument @GET@ that fans to both upstreams and
merges, gated, rewritten, and re-serialised. The public metadata cache is off (a zero
TTL). Each request pays the live private fetch, the merge, the rule sweep, and the
re-serialise. The public leg's fetch and decode are single-flight-amortised under
concurrency, since concurrent misses coalesce onto one in-flight fetch. This scenario
therefore narrows the contrast with the cached-public hit rather than standing as a strict
per-request worst case.
-}
mergeScenario :: Scenario
mergeScenario =
    Scenario
        { scenarioName = "merge-cold"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "Public download path with the private + public packument merge in the loop, over a large-emphasis mix drawn from the curated real-world corpus (the heavy many-version packuments are the primary drivers): GET /{pkg} fans to both upstreams -> merge -> rule-filter -> URL-rewrite -> ETag -> re-serialise, with the public metadata cache disabled (TTL 0). The public leg is single-flight, so concurrent misses coalesce onto one in-flight fetch+decode and followers share the leader's parsed packument: the public fetch+decode is amortised under load, not paid per request. Every request still pays the live private fetch, the merge, the rule sweep, and the re-serialise."
        , scenarioBoot = \knobs k -> withNpmProxy knobs 0 defaultCacheEntries serveMix (k . DriveHttpUrls)
        }

{- | The cheap, common high-throughput path: the same packument @GET@ with the public
origin served from a warm metadata cache. A long TTL plus the harness warm-up keeps it
warm, and the bound is large enough to hold the whole working set. The public fetch and
decode are then elided, so only the live private leg and the cache-served merge run.
-}
cacheHitScenario :: Scenario
cacheHitScenario =
    Scenario
        { scenarioName = "cached-public-hit"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "The cheap cache-served path over the same large-emphasis corpus mix: GET /{pkg} with the anonymous public origin served from the warm metadata cache (no public fetch or decode), the live private leg merged in. The passthrough model caches the public origin, not the per-client private one, so this is the faithful no-public-fetch shape."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl defaultCacheEntries serveMix (k . DriveHttpUrls)
        }

{- | The conditional-revalidation path: every request echoes a freshly primed @ETag@ as
@If-None-Match@, so the proxy answers @304@s. That is the dominant metadata pattern for CI
fleets that restore npm's local cache between runs. With the derived validator a @304@
costs the per-request private fetch and the plan, never the document assembly, the encode,
or an output hash. This scenario prices exactly that short-circuit, and catches a
regression that re-attaches heavy work to the conditional path. The priming @GET@ warms
the public cache and captures the tag the drive echoes. Success counts @304@s, because the
harness treats 2xx\/3xx alike.
-}
revalidateScenario :: Scenario
revalidateScenario =
    Scenario
        { scenarioName = "revalidate-not-modified"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "Conditional revalidation of the heaviest corpus packument: a priming GET captures the served ETag, then every driven request echoes it as If-None-Match and is answered 304 off the derived validator -- the private leg still fetched and the plan still computed per request, but no assembly, encode, or output hash. The realistic shape for CI fleets restoring npm's cache: metadata traffic that revalidates instead of re-downloading."
        , scenarioBoot = \knobs k ->
            let pkgs = take 1 (workingSet knobs)
             in withNpmProxy knobs longCacheTtl defaultCacheEntries (uniformMix pkgs) $ \case
                    url : _ -> do
                        etag <- primeETag url
                        k (DriveHttpHeaders [("If-None-Match", etag)] [url])
                    [] -> benchFail "revalidate-not-modified: no URL to drive"
        }

{- Prime the revalidation scenario: one plain @GET@ against the proxy. It warms the
public cache entry, and the drive echoes its response @ETag@. -}
primeETag :: Text -> IO Text
primeETag url = do
    manager <- newManager defaultManagerSettings
    request <- HTTP.parseRequest (toString url)
    response <- HTTP.httpLbs request manager
    case List.lookup hETag (HTTP.responseHeaders response) of
        Just tag -> pure (decodeUtf8 tag)
        Nothing -> benchFail "revalidate-not-modified: the priming GET returned no ETag"

{- | The cache-eviction baseline: a working set of several large packuments cycled
__uniformly__ against a cache whose bound __holds the whole set__ (TTL > 0). After warm-up
every entry stays resident, so the cache serves the public leg with no re-derivation. That
is the @fits-in-cache@ half of the eviction comparison. Its residency (≈ the whole working
set held at once) and its low alloc-per-request are the baseline for reading the eviction
scenario.
-}
cacheFitsScenario :: Scenario
cacheFitsScenario =
    Scenario
        { scenarioName = "cache-fits-large"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "Cache-eviction baseline (fits): a uniform working set of large packuments served with TTL > 0 and a cache bound that holds the whole set, so after warm-up every entry stays resident and the public leg is cache-served with no re-derivation. Residency reflects the whole working set held at once; alloc/request is the cheap warm-served floor. Read against cache-evicts-large to isolate the eviction cost."
        , scenarioBoot = \knobs k ->
            let pkgs = workingSet knobs
             in withNpmProxy knobs longCacheTtl (length pkgs) (uniformMix pkgs) (k . DriveHttpUrls)
        }

{- | The cache-eviction stress: the __same__ uniform working set of large packuments
(TTL > 0), but a cache bound __smaller than the working set__. The cache cannot hold the
whole set, so it continually evicts least-room entries and re-derives them on the next
request. It isolates the eviction cost against the @cache-fits-large@ baseline: throughput
and latency under churn, the alloc-per-request of re-deriving (re-fetch + decode +
project) each evicted large packument on its miss, and a peak residency bounded by the
store's entry bound of large entries plus the transient re-derivation. The bound and the
working-set size are the @BENCH_LOAD_CACHE_MAX_ENTRIES@ and @BENCH_LOAD_WORKING_SET@
knobs.
-}
cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    Scenario
        { scenarioName = "cache-evicts-large"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "Cache-eviction stress (exceeds): the same uniform large working set with TTL > 0, but a cache bound smaller than the working set, so entries are continually evicted and re-derived. Isolates the eviction cost against cache-fits-large: throughput/latency under churn, the alloc/request of re-deriving (re-fetch + decode + project) each evicted large packument on a miss, and a peak residency bounded by the cache bound rather than the whole set. Bound = BENCH_LOAD_CACHE_MAX_ENTRIES, working set = BENCH_LOAD_WORKING_SET."
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
            "Tarball proxy hot path: GET /npm/{pkg}/-/{unscoped-pkg}-9999.0.2.tgz (the scope-dropping npm convention) fans to the packument first to read the dist.tarball URL (instantly served from the metadata cache), then streams the tarball from the artifact upstream."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl defaultCacheEntries tarballMix (k . DriveHttpUrls)
        }

{- | The onboarding (fail-over) regime: every tarball request misses the private
pull-through, because nothing is mirrored yet, and takes the public leg. The steps are a
probe miss, the gate on the one version, the public byte stream, and the mirror-job
enqueue. This is the shape a __new project__ drives before the mirror warms. At steady
state the pull-through serves these reads instead ('tarballScenario'). Its figures price
the onboarding experience, not the steady state. The fleet transits that regime once per
project, per new package, and per new version.
-}
tarballOnboardingScenario :: Scenario
tarballOnboardingScenario =
    Scenario
        { scenarioName = "tarball-onboarding"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "The onboarding fail-over: GET /npm/{pkg}/-/{pkg}-1.0.0.tgz with the private pull-through missing everything (404 after the injected latency, as an unwarmed mirror would) and the public leg serving -- single-version gate, then the artifact streamed from the public origin's self-hosted location, then the mirror-job enqueue. Prices the regime a new project drives until the mirror warms; the steady state is tarball-hot-path. Per-request floor is two sequential upstream round trips (probe miss + artifact) plus the stream."
        , scenarioBoot = \knobs k ->
            let latency = lkUpstreamLatencyMicros knobs
                bytes = artifactBytes (lkPayloadBytes knobs)
             in withProxyOverStubs
                    knobs
                    longCacheTtl
                    defaultCacheEntries
                    (onboardingPrivateStub latency)
                    (onboardingPublicStub latency bytes)
                    onboardingMix
                    (k . DriveHttpUrls)
        }

{- | The streaming-ceiling probe: the same private-hit relay as 'tarballScenario', driven
at __four times the shared concurrency__ against a __2 ms__ stub latency. The binding
constraint is then the proxy's own relay (scheduler, pump, pool, syscalls) rather than the
load generator's connections x RTT. The ordinary tarball scenario is client-bound by
construction, and its throughput of about concurrency / RTT says nothing about the proxy's
limit. This one exists to chase the knee.

At the default base concurrency the scale lands on ~400 concurrent streams, the relay's
measured saturation sweet spot. Past that point, added concurrency measured as pure
backlog and eroding success, never throughput. Read this scenario with its own operating
point in mind: the shared operating-point line prints the unscaled base.
-}
tarballCeilingScenario :: Scenario
tarballCeilingScenario =
    Scenario
        { scenarioName = "tarball-ceiling"
        , scenarioConcurrencyScale = 4
        , scenarioDescription =
            "Streaming-ceiling probe on the private-hit relay: 4x the shared concurrency, 2 ms stub latency (overriding the probed RTT for this scenario alone), same conventional private read as tarball-hot-path. Chases the proxy's own streaming knee -- relay pump, connection handling, syscall pressure -- instead of the client's connections x RTT ceiling. Throughput here x the worker-artifact payload size approximates the relay's byte rate."
        , scenarioBoot = \knobs k ->
            withNpmProxy knobs{lkUpstreamLatencyMicros = 2_000} longCacheTtl defaultCacheEntries tarballMix (k . DriveHttpUrls)
        }

-- A cache TTL comfortably longer than any single scenario's warm-up plus measured
-- window. A warmed public entry then leaves by eviction, when the working set exceeds the
-- bound, rather than by expiry. The cache-fits baseline stays resident for the whole run.
longCacheTtl :: NominalDiffTime
longCacheTtl = 3600

-- The fixture's full-store entry bound, the scenarios' default working-set knob.
defaultCacheEntries :: Int
defaultCacheEntries = sbMaxEntries (cacheFullBudget defaultCacheConfig)

-- The fixture's byte split with the scenario's TTL and entry knob applied to every
-- store: the knob is the axis under test, never the split.
benchCacheConfig :: NominalDiffTime -> Int -> CacheConfig
benchCacheConfig ttl maxEntries =
    defaultCacheConfig
        { cacheTtl = ttl
        , cacheFullBudget = capEntries (cacheFullBudget defaultCacheConfig)
        , cacheVersionBudget = capEntries (cacheVersionBudget defaultCacheConfig)
        , cacheAssembledBudget = capEntries (cacheAssembledBudget defaultCacheConfig)
        }
  where
    capEntries budget = budget{sbMaxEntries = maxEntries}

{- | Boot the two packument upstreams over the real-world corpus, then the real composed
proxy over them, with the given metadata-cache TTL and entry bound. The public upstream is
path-aware and serves each package's real captured packument by the requested name. The
trusted-private upstream is path-aware too and serves a small disjoint overlay per package.
Yields the serve mix the caller builds (a weighted distribution, or a uniform working set)
to the body. All sockets are loopback @warp@ stubs torn down on exit.
-}
withNpmProxy :: LoadKnobs -> NominalDiffTime -> Int -> (Int -> [Text]) -> ([Text] -> IO a) -> IO a
withNpmProxy knobs ttl maxEntries mkMix body = do
    bodies <- loadServeBodies
    let bytes = artifactBytes (lkPayloadBytes knobs)
        latency = lkUpstreamLatencyMicros knobs
    testWithApplication (pure (stubUpstream octetContentType latency bytes)) $ \artPort ->
        withProxyOverStubs
            knobs
            ttl
            maxEntries
            (privateOverlayStub artPort latency bytes)
            (corpusPublicStub latency bodies)
            mkMix
            body

{- | Boot the composed proxy over the __given__ private and public upstream stubs, the
shared shell of every HTTP scenario. The standard fixture ('withNpmProxy') passes the
private overlay and corpus stubs. The onboarding scenario passes a missing-private stub
and a self-hosted public stub. Admission and the private pool resolve through the
composition root's own functions, so an unknobbed run measures the shipped defaults.
-}
withProxyOverStubs :: LoadKnobs -> NominalDiffTime -> Int -> Application -> Application -> (Int -> [Text]) -> ([Text] -> IO a) -> IO a
withProxyOverStubs knobs ttl maxEntries privateApp publicApp mkMix body = do
    capabilities <- getNumCapabilities
    fdLimit <- openFileSoftLimit
    let admissionCapacity = fst (resolveServeAdmission (lkServeMaxInFlight knobs) capabilities)
        privateConnections = fst (resolvePrivateConnections (lkPrivateConnectionsPerHost knobs) fdLimit)
        publicConnections = fst (resolvePublicConnections (lkPublicConnectionsPerHost knobs) fdLimit)
    testWithApplication (pure privateApp) $ \privatePort ->
        testWithApplication (pure publicApp) $ \publicPort -> do
            publicManager <- newManager (connectionPoolSettings publicConnections defaultManagerSettings)
            -- The private pool does not follow the admission capacity. It resolves
            -- through the same function the composition root uses, because a trusted
            -- tarball hit streams outside admission (see resolvePrivateConnections).
            privateManager <- newManager (connectionPoolSettings privateConnections defaultManagerSettings)
            admission <- newServeAdmission admissionCapacity
            cache <- newMetadataCache (benchCacheConfig ttl (max 1 maxEntries))
            logEnv <- benchLogEnv
            heartbeat <- newWorkerHeartbeat
            -- The production memory backend at the shipped default depth cap (50000,
            -- config/default.yaml's queueMemoryMaxDepth). No worker consumes this queue,
            -- so a public-leg tarball scenario (one enqueue per request, ~100 connections
            -- x 30s) can outgrow any fixed cap. Past the cap the backend sheds
            -- drop-newest exactly as production does, and the callback reports the running
            -- total. The backend rate-limits that callback to the first drop and every
            -- 1000th, so a shed is loud in the run log, never silent.
            queue <-
                newBoundedInMemoryQueue
                    (defaultMemoryQueueConfig 50_000)
                    (\n -> putTextLn ("bench serve stack: bounded in-memory mirror queue at cap; running dropped-job total: " <> show n))
            -- The same plain manager serves the private and public legs. The serve path
            -- never touches the publish-side registry handle, so it is the refusing
            -- placeholder.
            env <- newEnvWithAdmission admission queue publicManager privateManager cache logEnv telemetryDisabled heartbeat
            deps <- npmDeps privatePort publicPort
            let cfg = mkServerConfig (maybeToList (mountBindingFor Npm deps Nothing))
            testWithApplication (pure (application cfg env)) $ \proxyPort ->
                body (mkMix proxyPort)

-- The proxy URL for one corpus package's packument GET.
packageUrl :: Int -> Text -> Text
packageUrl proxyPort name = localhost proxyPort <> "/npm/" <> name

-- The weighted serve mix: each corpus package's proxy URL repeated by its serve weight.
-- Driven via --urls-from-file, oha then spreads requests across the corpus in the
-- large-emphasis proportion 'serveCorpus' encodes (the heavy packuments dominate).
serveMix :: Int -> [Text]
serveMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (packageUrl proxyPort (cpName cp))) serveCorpus

-- A uniform mix over the working set: each package once, so oha cycles them evenly. That
-- access pattern thrashes a too-small cache: the drive reuses every package, so it
-- requests an evicted one again and the proxy re-derives it.
uniformMix :: [CorpusPackage] -> Int -> [Text]
uniformMix pkgs proxyPort = map (packageUrl proxyPort . cpName) pkgs

-- The tarball serve mix: each corpus package's tarball URL repeated by its serve weight.
-- A tarball path is /npm/{pkg}/-/{unscoped-pkg}-{version}.tgz (npm convention).
tarballMix :: Int -> [Text]
tarballMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (localhost proxyPort <> "/npm/" <> cpName cp <> "/-/" <> cpUnscopedName cp <> "-9999.0.2.tgz")) serveCorpus

-- The cache-eviction working set: the leading 'lkWorkingSet' large corpus packages (in
-- 'serveCorpus' order, heaviest first), so a bound below its length forces eviction.
workingSet :: LoadKnobs -> [CorpusPackage]
workingSet knobs = take (max 1 (lkWorkingSet knobs)) serveCorpus

-- The packument-serve dependencies for the npm mount. They address the two stub ports by
-- the @localhost@ DNS name rather than a bare IP literal, exactly as the integration suite
-- does. The public leg's honoured tarball gate then never trips the internal-range block,
-- which only recognises a literal.
npmDeps :: Int -> Int -> IO PackumentDeps
npmDeps privatePort publicPort = do
    prepared <- prepare inertRuleDeps benchPolicy
    pure
        (npmServeDeps (Just (localhost privatePort)) (localhost publicPort) (MirrorOnAdmit "https://mirror.bench") prepared (pure benchNow))
            { pdMountBaseUrl = "https://bench.proxy"
            , pdEgressUrl = Right . loopbackRegistryUrl
            }

{- | A permissive rule set: the gate admits every version old enough to clear a one-day
quarantine. The merge and rewrite then run over the whole packument rather than
short-circuiting to a denial, the full serve-path cost the scenario means to measure.
-}
benchPolicy :: [PrecededRule]
benchPolicy = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]

{- | The mirror worker's hot loop: @fetch → verify → publish → ack@, driven in-process
against a stub artifact upstream for the configured duration. Each iteration fetches the
artifact over loopback and verifies it against its real digest, the per-job work. The
publish goes to a succeeding in-memory client, since the publish target is not part of the
hot path this scenario characterises. The mirror-presence probe answers absent through the
same client, so every job measures the full pipeline rather than the dedup short-circuit.
The loop has no HTTP surface, so the harness times it directly rather than driving it with
@oha@.
-}
workerScenario :: Scenario
workerScenario =
    Scenario
        { scenarioName = "worker-mirroring"
        , scenarioConcurrencyScale = 1
        , scenarioDescription =
            "The mirror worker's fetch -> verify -> publish -> ack loop, driven in-process over a stub artifact upstream: each job fetches the artifact over loopback, recomputes and verifies its integrity digest, and publishes through a succeeding in-memory client. The mirror-presence probe answers absent, so every job measures the full pipeline, never the dedup short-circuit."
        , scenarioBoot = \knobs k -> do
            counter <- newIORef (0 :: Int)
            let bytes = artifactBytes (lkPayloadBytes knobs)
            testWithApplication (pure (stubUpstream octetContentType (lkUpstreamLatencyMicros knobs) bytes)) $ \artPort -> do
                manager <- newManager defaultManagerSettings
                -- The production memory backend. The drive loop enqueues one job then
                -- receives it, so at most one job is ever outstanding. A cap of 16 is
                -- comfortably above that maximum. A drop, impossible unless the cadence
                -- breaks, fails the run loudly rather than quietly publishing fewer jobs
                -- than were enqueued.
                queue <-
                    newBoundedInMemoryQueue
                        (defaultMemoryQueueConfig 16)
                        (\n -> benchFail ("worker scenario: the in-memory mirror queue dropped a job (running total " <> show n <> "); the enqueue-receive cadence broke"))
                heartbeat <- newWorkerHeartbeat
                logEnv <- benchLogEnv
                let runtime =
                        WorkerRuntime
                            { wrQueue = queue
                            , wrManager = manager
                            , wrHeartbeat = heartbeat
                            , wrMetrics = noopWorkerMetricsPort
                            , wrTracing = passthroughWorkerTracingPort
                            , wrInjectTraceContext = id
                            , -- The verification digests are the re-admitted artifact's,
                              -- from the injected resolver, so they must be the true
                              -- digests of the stub's bytes for every job to publish. The
                              -- bundle publishes through the recording double.
                              wrPolicies = admitAllPolicies (succeedingPublishClient counter) (jobHashes bytes)
                            }
                    artUrl = localhost artPort <> "/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
                    job = mirrorJob artUrl
                k (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter))
        }

{- | Run the worker loop for the configured duration, timing each job's
fetch → verify → publish → ack, and return the per-job latencies in seconds. After the
loop, every processed job must show exactly one publish. A shortfall means the
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
                enqueue queue job >>= either (\f -> fail ("bench enqueue faulted: " <> show f)) pure
                messages <- receive queue >>= either (\f -> fail ("bench receive faulted: " <> show f)) pure
                t0 <- getMonotonicTime
                runWorkerM logEnv mempty runtime (processBatch messages)
                t1 <- getMonotonicTime
                go deadline ((t1 - t0) : acc)

-- A mirror job for the canned artifact at the given upstream URL. The payload names the
-- artifact by filename only. The digests the worker's verify gate passes on live on the
-- injected policies ('admitAllPolicies').
mirrorJob :: Text -> MirrorJob
mirrorJob url =
    MirrorJob
        { jobPackage = packageName
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = loopbackRegistryUrl url
        , jobArtifactFilename = packageText <> "-1.0.0.tgz"
        , jobTraceContext = Nothing
        }

-- A publish-capability double that records each publish and reports success, for the
-- worker hot loop.
--
-- The mirror-presence probe MUST answer "absent" here. The scenario re-mirrors the same
-- version every iteration, so a probe that confirmed presence would short-circuit every
-- job after the first into a no-op. That would falsely inflate the loop's throughput. An
-- unparseable probe body is the absent posture. A production mirror answers a package it
-- does not hold with an error body no version list parses from. Each job therefore drives
-- the full fetch -> verify -> publish pipeline.
succeedingPublishClient :: IORef Int -> MirrorPublish
succeedingPublishClient counter =
    MirrorPublish
        { mpPublishArtifact = \_ _ _ _ -> do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure (Right ())
        , mpProbeMetadata = const (pure (Right (RegistryResponse "")))
        , mpParseVersionList = const (Left (ParseError "bench mirror: nothing mirrored yet"))
        }

-- The canned artifact bytes for the worker scenario: a payload-sized buffer (the verify
-- step hashes the whole body, so its size is the per-job work).
artifactBytes :: Int -> LByteString
artifactBytes size = LBS.replicate (fromIntegral (max 1 size)) 0x61

-- The artifact's real integrity digests (an SRI sha512 and a hex sha1), so the worker's
-- recompute-and-compare verify gate admits exactly these bytes.
jobHashes :: LByteString -> NonEmpty Hash
jobHashes bytes = unsafeHash SRI (sriSha512OfLazy bytes) :| [unsafeHash SHA1 (hexSha1OfLazy bytes)]

-- A stub upstream that injects the configured latency then serves a fixed body. The
-- worker scenario's artifact upstream uses it, and answers any path the same way.
stubUpstream :: ByteString -> Int -> LByteString -> Application
stubUpstream contentType latency body _request respond = do
    when (latency > 0) (threadDelay latency)
    respond (responseLBS status200 [(hContentType, contentType)] body)

jsonContentType, octetContentType :: ByteString
jsonContentType = "application/json"
octetContentType = "application/octet-stream"

-- The public upstream over the corpus: inject the latency, then serve the real captured
-- packument for the requested package (the path's package segment). Each request in the
-- mix therefore decodes, merges, gates, rewrites, and re-serialises a genuinely
-- heterogeneous real document. An unrequested package 404s, and the mix only ever asks
-- for corpus ones.
corpusPublicStub :: Int -> Map Text LByteString -> Application
corpusPublicStub latency bodies request respond = do
    when (latency > 0) (threadDelay latency)
    respond $ case requestedPackage request >>= (`Map.lookup` bodies) of
        Just packument -> responseLBS status200 [(hContentType, jsonContentType)] packument
        Nothing -> responseLBS status404 [(hContentType, jsonContentType)] "{}"

-- The trusted-private upstream over the corpus: inject the latency, then serve a small
-- overlay of disjoint versions named for the requested package. The merge then serves a
-- genuine cross-upstream union for every package in the mix, never the public set alone.
-- It also serves the canned artifact bytes for any tarball path under the package, so the
-- conventional private-leg read (the hot path) succeeds.
privateOverlayStub :: Int -> Int -> LByteString -> Application
privateOverlayStub artPort latency bytes request respond = do
    when (latency > 0) (threadDelay latency)
    let mPkg = requestedPackage request
    case mPkg of
        -- Artifact request: /{pkg}/-/{file}
        Just pkg
            | "/-/" `T.isInfixOf` pkg ->
                respond (responseLBS status200 [(hContentType, octetContentType)] bytes)
        -- Packument request: /{pkg}
        Just pkg ->
            respond (responseLBS status200 [(hContentType, jsonContentType)] (encode (privateOverlay artPort pkg)))
        Nothing ->
            respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

-- The package name a packument GET addresses: the request path segments rejoined with
-- @/@. An unscoped fetch is @{base}/{pkg}@, one segment. A scoped fetch is
-- @{base}/@scope/name@, which the client may send as one percent-encoded segment or two
-- raw ones, and rejoining recovers @\@scope\/name@ either way. Empty path -> Nothing.
requestedPackage :: Request -> Maybe Text
requestedPackage request = case pathInfo request of
    [] -> Nothing
    segments -> Just (T.intercalate "/" segments)

-- The unscoped (base) name of a corpus package: the name with any @scope/ prefix dropped.
cpUnscopedName :: CorpusPackage -> Text
cpUnscopedName cp = case T.breakOn "/" (cpName cp) of
    (scope, name)
        | "@" `T.isPrefixOf` scope && not (T.null name) -> T.drop 1 name
    _ -> cpName cp

{- | One package in the load benchmarks serve mix: the package name, the capture file read
at boot, and the serve weight. The name is both the request path and the body's
self-reported name, scoped or not. The weight is the URL's multiplicity in the
large-emphasis weighted mix.
-}

{- | The onboarding (fail-over) fixture: the private upstream misses everything, and the
public upstream self-hosts a minimal admissible packument and the artifact bytes.

The private stub answers @404@ to every request __after the injected latency__. During
onboarding the pull-through private registry has nothing mirrored yet. The probe's round
trip is a real cost the scenario must price rather than skip. For any requested package
the public stub serves a one-version packument and the canned artifact bytes for any
tarball path. That packument's @dist.tarball@ points back at the stub's own authority,
read from the request's @Host@ header, so the same-host-as-packument tarball policy holds
with no configuration. The version was published long ago, so the bench policy admits it,
and it carries floor-meeting digests.

The gate's document decode here is deliberately small. The packument scenarios price the
packument-decode cost of the metadata paths. This scenario prices the fail-over
__shape__: probe miss, single-version gate, public stream, mirror enqueue.
-}
onboardingPrivateStub :: Int -> Application
onboardingPrivateStub latency _request respond = do
    when (latency > 0) (threadDelay latency)
    respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

-- The self-hosted public stub of the onboarding fixture (see 'onboardingPrivateStub').
onboardingPublicStub :: Int -> LByteString -> Application
onboardingPublicStub latency bytes request respond = do
    when (latency > 0) (threadDelay latency)
    case requestedPackage request of
        Just pkg
            | "/-/" `T.isInfixOf` pkg ->
                respond (responseLBS status200 [(hContentType, octetContentType)] bytes)
        Just pkg ->
            respond (responseLBS status200 [(hContentType, jsonContentType)] (encode (onboardingPackument (selfAuthority request) pkg)))
        Nothing ->
            respond (responseLBS status404 [(hContentType, jsonContentType)] "{}")

-- The stub's own base URL, recovered from the request's @Host@ header, so the packument's
-- self-referencing @dist.tarball@ carries the authority the proxy fetched from. The port
-- is not knowable before warp binds.
selfAuthority :: Request -> Text
selfAuthority request =
    "http://" <> maybe "localhost" decodeUtf8 (List.lookup hHost (requestHeaders request))

-- A minimal admissible packument: one old version, a self-hosted conventional tarball,
-- and floor-meeting digests. That is enough for the single-version gate to admit and the
-- public stream to fetch, and nothing more.
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
            ( (versionSpec name onboardingVersion (base <> "/" <> name <> "/-/" <> unscopedName name <> "-" <> onboardingVersion <> ".tgz"))
                { vsIntegrity = Just validSha512Sri
                , vsShasum = Just validSha1
                }
            )

onboardingVersion :: Text
onboardingVersion = "1.0.0"

-- The onboarding drive: each corpus package's tarball once, uniformly. A fresh project
-- pulls each dependency once, never by popularity weight.
onboardingMix :: Int -> [Text]
onboardingMix proxyPort =
    [localhost proxyPort <> "/npm/" <> cpName cp <> "/-/" <> unscopedName (cpName cp) <> "-" <> onboardingVersion <> ".tgz" | cp <- serveCorpus]

data CorpusPackage = CorpusPackage
    { cpName :: Text
    , cpFile :: FilePath
    , cpWeight :: Int
    }

{- | The curated serve mix: the work-per-request corpus of substantial, many-version
packages, ordered __heaviest first__ and weighted __large-emphasis__. The corpus leaves
out trivial few-version packages, since they stress nothing. The weighting makes the heavy
many-version packuments the primary drivers of the merge/cache scenarios rather than a
trivial path. The leading entries are also the cache-eviction working set ('workingSet').
The work-per-request micro-benches share these pins and captures
(@bench\/corpus\/pins.json@, @bench\/corpus\/npm\/*.full.json@), and @express@ is the
reused in-place anchor. See @docs\/architecture\/performance.md@.
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

-- Read each corpus package's captured packument into a name-to-body map at boot. A
-- missing or empty capture fails loudly, a literal harness failure rather than a result.
loadServeBodies :: IO (Map Text LByteString)
loadServeBodies = Map.fromList <$> traverse load serveCorpus
  where
    load cp = do
        packument <- readFileLBS (cpFile cp)
        when (LBS.null packument) (benchFail ("bench-load: corpus capture is empty: " <> toText (cpFile cp)))
        pure (cpName cp, packument)

{- | A small trusted-private overlay for the requested package: three versions disjoint
from any real version, named for it and old enough to clear the quarantine. The merge then
serves a genuine union for every package in the mix.
-}
privateOverlay :: Int -> Text -> Value
privateOverlay artPort name =
    packumentValue
        name
        "9999.0.2"
        [(version, overlayVersionObject artPort name version) | version <- overlayVersions]
        (("created" .= publishedLongAgo) : [Key.fromText version .= publishedLongAgo | version <- overlayVersions])
        ["_id" .= name]
  where
    overlayVersions :: [Text]
    overlayVersions = ["9999.0.0", "9999.0.1", "9999.0.2"]

-- One overlay version manifest, named for the package, with a rewritable dist.tarball and
-- floor-meeting integrity digests. The gate therefore admits the version, and the
-- serve-time rewrite runs.
overlayVersionObject :: Int -> Text -> Text -> Value
overlayVersionObject artPort name version =
    versionValue
        ( (versionSpec name version (localhost artPort <> "/" <> name <> "/-/" <> unscoped <> "-" <> version <> ".tgz"))
            { vsIntegrity = Just validSha512Sri
            , vsShasum = Just validSha1
            }
        )
  where
    unscoped = unscopedName name

{- | The npm tarball filename stem for a package: a scoped @\@scope\/name@ drops its
scope, the npm convention for @dist.tarball@ filenames. An unscoped name is itself.
-}
unscopedName :: Text -> Text
unscopedName name = case T.breakOn "/" name of
    (scope, base)
        | "@" `T.isPrefixOf` scope && not (T.null base) -> T.drop 1 base
    _ -> name

packageText :: Text
packageText = "bench-pkg"

packageName :: PackageName
packageName = mkPackageName Npm Nothing packageText

localhost :: Int -> Text
localhost port = "http://localhost:" <> show port

-- A fixed wall clock, so the age-based admission is deterministic across runs.
benchNow :: UTCTime
benchNow = UTCTime (fromGregorian 2026 6 1) 0

-- An ISO-8601 instant 400 days before 'benchNow', comfortably past any short quarantine,
-- so the gate admits every canned version.
publishedLongAgo :: Text
publishedLongAgo = toText (iso8601Show (addUTCTime (negate (400 * nominalDay)) benchNow))

-- A scribe-less katip log environment: the bench harness wants no log output competing
-- with its machine-readable per-scenario report on stdout.
benchLogEnv :: IO LogEnv
benchLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "bench-load")

-- A static credential provider with a placeholder token: the serve path strips the
-- public-leg credential and forwards the client's to the private leg, so this is unused.

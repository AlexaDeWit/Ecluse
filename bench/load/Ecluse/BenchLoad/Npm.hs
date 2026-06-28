{- | The npm 'UpstreamFixture' — the first and (today) only concrete instance of the
ecosystem interface the Layer B load harness drives ("Ecluse.BenchLoad.Harness").

It implements npm's three mandatory traffic scenarios over stub upstreams and the real
composed 'Ecluse.Server.application':

  1. __merge (cold)__ — a packument @GET@ that fans to both upstreams, merges, gates,
     rewrites, and re-serialises, with the public metadata cache disabled (a zero TTL).
     Every request pays the live private fetch, the merge, the rule sweep, and the
     re-serialise; the public leg's fetch and ~40 ms decode are __single-flight-amortised__
     under concurrency (see below), not paid per request;
  2. __cached-public hit__ — the same @GET@ with the public origin served from the warm
     metadata cache (no public fetch or decode), the cheap high-throughput path;
  3. __worker mirroring__ — the @fetch → verify → publish → ack@ loop, driven in-process
     (it has no HTTP surface) over a stub artifact upstream.

== The serve mix: a real-world corpus under a heavy-headed weighting

The two packument scenarios serve the __curated real-world corpus__, not one synthetic
payload: the public upstream serves each package's real captured packument by the
requested name (the unscoped subset of the Layer A corpus, captured under
@bench\/corpus\/npm\/@; see "Ecluse.Bench.Corpus" and
@docs\/architecture\/performance.md@), and the load is driven over a __weighted mix__ —
a few hot small utilities and a long one-shot tail of heavy packuments (a Zipfian
'serveCorpus' weighting) — so the figures reflect a realistic cheap-dominated mix with
rare heavy bursts rather than one shape. The private upstream serves a small disjoint
overlay per package so every request still merges a genuine cross-upstream union. The
worker scenario keeps its synthetic, payload-sized artifact (it mirrors a tarball, not a
packument).

Everything here is npm-specific setup and teardown: the corpus serve mix, the stub
upstreams, the worker's artifact payload, and the proxy wiring (the npm classifier,
renderer, and mount prefix). The harness structure — the @oha@ driver, the
runtime-statistics capture, and the reporting — is reused unchanged; a second ecosystem
(PyPI, RubyGems) is a second 'UpstreamFixture' beside this one, not a change to the
harness.

== Cache strategy and the scenarios

The proxy's default @passthrough@ posture caches only the __anonymous public__ origin;
the trusted private origin is the per-client authority and is fetched per request, never
cached (see "Ecluse.Core.Server.Pipeline"). So the two packument scenarios differ in the
cache TTL: the cold merge uses a zero TTL, while the cached-public hit uses a long TTL and
a warm-up pass.

A zero TTL does __not__ mean the public fetch+decode is paid once per request, though.
The public leg resolves through the metadata cache's __single-flight__ path
('Ecluse.Core.Server.Cache.resolveMetadata'): even at a zero TTL, concurrent misses
coalesce onto one in-flight fetch and share the leader's parsed packument, so followers
skip both the fetch and the ~40 ms decode. At the default concurrency the public
fetch+decode is therefore __amortised across followers__, not paid per request — which
narrows the contrast with the cached-public hit (both amortise the public fetch: one via
the cache, one via single-flight). The cold merge's per-request cost is the live private
leg, the merge, the rule sweep, and the re-serialise. This is real production behaviour;
the scenario does not defeat coalescing.

A literal "private-only cache hit" is not a shape the passthrough model has; the
cached-public hit is its faithful realisation of the issue's cheap, no-public-fetch path.

== Hermeticity

All upstreams are in-process @warp@ stubs on loopback; the proxy and the worker fetch
them over plain (no-TLS, unguarded) managers with @127.0.0.1@ opted into the
internal-range allowance, exactly as the integration suite does — so the harness opens no
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
import Ecluse.Core.Credential (AuthToken (AuthToken, authExpiresAt, authSecret), CredentialProvider, mkSecret, staticProvider)
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
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Core.Server.Cache (CacheConfig (cacheTtl), defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker (
    WorkerRuntime (
        WorkerRuntime,
        wrHeartbeat,
        wrManager,
        wrMetrics,
        wrQueue,
        wrRegistry,
        wrTracing
    ),
    newWorkerHeartbeat,
    processBatch,
    runWorkerM,
 )
import Ecluse.Env (newEnv)
import Ecluse.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (unsafeHash, validSha1, validSha512Sri)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)

-- ── the fixture ──────────────────────────────────────────────────────────────────

-- | The npm load-test fixture: the three mandatory traffic scenarios.
npmFixture :: UpstreamFixture
npmFixture =
    UpstreamFixture
        { fixtureEcosystem = Npm
        , fixtureScenarios =
            [ mergeScenario
            , cacheHitScenario
            , workerScenario
            ]
        }

-- ── packument scenarios ────────────────────────────────────────────────────────

{- | The expensive headline path: a packument @GET@ that fans to both upstreams and
merges, gated, rewritten, and re-serialised. The public metadata cache is disabled (a zero
TTL). Each request pays the live private fetch, the merge, the rule sweep, and the
re-serialise; the public leg's fetch and decode are single-flight-amortised under
concurrency (concurrent misses coalesce onto one in-flight fetch), not paid per request —
so this narrows the contrast with the cached-public hit rather than being a strict
per-request worst case.
-}
mergeScenario :: Scenario
mergeScenario =
    Scenario
        { scenarioName = "merge-cold"
        , scenarioDescription =
            "Public download path with the private + public packument merge in the loop, over a realistic heavy-headed package mix drawn from the curated real-world corpus (a few hot small utilities and a long one-shot tail of heavy packuments): GET /{pkg} fans to both upstreams -> merge -> rule-filter -> URL-rewrite -> ETag -> re-serialise, with the public metadata cache disabled (TTL 0). The public leg is single-flight, so concurrent misses coalesce onto one in-flight fetch+decode and followers share the leader's parsed packument: the public fetch+decode is amortised under load, not paid per request. Every request still pays the live private fetch, the merge, the rule sweep, and the re-serialise."
        , scenarioBoot = \knobs k -> withNpmProxy knobs 0 (k . DriveHttpUrls)
        }

{- | The cheap, common high-throughput path: the same packument @GET@, but with the
public origin served from a warm metadata cache (a long TTL plus the harness warm-up),
so the public fetch and decode are elided and only the live private leg and the
cache-served merge run.
-}
cacheHitScenario :: Scenario
cacheHitScenario =
    Scenario
        { scenarioName = "cached-public-hit"
        , scenarioDescription =
            "The cheap cache-served path over the same heavy-headed corpus mix: GET /{pkg} with the anonymous public origin served from the warm metadata cache (no public fetch or decode), the live private leg merged in. The passthrough model caches the public origin, not the per-client private one, so this is the faithful no-public-fetch shape; under the corpus mix the hot packages are warm-served while the long tail pays the occasional first-touch miss, as in production."
        , scenarioBoot = \knobs k -> withNpmProxy knobs longCacheTtl (k . DriveHttpUrls)
        }

-- A cache TTL comfortably longer than any single scenario's warm-up plus measured
-- window, so a warmed public entry stays resident for the whole run.
longCacheTtl :: NominalDiffTime
longCacheTtl = 3600

{- | Boot the two packument upstreams over the real-world corpus — a path-aware public
upstream serving each package's real captured packument by the requested name, and a
path-aware trusted-private upstream serving a small disjoint overlay per package — and
the real composed proxy over them, with the given metadata-cache TTL. Yields the
__weighted serve mix__ (the corpus URLs, each repeated by its 'cpWeight') to the body,
so the load reflects a realistic heavy-headed (Zipfian) package distribution rather than
one synthetic payload. All sockets are loopback @warp@ stubs torn down on exit.
-}
withNpmProxy :: LoadKnobs -> NominalDiffTime -> ([Text] -> IO a) -> IO a
withNpmProxy knobs ttl body = do
    bodies <- loadServeBodies
    testWithApplication (pure (privateOverlayStub latency)) $ \privatePort ->
        testWithApplication (pure (corpusPublicStub latency bodies)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            cache <- newMetadataCache defaultCacheConfig{cacheTtl = ttl}
            logEnv <- benchLogEnv
            heartbeat <- newWorkerHeartbeat
            queue <- newInMemoryQueue
            -- The same plain manager serves the private and public legs; the serve path
            -- never touches the publish-side registry handle, so it is the refusing
            -- placeholder.
            env <- newEnv refusingRegistry queue refusingCredentials manager manager cache logEnv telemetryDisabled heartbeat
            deps <- npmDeps privatePort publicPort
            let cfg = mkServerConfig [npmMount deps]
            testWithApplication (pure (application cfg env)) $ \proxyPort ->
                body (serveMix proxyPort)
  where
    latency = lkUpstreamLatencyMicros knobs

-- The weighted serve mix: each corpus package's proxy URL repeated by its serve weight,
-- so oha (driven via --urls-from-file) spreads requests across the corpus in the
-- heavy-headed proportion 'serveCorpus' encodes.
serveMix :: Int -> [Text]
serveMix proxyPort =
    concatMap (\cp -> replicate (cpWeight cp) (localhost proxyPort <> "/npm/" <> cpName cp)) serveCorpus

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
short-circuiting to a denial — the full serve-path cost the scenario means to measure.
-}
benchPolicy :: [PrecededRule]
benchPolicy = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]

-- ── worker mirroring scenario ──────────────────────────────────────────────────

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
                            }
                    artUrl = localhost artPort <> "/" <> packageText <> "/-/" <> packageText <> "-1.0.0.tgz"
                    job = mirrorJob artUrl (jobHashes bytes) size
                k (DriveInProcess (runWorkerLoop knobs logEnv runtime queue job counter))
        }

{- | Run the worker loop for the configured duration, timing each job's
fetch → verify → publish → ack, and return the per-job latencies in seconds. After the
loop, every processed job must have published exactly once — a shortfall means the
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
                <> " jobs published — a harness wiring failure (fetch/verify/publish broke)"
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
        { publishArtifact = \_ _ _ -> do
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

-- ── stub upstreams ──────────────────────────────────────────────────────────────

-- A stub upstream that injects the configured latency then serves a fixed body — used by
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

-- The package name a packument GET addresses: the single path segment of the upstream
-- request, since the proxy fetches an unscoped package at @{base}/{pkg}@.
requestedPackage :: Request -> Maybe Text
requestedPackage = listToMaybe . pathInfo

-- ── the corpus serve mix ─────────────────────────────────────────────────────────

{- | One package in the Layer B serve mix: the unscoped package name (both the request
path and the body's self-reported name), the capture file read at boot, and the serve
weight (its URL's multiplicity in the heavy-headed mix).
-}
data CorpusPackage = CorpusPackage
    { cpName :: Text
    , cpFile :: FilePath
    , cpWeight :: Int
    }

{- | The curated serve mix: the unscoped subset of the Layer A corpus (so the loopback
stub's path-to-package mapping is unambiguous, with no percent-encoded scope separator),
weighted heavy-headed (Zipfian). A few hot small utilities head the mix; the heavy
packuments (@typescript@, @webpack@) sit in the one-shot tail at weight 1 — so the served
mix is cheap-dominated with rare heavy bursts, the realistic access pattern, while the
heavy tail is still exercised (a heavy serve spikes the peak-residency high-water mark
when it lands). The pins and captures are shared with Layer A
(@bench\/corpus\/package.json@, @bench\/corpus\/npm\/*.full.json@); @express@ is the
reused in-place anchor. See @docs\/architecture\/performance.md@.
-}
serveCorpus :: [CorpusPackage]
serveCorpus =
    [ CorpusPackage "is-odd" "bench/corpus/npm/is-odd.full.json" 64
    , CorpusPackage "left-pad" "bench/corpus/npm/left-pad.full.json" 16
    , CorpusPackage "lodash" "bench/corpus/npm/lodash.full.json" 8
    , CorpusPackage "request" "bench/corpus/npm/request.full.json" 4
    , CorpusPackage "express" "core/test/unit/fixtures/npm/express.full.json" 3
    , CorpusPackage "react" "bench/corpus/npm/react.full.json" 2
    , CorpusPackage "typescript" "bench/corpus/npm/typescript.full.json" 1
    , CorpusPackage "webpack" "bench/corpus/npm/webpack.full.json" 1
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

-- ── shared values ──────────────────────────────────────────────────────────────

packageText :: Text
packageText = "bench-pkg"

packageName :: PackageName
packageName = mkPackageName Npm Nothing packageText

localhost :: Int -> Text
localhost port = "http://127.0.0.1:" <> show port

-- A fixed wall clock, so the age-based admission is deterministic across runs.
benchNow :: UTCTime
benchNow = UTCTime (fromGregorian 2026 6 1) 0

-- An ISO-8601 instant 400 days before 'benchNow' — comfortably past any short
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
        , publishArtifact = \_ _ _ -> refuse "publishArtifact"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }
  where
    refuse :: Text -> IO a
    refuse field = benchFail ("bench-load: the serve path must not use the registry handle field " <> field)

-- A static credential provider with a placeholder token: the serve path strips the
-- public-leg credential and forwards the client's to the private leg, so this is unused.
refusingCredentials :: CredentialProvider
refusingCredentials = staticProvider AuthToken{authSecret = mkSecret "unused", authExpiresAt = Nothing}

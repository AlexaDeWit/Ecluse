-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PEP 691 index, wheel relay, and cache load scenarios over local upstreams.
PyPI worker-mirroring remains a named gap until #765 supplies the async mirror worker.
-}
module Ecluse.BenchLoad.PyPI (
    pypiFixture,
    pypiLoadNotes,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, pathInfo, responseLBS)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, defaultCacheEntries, fetchChecked, loadCorpusBodies, longCacheTtl, primeETag, selfHosted, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveHttpHeaders, DriveHttpUrls), LoadKnobs (..), Scenario (..), UpstreamFixture (..))
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (..), SimpleIndex (..), simpleIndexMediaType)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Test.Corpus (CorpusPackage (cpWeight), cpName, permissiveAgeRules, pypiCorpusPackages)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Rules (inertRuleDeps)
import Ecluse.Test.Server.Mount (pypiServeDeps)
import Ecluse.Test.Wai (localhost, selfBaseUrl)

-- | The supported PyPI read paths, with no mirror worker or publishing stand-in.
pypiFixture :: UpstreamFixture
pypiFixture =
    UpstreamFixture
        { fixtureEcosystem = PyPI
        , fixtureScenarios =
            [ indexScenario "index-cold" "GET the weighted Simple-index corpus with public cache TTL 0. Each request merges a live private overlay and filters files. Concurrent public misses share an in-flight fetch and decode." 0
            , indexScenario "cached-public-hit" "GET the weighted Simple-index corpus from a warm public metadata cache, with the private index still fetched and merged on each request." longCacheTtl
            , revalidateScenario
            , cacheFitsScenario
            , cacheEvictsScenario
            , wheelScenario PrivateWheel
            , wheelScenario PublicOnboarding
            ]
        }

-- | State the effective eviction size and the limits of the service-time attribution.
pypiLoadNotes :: LoadKnobs -> Text
pypiLoadNotes knobs =
    "PyPI uses the configured injected latency. No live PyPI RTT probe runs.\n\n"
        <> "Service attribution subtracts one public baseline. Extra private or public hops remain in reported overhead, which is not intrinsic proxy CPU time.\n\n"
        <> "Cache working set: "
        <> show count
        <> " projects. Eviction entry bound: "
        <> either id show (evictionEntries (lkCacheMaxEntries knobs) count)
        <> " (configured "
        <> show (lkCacheMaxEntries knobs)
        <> ").\n"
  where
    count = length (workingSet knobs)

indexScenario :: Text -> Text -> NominalDiffTime -> Scenario
indexScenario name description ttl =
    Scenario
        { scenarioName = name
        , scenarioDescription = description
        , scenarioConcurrencyScale = 1
        , scenarioBoot = \knobs k -> withIndexProxy knobs ttl defaultCacheEntries pypiCorpusPackages cpWeight (k . DriveHttpUrls)
        }

revalidateScenario :: Scenario
revalidateScenario =
    Scenario
        { scenarioName = "revalidate-not-modified"
        , scenarioDescription = "GET the heaviest Simple index with a primed If-None-Match validator. The private index and admission plan remain live, but 304 avoids assembly and encoding."
        , scenarioConcurrencyScale = 1
        , scenarioBoot = \knobs k ->
            withIndexProxy knobs longCacheTtl defaultCacheEntries (take 1 pypiCorpusPackages) (const 1) $ \case
                url : _ -> do
                    etag <- primeETag url
                    k (DriveHttpHeaders [("If-None-Match", etag)] [url])
                [] -> benchFail "pypi/revalidate-not-modified: no URL to drive"
        }

cacheFitsScenario :: Scenario
cacheFitsScenario =
    Scenario
        { scenarioName = "cache-fits-large"
        , scenarioDescription = "GET a uniform Simple-index working set with one cache slot per project, so every public entry remains resident after warm-up."
        , scenarioConcurrencyScale = 1
        , scenarioBoot = \knobs k ->
            let packages = workingSet knobs
             in withIndexProxy knobs longCacheTtl (length packages) packages (const 1) (k . DriveHttpUrls)
        }

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    Scenario
        { scenarioName = "cache-evicts-large"
        , scenarioDescription = "GET the same uniform Simple-index working set with min(configured entries, project count - 1) cache slots, forcing repeated public fetch, decode, and projection."
        , scenarioConcurrencyScale = 1
        , scenarioBoot = \knobs k -> do
            let packages = workingSet knobs
            entries <- either benchFail pure (evictionEntries (lkCacheMaxEntries knobs) (length packages))
            withIndexProxy knobs longCacheTtl entries packages (const 1) (k . DriveHttpUrls)
        }

data WheelSource = PrivateWheel | PublicOnboarding

wheelScenario :: WheelSource -> Scenario
wheelScenario source =
    Scenario
        { scenarioName = case source of
            PrivateWheel -> "wheel-hot-path"
            PublicOnboarding -> "wheel-onboarding"
        , scenarioDescription = case source of
            PrivateWheel -> "GET a wheel through the private index and relay its bytes. The private index lookup and artifact stream incur two sequential upstream waits. The public upstream serves nothing."
            PublicOnboarding -> "GET a wheel after a private index 404, public index admission, and public artifact fetch. Public cache TTL 0 preserves admission work under load, with concurrent misses coalesced. No mirror job runs."
        , scenarioConcurrencyScale = 1
        , scenarioBoot = \knobs k -> do
            let bytes = artifactBytes (lkPayloadBytes knobs)
                latency = lkUpstreamLatencyMicros knobs
                (privateApp, publicApp, ttl) = case source of
                    PrivateWheel -> (wheelStub latency bytes, missingStub latency, longCacheTtl)
                    PublicOnboarding -> (missingStub latency, wheelStub latency bytes, 0)
            withProxyOverStubs PyPI pypiDeps knobs ttl defaultCacheEntries privateApp publicApp wheelMix $ \urls -> do
                for_ (zip pypiCorpusPackages urls) $ \(package, url) -> checkWheel bytes (cpName package) url
                k (DriveHttpUrls urls)
        }

workingSet :: LoadKnobs -> [CorpusPackage]
workingSet knobs = take (max 1 (lkWorkingSet knobs)) pypiCorpusPackages

withIndexProxy :: LoadKnobs -> NominalDiffTime -> Int -> [CorpusPackage] -> (CorpusPackage -> Int) -> ([Text] -> IO a) -> IO a
withIndexProxy knobs ttl entries packages weight body = do
    captures <- loadCorpusBodies packages
    rewritten <- newIORef mempty
    let latency = lkUpstreamLatencyMicros knobs
        mix port = concatMap (\package -> replicate (weight package) (indexUrl port (cpName package))) packages
    withProxyOverStubs
        PyPI
        pypiDeps
        knobs
        ttl
        entries
        (wheelStub latency (artifactBytes (lkPayloadBytes knobs)))
        (indexStub rewritten latency captures)
        mix
        ( \urls -> do
            for_ (ordNub urls) $ \url -> do
                index <- checkedIndex url
                let filenames = map ifFilename (siFiles index)
                unless (wheelFilename (siName index) `elem` filenames && length filenames > 1) $
                    benchFail ("pypi index preflight did not merge public files and the private overlay: " <> url)
            body urls
        )

pypiDeps :: Int -> Int -> IO PackumentDeps
pypiDeps privatePort publicPort = do
    prepared <- prepare inertRuleDeps permissiveAgeRules
    pure
        (pypiServeDeps (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite prepared (pure benchNow))
            { pdMountBaseUrl = pypiMountBase
            , pdEgressUrl = Right . loopbackRegistryUrl
            }

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
    threadDelay latency
    bodies <- selfHosted "https://files.pythonhosted.org" rewritten (selfBaseUrl request) captures
    respond $ case requestPath request of
        ["simple", project] | Just bytes <- Map.lookup project bodies -> responseLBS status200 [(hContentType, simpleIndexMediaType)] bytes
        _ -> responseLBS status404 [] ""

wheelStub :: Int -> LByteString -> Application
wheelStub latency bytes = \request respond -> do
    threadDelay latency
    respond $ case requestPath request of
        ["simple", project] | knownProject project -> responseLBS status200 [(hContentType, simpleIndexMediaType)] (encode (wheelIndex (selfBaseUrl request) digest size project))
        ["simple", project, filename] | knownProject project && filename == wheelFilename project -> responseLBS status200 [(hContentType, "application/octet-stream")] bytes
        _ -> responseLBS status404 [] ""
  where
    digest = hexSha256Of (LBS.toStrict bytes)
    size = fromIntegral (LBS.length bytes)
    knownProject project = project `elem` map cpName pypiCorpusPackages

missingStub :: Int -> Application
missingStub latency _ respond = do
    threadDelay latency
    respond (responseLBS status404 [] "")

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

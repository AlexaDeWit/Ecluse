-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load includes named batches, compact specifications, relays and complete platform copies.
module Ecluse.BenchLoad.RubyGems (DependencyWorkload (..), dependencyPackages, RelayShape (..), RelayFixture (..), relayFixture, relayArtifactFixture, rubyGemsFixture, rubyGemsPrivateCopy, rubyGemsPatternProvider, rubyGemsLoadNotes, infoStubWithArtifacts, selectWarmArtifacts) where

import Data.Aeson (withArray, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentLength, hContentType, status200, status404)
import Network.Wai (Application, pathInfo, rawPathInfo, responseLBS)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, delayUpstream, fetchChecked, httpTarget, longCacheTtl, preflightDistinct, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveBeside, DriveBurst, DriveRamp), LoadKnobs (..), Scenario (..), Target, UpstreamFixture, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.Listing (ListingSetup (..), listingLoad, listingWorkingSet, revalidateLoad, slowReaderScenario, withListingStubs)
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..))
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.BenchLoad.RubyGems.Worker (rubyGemsWorkerScenario)
import Ecluse.BenchLoad.RubyGems.WorkerFixture qualified as Worker
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.BenchLoad.SlowReader (slowReaders)
import Ecluse.BenchLoad.Stress (WarmLoadDriver (..), herdSize, pairedLoad, rampSteps)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageDetails (pkgArtifacts), PackageInfo (infoVersions), artFilename)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), cpName, readCorpusPins, rubyGemsCaptureUpstream, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Subset (newestRubyGemsShare)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.JsonStream (sliceChunks)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo, runRubyReference)
import Ecluse.Test.Wai (localhost, pacedBody)

-- | Native scenarios satisfy the ecosystem's full read, artifact and worker obligations.
rubyGemsFixture :: UpstreamFixture
rubyGemsFixture = readLoadContract nativeDriver [batchScenario, quickScenario, artifactScenarioWith False KnownLengthRelay, artifactScenarioWith False SlowChunkedRelay, rubyGemsWorkerScenario]

nativeDriver :: LoadReadDriver
nativeDriver =
    LoadReadDriver
        { lrdPrivateCopy = rubyGemsPrivateCopy
        , lrdColdMetadata = scenario "dependencies-cold" "Read named dependency records with public TTL 0, live private overlays and shared concurrent public reads." (listingLoad nativeListing 0)
        , lrdSlowReaders = slowReaderScenario nativeListing "dependencies-cold"
        , lrdAssembledHit = dependencyScenario RepeatedCorpus
        , lrdRevalidate = scenario "revalidate-not-modified" "Read a primed native dependency reply with its conditional validator." (revalidateLoad nativeListing)
        , lrdCacheFits = dependencyScenario OneProject
        , lrdCacheEvicts = dependencyScenario ManyProjects
        , lrdPrivateArtifact = artifactScenario True
        , lrdPublicArtifact = artifactScenario False
        , lrdArtifactCeiling = (artifactScenario True){scenarioName = "gem-ceiling", scenarioConcurrencyScale = 4, scenarioUpstreamLatencyMicros = Just 2_000, scenarioPublicLegUnavailableReason = Just "the fixture uses a fixed 2 ms upstream delay instead of the run's native RTT baseline"}
        , lrdHerd =
            ( scenario
                "herd"
                "Send simultaneous cold named-gem requests to an idle proxy without warm-up."
                ( \knobs use -> withNativeListings knobs (\port -> [listingUrl port "rails"]) $ \proxy -> \case
                    url : _ -> use (proxied proxy (DriveBurst herdSize url))
                    [] -> benchFail "rubygems/herd: no URL to drive"
                )
            )
                { scenarioServiceTime = False
                }
        , lrdWarmUnderCold = (scenario "warm-under-cold" "Read warm named metadata and artifacts while cold requests change an opaque public header." warmUnderCold){scenarioServiceTime = False}
        , lrdRamp = (scenario "ramp" "Read the weighted native corpus at each configured concurrency step with public TTL 0." (\knobs use -> withNativeListings knobs (weightedMix cpWeight listingUrl rubyGemsCorpusPackages) (\proxy urls -> use (proxied proxy (DriveRamp rampSteps (urlLoad urls)))))){scenarioServiceTime = False}
        , lrdPatternProvider = rubyGemsPatternProvider
        , lrdPatternPrivate = lsPrivate nativeListing
        , lrdPatternPublic = \knobs captures artifacts -> pure (infoStubWithArtifacts (lkUpstreamLatencyMicros knobs) captures artifacts)
        }

-- | Replay roots exclude the listing query so native artifact paths share the same mount.
rubyGemsPatternProvider :: PatternProvider
rubyGemsPatternProvider =
    PatternProvider
        { ppCaptureAuthority = "https://rubygems.org"
        , ppRootUrl = \port -> localhost port <> "/rubygems/"
        , ppListingSuffix = ("api/v1/dependencies?gems=" <>)
        , ppDiscoveryPath = Nothing
        , ppCapturedRead = Right . gemReadWith (const pass) (const pass)
        , ppSelectedArtifacts = \selected packages captures -> case selected of
            Nothing -> pure Map.empty
            Just "pinned" -> selectWarmArtifacts packages captures
            Just _ -> benchFail "RubyGems selected replay requires pinned"
        }

nativeListing :: ListingSetup
nativeListing =
    ListingSetup
        { lsCopy = rubyGemsPrivateCopy
        , lsPrivate = \knobs -> gemStub (lkUpstreamLatencyMicros knobs) (artifactBytes (lkPayloadBytes knobs))
        , lsCapturePackages = id
        , lsPreflight = preflightListings
        , lsEvictionEntries = evictionEntries . lkCacheMaxEntries
        , lsRevalidateError = "rubygems/revalidate-not-modified: no URL to drive"
        }

-- | Private cuts retain complete native platform groups and original registry timestamps.
rubyGemsPrivateCopy :: PrivateCopy
rubyGemsPrivateCopy =
    PrivateCopy
        { pcEcosystem = RubyGems
        , pcListing = "GET the weighted named-gem dependency corpus"
        , pcRegistry = "a private registry holding native named-gem metadata"
        , pcPackages = rubyGemsCorpusPackages
        , pcCut = newestRubyGemsShare
        , pcStub = \latency captures -> pure (infoStub latency copyBytes captures)
        , pcUrl = listingUrl
        , pcPreflight = preflightListings
        , pcHerdPackage = "rails"
        }

-- | Relay bodies replace only checksum fields. Native worker archives remain independent genuine packages.
rubyGemsLoadNotes :: LoadKnobs -> Text
rubyGemsLoadNotes knobs =
    "HTTP relay fixtures replace captured checksums with hashes of owned payloads. Relay bytes do not prove client installation. Native worker work copies four genuine platform archives, each with "
        <> show (lkPayloadBytes knobs)
        <> " uncompressed payload bytes. Quick-spec work returns original Ruby-generated bytes.\n\n"
        <> "Native dependency working sets: repeated corpus "
        <> count RepeatedCorpus
        <> ", one project "
        <> count OneProject
        <> ", many projects "
        <> count ManyProjects
        <> ". Replies are rebuilt per request. Full-store misses and collapsed reads describe public fetches and their waiting consumers. They do not count private reads or exact per-name upstream requests."
  where
    count workload = either ("unavailable: " <>) (show . length) (dependencyPackages workload knobs)

-- | Named dependency workloads vary corpus cardinality without claiming assembled-response retention.
data DependencyWorkload = RepeatedCorpus | OneProject | ManyProjects
    deriving stock (Eq, Show)

-- | The many-project comparison requires at least two selected captures before proxy boot.
dependencyPackages :: DependencyWorkload -> LoadKnobs -> Either Text [CorpusPackage]
dependencyPackages workload knobs = case workload of
    RepeatedCorpus -> Right rubyGemsCorpusPackages
    OneProject -> Right (take 1 rubyGemsCorpusPackages)
    ManyProjects ->
        let packages = listingWorkingSet nativeListing knobs
         in if length packages < 2
                then Left "dependencies-working-set-many requires at least two corpus projects"
                else Right packages

dependencyScenario :: DependencyWorkload -> Scenario
dependencyScenario workload = scenario name description $ \knobs use -> do
    packages <- either benchFail pure (dependencyPackages workload knobs)
    withListingStubs nativeListing knobs longCacheTtl Nothing packages (weightedMix weight listingUrl packages) $ \proxy urls ->
        preflightDistinct (preflightListings . pure) urls (httpTarget use proxy urls)
  where
    weight = if workload == RepeatedCorpus then cpWeight else const 1
    (name, description) = case workload of
        RepeatedCorpus -> ("dependencies-repeated-corpus", "Read the weighted three-capture native corpus. Each request admits source documents and encodes its named dependency reply.")
        OneProject -> ("dependencies-working-set-one", "Read one captured native history repeatedly, with a fresh named dependency reply per request.")
        ManyProjects -> ("dependencies-working-set-many", "Read the configured native corpus working set uniformly, with at least two histories and a fresh dependency reply per request.")

withNativeListings :: LoadKnobs -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withNativeListings knobs = withListingStubs nativeListing knobs 0 Nothing rubyGemsCorpusPackages

listingUrl :: Int -> Text -> Text
listingUrl port name = localhost port <> "/rubygems/api/v1/dependencies?gems=" <> name

-- | Native fixture replies retain ordinary, declared-length or paced chunked transport.
data RelayShape = OrdinaryRelay | KnownLengthRelay | SlowChunkedRelay
    deriving stock (Eq)

-- | A relay workload binds native targets to its private and public source applications.
data RelayFixture = RelayFixture
    { rfPrivate :: Application
    , rfPublic :: Application
    , rfCacheTtlSeconds :: Int
    , rfTargets :: Int -> [Text]
    }

-- | Source selection changes the stub pair. Both paths use the same mounted native artifact route.
relayFixture :: Bool -> RelayShape -> LoadKnobs -> RelayFixture
relayFixture privateHit shape knobs =
    RelayFixture
        { rfPrivate = if privateHit then actual else absent
        , rfPublic = if privateHit then absent else actual
        , rfCacheTtlSeconds = if privateHit then longCacheTtl else 0
        , rfTargets = \port -> [localhost port <> "/rubygems/gems/" <> cpName package <> "-9999.0.2.gem" | package <- rubyGemsCorpusPackages]
        }
  where
    latency = lkUpstreamLatencyMicros knobs
    actual = relayArtifactFixture shape latency (artifactBytes (lkPayloadBytes knobs))
    absent = missingStub latency

artifactScenario :: Bool -> Scenario
artifactScenario privateHit = artifactScenarioWith privateHit OrdinaryRelay

artifactScenarioWith :: Bool -> RelayShape -> Scenario
artifactScenarioWith privateHit shape =
    (scenario name description boot){scenarioVerifiedArtifactMetrics = True, scenarioServiceTime = shape /= SlowChunkedRelay}
  where
    boot knobs use = do
        let bytes = artifactBytes (lkPayloadBytes knobs)
            fixture = relayFixture privateHit shape knobs
        withProxyOverStubs RubyGems knobs (rfCacheTtlSeconds fixture) Nothing (rfPrivate fixture) (rfPublic fixture) (rfTargets fixture) $ \proxy urls -> do
            for_ urls $ \url -> do
                response <- fetchChecked status200 [] url
                unless (HTTP.responseBody response == bytes) (benchFail "RubyGems relay bytes differ from the independently owned payload")
            if shape == SlowChunkedRelay
                then use (proxied proxy (DriveBeside (urlLoad urls) (slowReaders (lkConcurrency knobs) urls)))
                else httpTarget use proxy urls
    name = case shape of
        KnownLengthRelay -> "gem-verified-known-length"
        SlowChunkedRelay -> "gem-verified-slow-readers"
        OrdinaryRelay -> if privateHit then "gem-hot-path" else "gem-onboarding"
    description = case shape of
        KnownLengthRelay -> "Verify a public native artifact before replying, with an explicit upstream Content-Length. Record complete-body first-byte latency, actual read/hash bytes and admission pressure."
        SlowChunkedRelay -> "Verify a public native artifact from a paced chunked origin while equal extra clients read replies slowly. Each 64 KiB source chunk waits its share of one configured upstream delay. Retain reservation and body through client delivery."
        OrdinaryRelay -> if privateHit then "Relay a private native artifact with caller-authorised metadata and exact digest verification." else "Admit a public native artifact after private absence, then relay its digest-bound bytes without mirroring."

-- | Use the same native fixture and framing in direct controls and the timed proxy load.
relayArtifactFixture :: RelayShape -> Int -> LByteString -> Application
relayArtifactFixture shape latency = gemStubWith deliver latency
  where
    deliver bytes = case shape of
        OrdinaryRelay -> \_ respond -> respond (responseLBS status200 [] bytes)
        KnownLengthRelay -> \_ respond -> respond (responseLBS status200 [(hContentLength, encodeUtf8 (show (LBS.length bytes) :: Text))] bytes)
        SlowChunkedRelay ->
            let chunks = sliceChunks (64 * 1024) (toStrict bytes)
                pause = latency `div` max 1 (length chunks)
             in pacedBody [(pause, chunk) | chunk <- chunks]

batchScenario :: Scenario
batchScenario = scenario "batch-dependencies" "Assemble all three named-gem histories in one bounded native Dependencies API reply." $ \knobs use ->
    withNativeListings knobs (\port -> [listingUrl port (T.intercalate "," (map cpName rubyGemsCorpusPackages))]) $ \proxy urls -> do
        preflightListings urls
        httpTarget use proxy urls

quickScenario :: Scenario
quickScenario = scenario "quick-gemspec" "Admit an exact native platform coordinate and relay its original Ruby-generated zlib compact specification." $ \knobs use -> do
    fixture <- Worker.newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
        urls port = [localhost port <> "/rubygems/" <> path | path <- Map.keys (Worker.wfSpecs fixture)]
    withProxyOverStubs RubyGems knobs 0 Nothing (missingStub latency) (Worker.sourceApp latency fixture) urls $ \proxy targets -> do
        for_ (zip (Map.toAscList (Worker.wfSpecs fixture)) targets) $ \((_, original), target) -> do
            response <- fetchChecked status200 [] target
            unless (HTTP.responseBody response == fromStrict original) (benchFail "RubyGems compact specification bytes differ")
            void (runRubyReference "compact" (toStrict (HTTP.responseBody response)))
        httpTarget use proxy targets

warmUnderCold :: LoadKnobs -> (Target -> IO a) -> IO a
warmUnderCold knobs use = do
    revisions <- newIORef (0 :: Int)
    pairedLoad
        WarmLoadDriver
            { wldEcosystem = RubyGems
            , wldPackages = rubyGemsCorpusPackages
            , wldListingUrl = listingUrl
            , wldArtifactUrl = \port artifact -> localhost port <> "/rubygems/" <> saProxyPath artifact
            , wldSelect = selectWarmArtifacts
            , wldPrivate = const gemStub
            , wldPublic = changingInfoStub revisions
            }
        knobs
        use

changingInfoStub :: IORef Int -> IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
changingInfoStub revisions _ latency captures artifacts =
    let fallback = infoStubWithArtifacts latency captures artifacts
        heavy = Map.lookup "rails" captures <&> relayBody (fromMaybe copyBytes (listToMaybe (Map.elems artifacts)))
     in \request respond -> case pathInfo request of
            ["info", "rails"] | Just captured <- heavy -> do
                body <- either benchFail pure captured
                revision <- atomicModifyIORef' revisions (\old -> (old + 1, old))
                let varied = "ecluse_benchmark_revision:" <> encodeUtf8 (show revision :: Text) <> "\n" <> body
                delayUpstream latency
                respond (responseLBS status200 [(hContentType, "text/plain")] varied)
            _ -> fallback request respond

-- | Workload pins resolve through the production full reader before any timed request starts.
selectWarmArtifacts :: [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
selectWarmArtifacts packages captures = do
    pins <- readCorpusPins (.: "rubygemsPins") >>= either (benchFail . toText) pure
    fmap Map.fromList $ forM packages $ \package -> do
        let name = cpName package
        version <- maybe (benchFail ("missing RubyGems workload pin: " <> name)) pure (Map.lookup name pins)
        bytes <- maybe (benchFail ("missing RubyGems native capture: " <> name)) pure (Map.lookup name captures)
        manifest <- captureManifest (gemReadWith (const pass) (const pass) "https://rubygems.org") rubyGemsCaptureUpstream (cpPackage package) [toStrict bytes] >>= either (benchFail . show) pure
        details <- maybe (benchFail ("RubyGems pin is absent from the native capture: " <> name <> "@" <> version)) pure (Map.lookup version (infoVersions (manifestInfo manifest)))
        let filename = artFilename (NE.head (pkgArtifacts details))
        pure (name, SelectedArtifact ("gems/" <> filename) ("https://rubygems.org/gems/" <> filename))

infoStub :: Int -> LByteString -> Map Text LByteString -> Application
infoStub latency payload captures =
    let bodies = Map.map (relayBody payload) captures
        parsed = traverse (referenceInfo . toStrict) captures
        files = Set.fromList . concatMap (\(name, rows) -> map (filename name) rows) . Map.toList <$> parsed
     in \request respond -> do
            delayUpstream latency
            capturedFiles <- either benchFail pure files
            case pathInfo request of
                ["info", name] | Just body <- Map.lookup name bodies -> do
                    bytes <- either benchFail pure body
                    respond (responseLBS status200 [(hContentType, "text/plain")] bytes)
                ["gems", file] | file `Set.member` capturedFiles -> respond (responseLBS status200 [] payload)
                _ -> respond (responseLBS status404 [] "missing")
  where
    filename name row = name <> "-" <> rowVersion row <> (if rowPlatform row == "ruby" then "" else "-" <> rowPlatform row) <> ".gem"

-- | Selected replays serve the exact requested owned payload under its native filename.
infoStubWithArtifacts :: Int -> Map Text LByteString -> Map ByteString LByteString -> Application
infoStubWithArtifacts latency captures artifacts request respond = case Map.lookup (rawPathInfo request) artifacts of
    Just bytes -> delayUpstream latency >> respond (responseLBS status200 [] bytes)
    Nothing -> infoStub latency (fromMaybe copyBytes (listToMaybe (Map.elems artifacts))) captures request respond

gemStub :: Int -> LByteString -> Application
gemStub = relayArtifactFixture OrdinaryRelay

gemStubWith :: (LByteString -> Application) -> Int -> LByteString -> Application
gemStubWith deliver latency bytes request respond = do
    delayUpstream latency
    case pathInfo request of
        ["info", name] | name `elem` map cpName rubyGemsCorpusPackages -> respond (responseLBS status200 [(hContentType, "text/plain")] ("---\n9999.0.2 |checksum:" <> encodeUtf8 (hexSha256Of (toStrict bytes)) <> ",created_at:2020-01-01T00:00:00Z\n"))
        ["gems", filename] | filename `elem` [cpName package <> "-9999.0.2.gem" | package <- rubyGemsCorpusPackages] -> deliver bytes request respond
        _ -> respond (responseLBS status404 [] "missing")

missingStub :: Int -> Application
missingStub latency _ respond = delayUpstream latency >> respond (responseLBS status404 [] "missing")

relayBody :: LByteString -> LByteString -> Either Text LByteString
relayBody payload original = do
    rows <- referenceInfo (toStrict original)
    checksums <- fmap Map.fromList $ forM rows $ \row -> do
        previous <- maybeToRight "RubyGems relay row has no checksum" (lookup "checksum" (rowFields row))
        pure (rowPosition row, previous)
    let digest = hexSha256Of (toStrict payload)
    fromStrict . BS.intercalate "\n" <$> traverse (replace checksums digest) (zip [0 ..] (BS.split 10 (toStrict original)))
  where
    replace checksums digest (position, bytes) = case Map.lookup position checksums of
        Nothing -> Right bytes
        Just previous -> do
            let before = "checksum:" <> previous
                after = "checksum:" <> digest
                raw = decodeUtf8 bytes
            unless (before `T.isInfixOf` raw) (Left "RubyGems relay row has no exact checksum field")
            let result = encodeUtf8 (T.replace before after raw)
            unless (BS.length result == BS.length bytes) (Left "RubyGems relay fixture changed its native wire width")
            pure result

preflightListings :: [Text] -> IO ()
preflightListings = traverse_ checked . ordNub
  where
    checked url = do
        response <- fetchChecked status200 [] url
        value <- runRubyReference "dependencies" (toStrict (HTTP.responseBody response))
        rows <- either (benchFail . toText) pure (parseEither (withArray "native dependency rows" (traverse row . toList)) value)
        let requested = T.splitOn "," (snd (T.breakOnEnd "?gems=" url))
        unless (not (null rows) && sort (ordNub (map fst rows)) == sort requested && not (any (T.null . snd) rows)) (benchFail "RubyGems dependency preflight lost a named package or native version")
    row = withObject "native dependency row" $ \fields -> do
        name <- fields .: "name"
        number <- fields .: "number"
        _ <- fields .: "platform" :: Parser Text
        _ <- fields .: "dependencies" :: Parser [[Text]]
        pure (name, number)

copyBytes :: LByteString
copyBytes = "Owned RubyGems native listing relay bytes"

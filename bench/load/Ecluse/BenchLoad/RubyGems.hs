-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load includes per-gem info, directory discovery, compact specifications and platform copies.
module Ecluse.BenchLoad.RubyGems (InfoWorkload (..), infoPackages, RelayShape (..), RelayFixture (..), relayFixture, relayArtifactFixture, rubyGemsFixture, rubyGemsPrivateCopy, rubyGemsPatternProvider, rubyGemsLoadNotes, infoStubWithArtifacts, selectWarmArtifacts) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentLength, hContentType, status200, status404)
import Network.Wai (Application, pathInfo, rawPathInfo, responseLBS)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..), selectedArtifact)
import Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (absentUpstream, artifactBytes, delayUpstream, fetchChecked, httpTarget, longCacheTtl, preflightDistinct, rewriteLines, selectPinnedArtifacts, swapChecksumPreservingWidth, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveBurst, DriveRamp), LoadKnobs (..), Scenario (..), Target, UpstreamFixture, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.Listing (ListingSetup (..), listingLoad, listingWorkingSet, revalidateLoad, slowReaderScenario, withListingStubs)
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..))
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.BenchLoad.RubyGems.Directory (rubyGemsDirectoryScenario)
import Ecluse.BenchLoad.RubyGems.Worker (rubyGemsWorkerScenario)
import Ecluse.BenchLoad.RubyGems.WorkerFixture qualified as Worker
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.BenchLoad.Stress (WarmLoadDriver (..), herdSize, pairedLoad, rampSteps)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), cpName, rubyGemsCaptureUpstream, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Subset (newestRubyGemsShare)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.JsonStream (sliceChunks)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo, runRubyReference)
import Ecluse.Test.Wai (localhost, pacedBody)

-- | Native scenarios satisfy the ecosystem's full read, artifact and worker obligations.
rubyGemsFixture :: UpstreamFixture
rubyGemsFixture = readLoadContract nativeDriver [quickScenario, rubyGemsDirectoryScenario, rubyGemsWorkerScenario]

nativeDriver :: LoadReadDriver
nativeDriver =
    LoadReadDriver
        { lrdPrivateCopy = rubyGemsPrivateCopy
        , lrdColdMetadata = scenario "info-cold" "Read per-gem Compact Index records with public TTL 0, live private overlays and shared concurrent public reads." (listingLoad nativeListing 0)
        , lrdSlowReaders = slowReaderScenario nativeListing "info-cold"
        , lrdAssembledHit = infoScenario RepeatedCorpus
        , lrdRevalidate = scenario "revalidate-not-modified" "Read primed per-gem Compact Index records with their conditional validator." (revalidateLoad nativeListing)
        , lrdCacheFits = infoScenario OneProject
        , lrdCacheEvicts = infoScenario ManyProjects
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

-- | Per-gem diagnostic replays share one mount for info and artifact paths. They exclude discovery.
rubyGemsPatternProvider :: PatternProvider
rubyGemsPatternProvider =
    PatternProvider
        { ppCaptureAuthority = "https://rubygems.org"
        , ppRootUrl = \port -> localhost port <> "/rubygems/"
        , ppListingSuffix = ("info/" <>)
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
        , pcListing = "GET the weighted per-gem Compact Index corpus"
        , pcRegistry = "a private registry holding native per-gem metadata"
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
        <> "Native info working sets: repeated corpus "
        <> count RepeatedCorpus
        <> ", one project "
        <> count OneProject
        <> ", many projects "
        <> count ManyProjects
        <> ". Info replies are rebuilt per request. Full-store misses and collapsed reads describe public fetches and their waiting consumers. They do not count private reads or exact per-name upstream requests. Ruby pattern rows measure per-gem metadata and synthetic archive requests. They omit /versions discovery and do not measure complete Bundler installations."
  where
    count workload = either ("unavailable: " <>) (show . length) (infoPackages workload knobs)

-- | Per-gem info workloads vary corpus cardinality without claiming assembled-response retention.
data InfoWorkload = RepeatedCorpus | OneProject | ManyProjects
    deriving stock (Eq, Show)

-- | The many-project comparison requires at least two selected captures before proxy boot.
infoPackages :: InfoWorkload -> LoadKnobs -> Either Text [CorpusPackage]
infoPackages workload knobs = case workload of
    RepeatedCorpus -> Right rubyGemsCorpusPackages
    OneProject -> Right (take 1 rubyGemsCorpusPackages)
    ManyProjects ->
        let packages = listingWorkingSet nativeListing knobs
         in if length packages < 2
                then Left "info-working-set-many requires at least two corpus projects"
                else Right packages

infoScenario :: InfoWorkload -> Scenario
infoScenario workload = scenario name description $ \knobs use -> do
    packages <- either benchFail pure (infoPackages workload knobs)
    withListingStubs nativeListing knobs longCacheTtl Nothing packages (weightedMix weight listingUrl packages) $ \proxy urls ->
        preflightDistinct (preflightListings . pure) urls (httpTarget use proxy urls)
  where
    weight = if workload == RepeatedCorpus then cpWeight else const 1
    (name, description) = case workload of
        RepeatedCorpus -> ("info-repeated-corpus", "Read the weighted three-capture native corpus. Each request admits source documents and encodes its Compact Index info reply.")
        OneProject -> ("info-working-set-one", "Read one captured native history repeatedly, with a fresh Compact Index info reply per request.")
        ManyProjects -> ("info-working-set-many", "Read the configured native corpus working set uniformly, with at least two histories and a fresh Compact Index info reply per request.")

withNativeListings :: LoadKnobs -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withNativeListings knobs = withListingStubs nativeListing knobs 0 Nothing rubyGemsCorpusPackages

listingUrl :: Int -> Text -> Text
listingUrl port name = localhost port <> "/rubygems/info/" <> name

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
    absent = absentUpstream latency

artifactScenario :: Bool -> Scenario
artifactScenario privateHit = scenario name description $ \knobs use -> do
    let bytes = artifactBytes (lkPayloadBytes knobs)
        fixture = relayFixture privateHit OrdinaryRelay knobs
    withProxyOverStubs RubyGems knobs (rfCacheTtlSeconds fixture) Nothing (rfPrivate fixture) (rfPublic fixture) (rfTargets fixture) $ \proxy urls -> do
        for_ urls $ \url -> do
            response <- fetchChecked status200 [] url
            unless (HTTP.responseBody response == bytes) (benchFail "RubyGems relay bytes differ from the independently owned payload")
        httpTarget use proxy urls
  where
    name = if privateHit then "gem-hot-path" else "gem-onboarding"
    description =
        if privateHit
            then "Stream a private native artifact after caller-authorised metadata admission."
            else "Stream a public native artifact after private absence and current admission."

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

quickScenario :: Scenario
quickScenario = scenario "quick-gemspec" "Admit an exact native platform coordinate and relay its original Ruby-generated zlib compact specification." $ \knobs use -> do
    fixture <- Worker.newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
        urls port = [localhost port <> "/rubygems/" <> path | path <- Map.keys (Worker.wfSpecs fixture)]
    withProxyOverStubs RubyGems knobs 0 Nothing (absentUpstream latency) (Worker.sourceApp latency fixture) urls $ \proxy targets -> do
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
selectWarmArtifacts = selectPinnedArtifacts "rubygemsPins" $ \package version bytes -> do
    manifest <- captureManifest (gemReadWith (const pass) (const pass) "https://rubygems.org") rubyGemsCaptureUpstream (cpPackage package) [bytes] >>= either (benchFail . show) pure
    pure (selectedArtifact version ("RubyGems pin is absent from the native capture: " <> cpName package <> "@" <> version) (Right . ("gems/" <>)) (manifestInfo manifest))

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

relayBody :: LByteString -> LByteString -> Either Text LByteString
relayBody payload original = do
    rows <- referenceInfo (toStrict original)
    checksums <- fmap Map.fromList $ forM rows $ \row -> do
        previous <- maybeToRight "RubyGems relay row has no checksum" (lookup "checksum" (rowFields row))
        pure (rowPosition row, previous)
    let digest = hexSha256Of (toStrict payload)
        swap bytes previous = swapChecksumPreservingWidth "RubyGems" ("checksum:" <> previous) ("checksum:" <> digest) bytes
    rewriteLines (\position bytes -> maybe (Right bytes) (swap bytes) (Map.lookup position checksums)) original

preflightListings :: [Text] -> IO ()
preflightListings = traverse_ checked . ordNub
  where
    checked url = do
        response <- fetchChecked status200 [] url
        let bytes = toStrict (HTTP.responseBody response)
        rows <- either benchFail pure (referenceInfo bytes)
        void (runRubyReference "info" bytes)
        unless (not (null rows) && not (any (T.null . rowVersion) rows) && all (isJust . lookup "checksum" . rowFields) rows) (benchFail "RubyGems info preflight lost native versions or checksums")

copyBytes :: LByteString
copyBytes = "Owned RubyGems native listing relay bytes"

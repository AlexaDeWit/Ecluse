-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A finite matrix over authenticated captures, with independent cache and upstream evidence.
module Ecluse.BenchLoad.PatternScenario (patternScenarios, loadPins, selectArtifacts) where

import Data.Aeson ((.:))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Network.HTTP.Client qualified as HTTP
import Network.Wai (Application, rawPathInfo)
import UnliftIO (evaluate)

import Ecluse.BenchLoad.BootLines (BootLimits (blCacheBytes, blCacheEntries), bootLimits)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (CacheOutcomes (..), Sample, expositionName, seriesTotal, storeLabel, storeOutcomes)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, loadCorpusBodies, withProxyConfigured)
import Ecluse.BenchLoad.Harness (Driver (DriveReplay), LoadKnobs (..), Scenario (scenarioServiceTime), Target, proxied, scenario)
import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..), selectedNpmArtifact)
import Ecluse.BenchLoad.PatternReport (StoreEvidence (..), renderStoreEvidence)
import Ecluse.BenchLoad.Patterns
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess, ProxySettings (..), proxyBootLines, proxyScrape)
import Ecluse.BenchLoad.Replay (Replay (..))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Cache (CacheEntry (..))
import Ecluse.Core.Telemetry.Catalogue (MetricName (AssembledCacheResidentBytes, MetadataCacheRefused, MetadataCacheResidentBytes, SingleVersionCacheResidentBytes))
import Ecluse.Core.Telemetry.Metrics (CacheStore (AssembledStore, FullStore, VersionStore), Label (LCacheStore), renderLabel)
import Ecluse.Test.Corpus (CaptureRecord (crBytes, crCapturedAt), CorpusPackage (cpPackage), cpName, readCaptureRecords, readCorpusPins)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Server.Cache (weighCacheEntry)
import Ecluse.Test.Wai (localhost, rebaseAuthority)

-- | Every family receives a fresh proxy. No preflight request consumes or warms its trace.
patternScenarios :: Ecosystem -> [CorpusPackage] -> (LoadKnobs -> Application) -> (LoadKnobs -> Map Text LByteString -> Map ByteString LByteString -> IO Application) -> (Int -> Text -> Text) -> [Scenario]
patternScenarios ecosystem packages privateApp publicApp urlFor =
    [cell patternKind False | patternKind <- [minBound .. maxBound]]
        <> [cell ColdInstall True]
  where
    cell patternKind defaultCap =
        ( scenario
            (patternName patternKind <> if defaultCap then "-default-body-cap" else "")
            "Finite captured-name replay from empty stores. TTL 60 seconds. Hot-set is an upper-bound control. Other families carry no workload preference."
            (boot patternKind defaultCap)
        )
            { scenarioServiceTime = False
            }

    boot :: Pattern -> Bool -> LoadKnobs -> (Target -> IO a) -> IO a
    boot patternKind defaultCap knobs use = do
        eco <- maybe (benchFail "pattern scenarios need an ecosystem with a metadata adapter") (pure . metadataRead . adapterMetadata) (adapterFor ecosystem)
        captures <- loadCorpusBodies packages
        evaluationTime <- verifyCaptures ecosystem captures >>= patternClock
        patternKnobs <- knobsFromEnv patternKind (length packages)
        requestTrace <- either benchFail pure (makeTrace patternKind patternKnobs (map cpName packages))
        wireBytes <- either benchFail pure (workingBytes (Map.map (fromIntegral . LBS.length) captures) requestTrace)
        let largest = foldl' max 0 (map (fromIntegral . LBS.length) (Map.elems captures))
        measuredBodies <- newIORef (maxMetadataBytes defaultLimits, wireBytes, largest, 0)
        fullCapacity <- readKnob "BENCH_PATTERN_FULL_BYTES" (0 :: Int)
        when (fullCapacity /= 0) (benchFail "BENCH_PATTERN_FULL_BYTES must be zero: the local backend never retains full metadata")
        for_ ["BENCH_PATTERN_VERSION_BYTES", "BENCH_PATTERN_ASSEMBLED_BYTES"] $ \name -> do
            configured <- lookupEnv name
            when (isJust configured) (benchFail (toText name <> " was replaced by BENCH_PATTERN_CACHE_BYTES for the shared pool"))
        -- Unset, the proxy sizes the shared pool from its heap as a pod would.
        capacity <- traverse (maybe (benchFail "invalid BENCH_PATTERN_CACHE_BYTES") pure . readMaybe) =<< lookupEnv "BENCH_PATTERN_CACHE_BYTES"
        when (maybe False (<= 0) capacity) (benchFail "pattern cache budget must be positive")
        let configure publicPort = do
                let authority = if ecosystem == Npm then "https://registry.npmjs.org" else "https://files.pythonhosted.org"
                    servedBodies = Map.map (rebaseAuthority authority (localhost publicPort)) captures
                    servedSizes = Map.map (fromIntegral . LBS.length) servedBodies
                    servedLargest = foldl' max 0 (Map.elems servedSizes)
                    bodyCap = if defaultCap then maxMetadataBytes defaultLimits else max (maxMetadataBytes defaultLimits) servedLargest
                servedWorking <- either benchFail pure (workingBytes servedSizes requestTrace)
                fullWeights <-
                    traverse
                        ( \package ->
                            case Map.lookup (cpName package) servedBodies of
                                Nothing -> benchFail "missing served capture"
                                Just bytes -> accountedFullBytes eco (localhost publicPort) package bytes >>= either benchFail evaluate
                        )
                        [package | package <- packages, cpName package `elem` rtNames requestTrace]
                writeIORef measuredBodies (bodyCap, servedWorking, servedLargest, sum fullWeights)
                pure $ \settings ->
                    settings
                        { psCacheMaxBytes = capacity
                        , psMaxResponseBytes = bodyCap <$ guard (bodyCap /= maxMetadataBytes defaultLimits)
                        , psClock = Just evaluationTime
                        }
        deadlineMicros <- readKnob "BENCH_PATTERN_DEADLINE_US" (120_000_000 :: Int)
        when (deadlineMicros <= 0) (benchFail "BENCH_PATTERN_DEADLINE_US must be positive")
        selected <- lookupEnv "BENCH_PATTERN_SELECTED_VERSION"
        pins <- loadPins
        selectedArtifacts <- either benchFail pure (selectArtifacts ecosystem selected pins [package | package <- packages, cpName package `elem` rtNames requestTrace] captures)
        artifactRequests <- traverse (HTTP.parseRequest . toString . saUpstreamUrl) (Map.elems selectedArtifacts)
        let artifactBodies = Map.fromList [(HTTP.path request, artifactBytes (lkPayloadBytes knobs)) | request <- artifactRequests]
        upstreamCount <- newIORef (0 :: Int, 0 :: Int)
        public <- publicApp knobs captures artifactBodies
        let counted request respond = do
                atomicModifyIORef'
                    upstreamCount
                    ( \(metadataCount, artifactCount) ->
                        (if Map.member (rawPathInfo request) artifactBodies then (metadataCount, artifactCount + 1) else (metadataCount + 1, artifactCount), ())
                    )
                public request respond
        withProxyConfigured ecosystem knobs configure (privateApp knobs) counted (\port -> [urlFor port ""]) $ \proxy -> \case
            [root] ->
                use
                    ( proxied
                        proxy
                        ( DriveReplay
                            Replay
                                { replayTrace = requestTrace
                                , replayDeadlineMicros = deadlineMicros
                                , replayUrls = \name -> (root <> name) : [root <> saProxyPath artifact | artifact <- maybeToList (Map.lookup name selectedArtifacts)]
                                , replayEvidence = (<> ("\nReplay deadline: " <> show deadlineMicros <> " microseconds.\n")) <$> evidence proxy upstreamCount capacity wireBytes measuredBodies patternKnobs requestTrace selected evaluationTime
                                }
                        )
                    )
            _ -> benchFail "pattern fixture requires exactly one URL root"

-- | The captured version pins in @bench/corpus/pins.json@, by package name.
loadPins :: IO (Map Text Text)
loadPins = readCorpusPins (.: "pins") >>= either (benchFail . toText) pure

knobsFromEnv :: Pattern -> Int -> IO PatternKnobs
knobsFromEnv family available = do
    let defaults = defaultPatternKnobs
        heterogeneous = family == Heterogeneous
    names <- readKnob "BENCH_PATTERN_NAMES" (if heterogeneous then 2 else available)
    clients <- readKnob "BENCH_PATTERN_CLIENTS" (if heterogeneous then min 2 available else pkClients defaults)
    skew <- readKnob "BENCH_PATTERN_SKEW_US" (pkSkewMicros defaults)
    rounds <- readKnob "BENCH_PATTERN_ROUNDS" (pkRounds defaults)
    overlap <- readKnob "BENCH_PATTERN_OVERLAP" (pkOverlap defaults)
    exponent <- readKnob "BENCH_PATTERN_ZIPF_EXPONENT" (pkExponent defaults)
    arrival <- readKnob "BENCH_PATTERN_ARRIVAL_US" (pkArrivalMicros defaults)
    seed <- readKnob "BENCH_PATTERN_SEED" (pkSeed defaults)
    pure (PatternKnobs names clients skew rounds overlap exponent arrival seed)

readKnob :: (Read a) => String -> a -> IO a
readKnob name fallback =
    lookupEnv name >>= \case
        Nothing -> pure fallback
        Just raw -> maybe (benchFail ("invalid " <> toText name)) pure (readMaybe raw)

verifyCaptures :: Ecosystem -> Map Text LByteString -> IO UTCTime
verifyCaptures ecosystem bodies = do
    records <- readCaptureRecords ecosystem >>= either (benchFail . toText) pure
    for_ (Map.toList bodies) $ \(name, body) ->
        unless ((crBytes <$> Map.lookup name records) == Just (LBS.length body)) (benchFail ("complete capture provenance missing or byte count differs: " <> name))
    pure (addUTCTime (2 * nominalDay) (foldl' max benchNow (map crCapturedAt (Map.elems records))))

evidence :: ProxyProcess -> IORef (Int, Int) -> Maybe Int -> Int -> IORef (Int, Int, Int, Int) -> PatternKnobs -> RequestTrace -> Maybe String -> UTCTime -> IO Text
evidence proxy upstreamCount capacity rawBytes measuredBodies knobs requestTrace selected evaluationTime = do
    (bodyCap, wireBytes, largest, fullWorkingBytes) <- readIORef measuredBodies
    scraped <- fromMaybe [] <$> proxyScrape proxy
    (metadataRequests, artifactRequests) <- readIORef upstreamCount
    let limits = bootLimits (proxyBootLines proxy)
        shared = capacity <|> blCacheBytes limits
        stores =
            [ collect scraped fullWorkingBytes FullStore 0
            , collect scraped fullWorkingBytes VersionStore (fromMaybe 0 shared)
            , collect scraped fullWorkingBytes AssembledStore (fromMaybe 0 shared)
            ]
    pure $
        T.unlines
            [ "Replay parameters: `" <> show knobs <> "`. Actual clients: " <> show (length (rtClients requestTrace)) <> ". Distinct measured names: " <> show (length (rtNames requestTrace)) <> "."
            , "Pattern evaluation time: " <> toText (iso8601Show evaluationTime) <> ". Listing-only and artifact-follow-up cells share this clock. Default: latest authenticated capture time plus two days. BENCH_PATTERN_NOW overrides it."
            , "Corpus space and tail are bounded by the committed captures. Zipf is a finite sampled trace, not registry-wide traffic."
            , "Shared local budget: " <> maybe "unknown" show shared <> " accounted bytes and " <> maybe "unknown" show (blCacheEntries limits) <> " entries, " <> maybe "as the proxy sized it from its heap" (const "set by BENCH_PATTERN_CACHE_BYTES") capacity <> ". Version and assembled rows share this ceiling, not separate capacities. Their floors are zero."
            , "Local full retention is ineligible. Effective full capacity is zero. Full requests still coalesce, without retention weighing, encoding, insertion, or capacity refusals."
            , "Raw captured working bytes: " <> show rawBytes <> " B. Served stub working bytes: " <> show wireBytes <> " B. Selected-version mode: " <> maybe "none" toText selected <> "."
            , "Body cap: " <> show bodyCap <> " B. Default cap: " <> show (maxMetadataBytes defaultLimits) <> " B. Largest served stub body: " <> show largest <> " B. Default would refuse largest: " <> show (largest > maxMetadataBytes defaultLimits) <> "."
            , "Wire working set / full-store wire-equivalent budget: " <> show wireBytes <> " / 0 B. Full retention is ineligible."
            , "Public upstream requests (metadata / artifact): " <> show metadataRequests <> " / " <> show artifactRequests <> ". Selected lookups use only the selected provider capability."
            , renderStoreEvidence stores
            , "Selected npm replay follows listings with captured public tarball coordinates after private misses. Artifact bytes are synthetic relay payloads. This measures the HTTP metadata gate, not a complete npm install or client integrity validation."
            , "RTS figures describe the proxy process alone. The replay client and the stub upstreams run in the harness process."
            , "Occupancy is the final reported gauge, not peak heap. Full working bytes use production projection and historical weighCacheEntry over each distinct rewritten body before measurement. Version and assembled working sets are unavailable. Their representations differ from listing wire bytes."
            , "Full candidate charges above are diagnostic preparation only. The local request path never weighs full candidates. Compare equal successful work and the shared eligible-store budget."
            ]

-- One store's outcomes and occupancy from the scrape.
collect :: [Sample] -> Int -> CacheStore -> Int -> StoreEvidence
collect scraped fullWorkingBytes store capacity =
    StoreEvidence
        { seStore = storeLabel store
        , seCapacity = capacity
        , seAccountedWorkingSet = if store == FullStore then Just fullWorkingBytes else Nothing
        , seResidentBytes = round (fromMaybe 0 (seriesTotal (expositionName residentBytes) [] scraped))
        , seHits = coHits outcomes
        , seMisses = coMisses outcomes
        , seCollapsed = coCollapsed outcomes
        , seRefused = round (fromMaybe 0 (seriesTotal (expositionName MetadataCacheRefused) [renderLabel (LCacheStore store)] scraped))
        }
  where
    outcomes = storeOutcomes scraped store
    residentBytes = case store of
        FullStore -> MetadataCacheResidentBytes
        VersionStore -> SingleVersionCacheResidentBytes
        AssembledStore -> AssembledCacheResidentBytes

-- The cache weight of a served body's production full read, as a fetch from the stub upstream holds it.
accountedFullBytes :: EcosystemRead -> Text -> CorpusPackage -> LByteString -> IO (Either Text Int)
accountedFullBytes eco upstreamBase package bytes =
    bimap show weigh <$> heldManifest eco defaultLimits upstreamBase (cpPackage package) [LBS.toStrict bytes]
  where
    weigh manifest = weighCacheEntry (CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestDigest manifest))

-- | The captured artifact each package's selected version names, keyed by package name. npm only.
selectArtifacts :: Ecosystem -> Maybe String -> Map Text Text -> [CorpusPackage] -> Map Text LByteString -> Either Text (Map Text SelectedArtifact)
selectArtifacts ecosystem selected pins packages captures =
    case selected of
        Just choice | ecosystem == Npm -> Map.fromList <$> traverse (select choice) packages
        _ -> Right Map.empty
  where
    select choice package = do
        let name = cpName package
        version <- if choice == "pinned" then maybe (Left ("missing captured pin: " <> name)) Right (Map.lookup name pins) else Right (toText choice)
        bytes <- maybe (Left ("missing captured body: " <> name)) Right (Map.lookup name captures)
        artifact <- selectedNpmArtifact (cpPackage package) version (LBS.toStrict bytes)
        pure (name, artifact)

patternClock :: UTCTime -> IO UTCTime
patternClock fallback =
    lookupEnv "BENCH_PATTERN_NOW" >>= \case
        Nothing -> pure fallback
        Just raw -> maybe (benchFail "BENCH_PATTERN_NOW must be an ISO8601 UTC time") pure (iso8601ParseM raw)

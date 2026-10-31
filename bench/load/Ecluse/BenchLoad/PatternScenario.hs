-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A finite matrix over authenticated captures, with independent cache and upstream evidence.
module Ecluse.BenchLoad.PatternScenario (PatternProvider (..), patternRequestUrls, patternScenarios) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Network.HTTP.Client qualified as HTTP
import Network.Wai (Application, rawPathInfo)
import UnliftIO (evaluate)

import Ecluse.BenchLoad.BootLines (BootLimits (blCacheBytes, blCacheEntries), bootLimits)
import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (CacheOutcomes (..), Sample, expositionName, seriesTotal, storeLabel, storeOutcomes, storeSamples)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, loadCorpusBodies, withProxyConfigured)
import Ecluse.BenchLoad.Harness (Driver (DriveReplay), LoadKnobs (..), Scenario (scenarioServiceTime), Target, proxied, scenario)
import Ecluse.BenchLoad.PatternReport (StoreEvidence (..), renderStoreEvidence)
import Ecluse.BenchLoad.Patterns
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess, ProxySettings (..), proxyBootLines, proxyScrape)
import Ecluse.BenchLoad.Replay (Replay (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Cache (CacheEntry (..))
import Ecluse.Core.Telemetry.Catalogue (MetricName (AssembledCacheResidentBytes, MetadataCacheRefused, MetadataCacheResidentBytes, SingleVersionCacheResidentBytes))
import Ecluse.Core.Telemetry.Metrics (CacheStore (AssembledStore, DirectoryStore, FullStore, VersionStore), Label (LCacheStore), renderLabel)
import Ecluse.Test.Corpus (CaptureRecord (crCapturedAt), CorpusPackage (cpPackage), cpName, readCaptureRecords)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Server.Cache (weighCacheEntry)
import Ecluse.Test.Wai (localhost, rebaseAuthority)

-- | Native replay inputs keep capture rewriting and artifact selection with the provider.
data PatternProvider = PatternProvider
    { ppCaptureAuthority :: Text
    , ppRootUrl :: Int -> Text
    -- ^ The native proxy root, including its final slash, without a listing query.
    , ppListingSuffix :: Text -> Text
    , ppDiscoveryPath :: Maybe Text
    , ppCapturedRead :: Text -> Either MetadataError EcosystemRead
    , ppSelectedArtifacts :: Maybe String -> [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
    }

-- | Every family receives a fresh proxy. No preflight request consumes or warms its trace.
patternScenarios :: PatternProvider -> Ecosystem -> [CorpusPackage] -> (LoadKnobs -> Application) -> (LoadKnobs -> Map Text LByteString -> Map ByteString LByteString -> IO Application) -> [Scenario]
patternScenarios provider ecosystem packages privateApp publicApp =
    [cell patternKind False | patternKind <- [minBound .. maxBound]]
        <> [cell ColdInstall True]
  where
    cell patternKind defaultCap =
        ( scenario
            (patternName patternKind <> if defaultCap then "-default-body-cap" else "")
            ( patternDescription patternKind
                <> " Each client sends sequential requests over captured package names. Every scenario starts with a fresh proxy and empty stores. Cache TTL is 60 seconds."
                <> if defaultCap then " The production response body cap applies, so oversized metadata receives a refusal." else ""
            )
            (boot patternKind defaultCap)
        )
            { scenarioServiceTime = False
            }

    boot :: Pattern -> Bool -> LoadKnobs -> (Target -> IO a) -> IO a
    boot patternKind defaultCap knobs use = do
        captures <- loadCorpusBodies packages
        evaluationTime <- captureClock ecosystem >>= patternClock
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
                let servedBodies = Map.map (rebaseAuthority (ppCaptureAuthority provider) (localhost publicPort)) captures
                    servedSizes = Map.map (fromIntegral . LBS.length) servedBodies
                    servedLargest = foldl' max 0 (Map.elems servedSizes)
                    bodyCap = if defaultCap then maxMetadataBytes defaultLimits else max (maxMetadataBytes defaultLimits) servedLargest
                servedWorking <- either benchFail pure (workingBytes servedSizes requestTrace)
                eco <- either (benchFail . show) pure (ppCapturedRead provider (localhost publicPort))
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
        selectedArtifacts <- ppSelectedArtifacts provider selected [package | package <- packages, cpName package `elem` rtNames requestTrace] captures
        artifactRequests <- traverse (HTTP.parseRequest . toString . saUpstreamUrl) (Map.elems selectedArtifacts)
        let artifactBodies = Map.fromList [(HTTP.path request, artifactBytes (lkPayloadBytes knobs)) | request <- artifactRequests]
        upstreamCount <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
        public <- publicApp knobs captures artifactBodies
        let counted request respond = do
                atomicModifyIORef'
                    upstreamCount
                    ( \(metadataCount, artifactCount, discoveryCount) ->
                        ( if maybe False ((== rawPathInfo request) . encodeUtf8 . ("/" <>)) (ppDiscoveryPath provider)
                            then (metadataCount, artifactCount, discoveryCount + 1)
                            else
                                if Map.member (rawPathInfo request) artifactBodies
                                    then (metadataCount, artifactCount + 1, discoveryCount)
                                    else (metadataCount + 1, artifactCount, discoveryCount)
                        , ()
                        )
                    )
                public request respond
        withProxyConfigured ecosystem knobs configure (privateApp knobs) counted (\port -> [ppRootUrl provider port]) $ \proxy -> \case
            [root] -> do
                writeIORef upstreamCount (0, 0, 0)
                use
                    ( proxied
                        proxy
                        ( DriveReplay
                            Replay
                                { replayTrace = requestTrace
                                , replayDeadlineMicros = deadlineMicros
                                , replayUrls = patternRequestUrls provider root selectedArtifacts
                                , replayEvidence = (<> ("\nReplay deadline: " <> show deadlineMicros <> " microseconds.\n")) <$> evidence proxy upstreamCount capacity wireBytes measuredBodies patternKnobs requestTrace selected evaluationTime
                                }
                        )
                    )
            _ -> benchFail "pattern fixture requires exactly one URL root"

-- | Discovery precedes the listing, followed by a selected native artifact when present.
patternRequestUrls :: PatternProvider -> Text -> Map Text SelectedArtifact -> Text -> [Text]
patternRequestUrls provider root selected name =
    [root <> path | path <- maybeToList (ppDiscoveryPath provider)]
        <> [root <> ppListingSuffix provider name]
        <> [root <> saProxyPath artifact | artifact <- maybeToList (Map.lookup name selected)]

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

captureClock :: Ecosystem -> IO UTCTime
captureClock ecosystem = do
    records <- readCaptureRecords ecosystem >>= either (benchFail . toText) pure
    pure (addUTCTime (2 * nominalDay) (foldl' max benchNow (map crCapturedAt (Map.elems records))))

evidence :: ProxyProcess -> IORef (Int, Int, Int) -> Maybe Int -> Int -> IORef (Int, Int, Int, Int) -> PatternKnobs -> RequestTrace -> Maybe String -> UTCTime -> IO Text
evidence proxy upstreamCount capacity rawBytes measuredBodies knobs requestTrace selected evaluationTime = do
    (bodyCap, wireBytes, largest, fullWorkingBytes) <- readIORef measuredBodies
    scraped <- fromMaybe [] <$> proxyScrape proxy
    (metadataRequests, artifactRequests, discoveryRequests) <- readIORef upstreamCount
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
            , "Public upstream requests (metadata / artifact): " <> show metadataRequests <> " / " <> show artifactRequests <> ". Discovery requests: " <> show discoveryRequests <> ". Selected lookups use only the selected provider capability."
            , renderStoreEvidence stores
            , "Selected replay follows native discovery and listings with captured public artifact coordinates after private misses. Artifact bytes are synthetic relay payloads. This measures the HTTP metadata gate, not a complete npm install or client integrity validation."
            , "RTS figures describe the proxy process alone. The replay client and the stub upstreams run in the harness process."
            , "Occupancy is the final reported gauge, not peak heap. Full working bytes use production projection and historical weighCacheEntry over each distinct rewritten body before measurement. Version and assembled working sets are unavailable. Their representations differ from listing wire bytes."
            , "Full candidate charges above are diagnostic preparation only. The local request path never weighs full candidates. Compare equal successful work and the shared eligible-store budget."
            ]

collect :: [Sample] -> Int -> CacheStore -> Int -> StoreEvidence
collect scraped fullWorkingBytes store capacity =
    StoreEvidence
        { seStore = storeLabel store
        , seCapacity = capacity
        , seAccountedWorkingSet = if store == FullStore then Just fullWorkingBytes else Nothing
        , seResidentBytes = round (fromMaybe 0 (seriesTotal (expositionName residentBytes) [] (storeSamples store scraped)))
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
        DirectoryStore -> MetadataCacheResidentBytes

accountedFullBytes :: EcosystemRead -> Text -> CorpusPackage -> LByteString -> IO (Either Text Int)
accountedFullBytes eco upstreamBase package bytes =
    bimap show weigh <$> heldManifest eco defaultLimits upstreamBase (cpPackage package) [LBS.toStrict bytes]
  where
    weigh manifest = weighCacheEntry (CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestDigest manifest))

patternClock :: UTCTime -> IO UTCTime
patternClock fallback =
    lookupEnv "BENCH_PATTERN_NOW" >>= \case
        Nothing -> pure fallback
        Just raw -> maybe (benchFail "BENCH_PATTERN_NOW must be an ISO8601 UTC time") pure (iso8601ParseM raw)

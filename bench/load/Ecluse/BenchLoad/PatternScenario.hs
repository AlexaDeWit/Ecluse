-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A finite matrix over authenticated captures, with independent cache and upstream evidence.
module Ecluse.BenchLoad.PatternScenario (patternScenarios) where

import Data.Aeson (Value, eitherDecode, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Universe.Class qualified as Universe
import Network.HTTP.Client qualified as HTTP
import Network.Wai (Application, rawPathInfo)
import OpenTelemetry.Attributes (fromAttribute, lookupAttribute)
import OpenTelemetry.MeterProvider (SdkMeterEnv)
import UnliftIO (evaluate)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, benchNow, loadCorpusBodies, withProxyConfigured)
import Ecluse.BenchLoad.Harness (Driver (DriveReplay), LoadKnobs (..), Scenario (..))
import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..), selectedNpmArtifact)
import Ecluse.BenchLoad.PatternReport (StoreEvidence (..), renderStoreEvidence)
import Ecluse.BenchLoad.Patterns
import Ecluse.BenchLoad.Replay (Replay (..))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Registry.CachedDocument (npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (digestOf)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Cache (CacheConfig (..), CacheEntry (..), StoreBudget (..))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.MemoryModel (contractResidentBytes)
import Ecluse.Core.Telemetry.Catalogue (MetricName, metricName)
import Ecluse.Runtime.Test.Telemetry (gaugePoints, sumPoints, withTestTelemetry)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage), cpName)
import Ecluse.Test.Server.Cache (defaultCacheConfig, weighCacheEntry)
import Ecluse.Test.Wai (localhost, rebaseAuthority)

-- | Every family receives a fresh proxy. No preflight request consumes or warms its trace.
patternScenarios :: Ecosystem -> [CorpusPackage] -> (Int -> Int -> IO PackumentDeps) -> (LoadKnobs -> Application) -> (LoadKnobs -> Map Text LByteString -> Map ByteString LByteString -> IO Application) -> (Int -> Text -> Text) -> [Scenario]
patternScenarios ecosystem packages depsFor privateApp publicApp urlFor =
    [scenario patternKind False | patternKind <- [minBound .. maxBound]]
        <> [scenario ColdInstall True]
  where
    scenario patternKind defaultCap =
        Scenario
            { scenarioName = patternName patternKind <> if defaultCap then "-default-body-cap" else ""
            , scenarioDescription = "Finite captured-name replay from empty stores. TTL 60 seconds. Hot-set is an upper-bound control. Other families carry no workload preference."
            , scenarioConcurrencyScale = 1
            , scenarioBoot = \knobs use -> do
                captures <- loadCorpusBodies packages
                evaluationTime <- verifyCaptures ecosystem captures >>= patternClock
                patternKnobs <- knobsFromEnv patternKind (length packages)
                requestTrace <- either benchFail pure (makeTrace patternKind patternKnobs (map cpName packages))
                wireBytes <- either benchFail pure (workingBytes (Map.map (fromIntegral . LBS.length) captures) requestTrace)
                let largest = foldl' max 0 (map (fromIntegral . LBS.length) (Map.elems captures))
                measuredBodies <- newIORef (maxMetadataBytes defaultLimits, wireBytes, largest, 0)
                fullCapacity <- readKnob "BENCH_PATTERN_FULL_BYTES" (0 :: Int)
                when (fullCapacity /= 0) (benchFail "BENCH_PATTERN_FULL_BYTES must be zero: the local backend never retains full metadata")
                versionCapacity <- readKnob "BENCH_PATTERN_VERSION_BYTES" (sbMaxBytes (cacheVersionBudget defaultCacheConfig))
                assembledCapacity <- readKnob "BENCH_PATTERN_ASSEMBLED_BYTES" (sbMaxBytes (cacheAssembledBudget defaultCacheConfig))
                when
                    (fullCapacity < 0 || versionCapacity <= 0 || assembledCapacity <= 0)
                    (benchFail "pattern full budget must be non-negative and version/assembled budgets must be positive")
                let cacheConfig =
                        defaultCacheConfig
                            { cacheVersionBudget = (cacheVersionBudget defaultCacheConfig){sbMaxBytes = versionCapacity}
                            , cacheAssembledBudget = (cacheAssembledBudget defaultCacheConfig){sbMaxBytes = assembledCapacity}
                            }
                    deps privatePort publicPort = do
                        base <- depsFor privatePort publicPort
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
                                        Just bytes -> either benchFail evaluate (accountedFullBytes ecosystem (localhost publicPort) package bytes)
                                )
                                [package | package <- packages, cpName package `elem` rtNames requestTrace]
                        writeIORef measuredBodies (bodyCap, servedWorking, servedLargest, sum fullWeights)
                        pure base{pdLimits = (pdLimits base){maxMetadataBytes = bodyCap}, pdNow = pure evaluationTime}
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
                withTestTelemetry $ \telemetry meter ->
                    withProxyConfigured ecosystem deps knobs cacheConfig telemetry (privateApp knobs) counted (\port -> [urlFor port ""]) $ \case
                        [root] ->
                            use
                                ( DriveReplay
                                    Replay
                                        { replayTrace = requestTrace
                                        , replayDeadlineMicros = deadlineMicros
                                        , replayUrls = \name -> (root <> name) : [root <> saProxyPath artifact | artifact <- maybeToList (Map.lookup name selectedArtifacts)]
                                        , replayEvidence = (<> ("\nReplay deadline: " <> show deadlineMicros <> " microseconds.\n")) <$> evidence meter upstreamCount cacheConfig wireBytes measuredBodies patternKnobs requestTrace selected evaluationTime
                                        }
                                )
                        _ -> benchFail "pattern fixture requires exactly one URL root"
            }

loadPins :: IO (Map Text Text)
loadPins = do
    raw <- readFileLBS "bench/corpus/pins.json"
    value <- either (benchFail . toText) pure (eitherDecode raw :: Either String Value)
    either (benchFail . toText) pure (parseEither (withObject "pins" (.: "pins")) value)

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
    raw <- readFileLBS "bench/corpus/pins.json"
    manifest <- either (benchFail . toText) pure (eitherDecode raw :: Either String Value)
    sizes <- either (benchFail . toText) pure (parseEither parser manifest)
    for_ (Map.toList bodies) $ \(name, body) ->
        unless ((fst <$> Map.lookup name sizes) == Just (LBS.length body)) (benchFail ("complete capture provenance missing or byte count differs: " <> name))
    pure (addUTCTime (2 * nominalDay) (foldl' max benchNow (map snd (Map.elems sizes))))
  where
    parser = withObject "pins" $ \pins -> do
        captures <- pins .: "captures"
        entries <- captures .: fromString (toString (ecosystemName ecosystem))
        traverse (withObject "capture" (\capture -> (,) <$> capture .: "bytes" <*> capture .: "capturedAt")) entries

evidence :: SdkMeterEnv -> IORef (Int, Int) -> CacheConfig -> Int -> IORef (Int, Int, Int, Int) -> PatternKnobs -> RequestTrace -> Maybe String -> UTCTime -> IO Text
evidence meter upstreamCount config rawBytes measuredBodies knobs requestTrace selected evaluationTime = do
    (bodyCap, wireBytes, largest, fullWorkingBytes) <- readIORef measuredBodies
    stores <-
        traverse
            (collect fullWorkingBytes)
            [("full", "", cacheFullBudget config), ("version", ".version", cacheVersionBudget config), ("assembled", ".assembled", cacheAssembledBudget config)]
    (metadataRequests, artifactRequests) <- readIORef upstreamCount
    pure $
        T.unlines
            [ "Replay parameters: `" <> show knobs <> "`. Actual clients: " <> show (length (rtClients requestTrace)) <> ". Distinct measured names: " <> show (length (rtNames requestTrace)) <> "."
            , "Pattern evaluation time: " <> toText (iso8601Show evaluationTime) <> ". Listing-only and artifact-follow-up cells share this clock. Default: latest authenticated capture time plus two days. BENCH_PATTERN_NOW overrides it."
            , "Corpus space and tail are bounded by the committed captures. Zipf is a finite sampled trace, not registry-wide traffic."
            , "Configured store budgets (full / version / assembled): " <> show (sbMaxBytes (cacheFullBudget config)) <> " / " <> show (sbMaxBytes (cacheVersionBudget config)) <> " / " <> show (sbMaxBytes (cacheAssembledBudget config)) <> " accounted bytes."
            , "Local full retention is ineligible. Effective full capacity is zero. Full requests still coalesce, without retention weighing, encoding, insertion, or capacity refusals."
            , "Raw captured working bytes: " <> show rawBytes <> " B. Served stub working bytes: " <> show wireBytes <> " B. Selected-version mode: " <> maybe "none" toText selected <> "."
            , "Body cap: " <> show bodyCap <> " B. Default cap: " <> show (maxMetadataBytes defaultLimits) <> " B. Largest served stub body: " <> show largest <> " B. Default would refuse largest: " <> show (largest > maxMetadataBytes defaultLimits) <> "."
            , "Wire working set / full-store wire-equivalent budget: " <> show wireBytes <> " / " <> show (contractResidentBytes (sbMaxBytes (cacheFullBudget config))) <> " B. The resident estimate excludes retained artifact keys."
            , "Public upstream requests (metadata / artifact): " <> show metadataRequests <> " / " <> show artifactRequests <> ". Selected lookups use only the selected provider capability."
            , if metricsAvailable then renderStoreEvidence stores else "Cache evidence unavailable: this build lacks the collapse and refusal telemetry catalogue. Full-store candidate accounted bytes / capacity: " <> show fullWorkingBytes <> " / " <> show (sbMaxBytes (cacheFullBudget config)) <> "."
            , "Selected npm replay follows listings with captured public tarball coordinates after private misses. Artifact bytes are synthetic relay payloads. This measures the HTTP metadata gate, not a complete npm install or client integrity validation."
            , "RTS allocation and heap figures include the in-process replay client and stub upstreams. They are not proxy-only costs or directly comparable with the external oha generator."
            , "Occupancy is the final reported gauge, not peak heap. Full working bytes use production projection and historical weighCacheEntry over each distinct rewritten body before measurement. Version and assembled working sets are unavailable. Their representations differ from listing wire bytes."
            , "Full candidate charges above are diagnostic preparation only. The local request path never weighs full candidates. Compare equal successful work and all eligible store budgets."
            ]
  where
    metricsAvailable =
        all
            (`elem` map metricName (Universe.universe :: [MetricName]))
            ["ecluse.metadata_cache.version.requests", "ecluse.metadata_cache.assembled.requests", "ecluse.metadata_cache.refused"]
    collect fullWorkingBytes (storeName, suffix, budget) = do
        outcomes <- sumPoints ("ecluse.metadata_cache" <> suffix <> ".requests") meter
        occupied <- gaugePoints ("ecluse.metadata_cache" <> suffix <> ".resident_bytes") meter
        refused <- sumPoints "ecluse.metadata_cache.refused" meter
        let count key value points = fromIntegral (sum [n | (attrs, n) <- points, (lookupAttribute attrs key >>= fromAttribute) == Just (value :: Text)])
        pure
            StoreEvidence
                { seStore = storeName
                , seCapacity = sbMaxBytes budget
                , seAccountedWorkingSet = if storeName == "full" then Just fullWorkingBytes else Nothing
                , seResidentBytes = fromIntegral (sum (map snd occupied))
                , seHits = count "result" "hit" outcomes
                , seMisses = count "result" "miss" outcomes
                , seCollapsed = count "result" "collapsed" outcomes
                , seRefused = count "store" storeName refused
                }

accountedFullBytes :: Ecosystem -> Text -> CorpusPackage -> LByteString -> Either Text Int
accountedFullBytes ecosystem upstreamBase package bytes = do
    let raw = LBS.toStrict bytes
    (info, document) <-
        first show $
            if ecosystem == Npm
                then second (fst npmCached) <$> projectNpmManifest defaultLimits (cpPackage package) raw
                else second (fst pypiSimpleCached) <$> projectPyPIIndex defaultLimits (cpPackage package) raw
    let hosts = if ecosystem == Npm then npmArtifactHosts else pypiArtifactHosts
        located = enforceArtifactLocations (ecosystemArtifactAuthorities hosts) upstreamBase info
    pure (weighCacheEntry (CacheEntry located document (fromIntegral (LBS.length bytes)) (digestOf raw)))

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

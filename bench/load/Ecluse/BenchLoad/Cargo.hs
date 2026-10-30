-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo HTTP reads and whole-version worker load scenarios over native local fixtures.
module Ecluse.BenchLoad.Cargo (
    cargoFixture,
    cargoLoadNotes,
    cargoPrivateCopy,
    selectWarmArtifacts,
    indexStubWithArtifacts,
    crateStubWith,
) where

import Data.Aeson (Object, Value, eitherDecodeStrict, encode, object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Request, Response, pathInfo, rawPathInfo, responseLBS)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..), selectedArtifact)
import Ecluse.BenchLoad.Cargo.CopyState (withCopyState)
import Ecluse.BenchLoad.Cargo.Worker (cargoWorkerScenario)
import Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, delayUpstream, fetchChecked, httpTarget, longCacheTtl, rewriteLines, selectPinnedArtifacts, swapChecksumPreservingWidth, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveBurst, DriveRamp), LoadKnobs (..), Scenario (..), UpstreamFixture, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.Listing (ListingSetup (..), cacheEvictsLoad, cacheFitsLoad, listingLoad, listingWorkingSet, revalidateLoad, slowReaderScenario, withListingStubs)
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..))
import Ecluse.BenchLoad.PrivateCopy (ManagedCopyState (ManagedCopyState), PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.BenchLoad.Selection (evictionEntries)
import Ecluse.BenchLoad.Stress (WarmLoadDriver (..), herdSize, pairedLoad, rampSteps)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Registry.Cargo.Discovery (renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Security (ecosystemArtifactAuthorities)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpWeight), cargoCaptureUpstream, cargoCorpusPackages, cpName)
import Ecluse.Test.Corpus.Cargo (cargoCaptureDiscovery, cargoVersionKeys)
import Ecluse.Test.Corpus.Subset (newestCargoShare)
import Ecluse.Test.Json (withKeys)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoShard)
import Ecluse.Test.Registry.Cargo.Metadata (cargoArtifactPath, cargoRead, parseCargoDiscovery)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexValue), referenceDiscovery, referenceIndex)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Wai (localhost, selfBaseUrl)

-- | Recurring Cargo reads and whole-version worker measurements through native upstream fixtures.
cargoFixture :: UpstreamFixture
cargoFixture = readLoadContract cargoReadDriver [cargoWorkerScenario]

cargoReadDriver :: LoadReadDriver
cargoReadDriver =
    LoadReadDriver
        { lrdPrivateCopy = cargoPrivateCopy
        , lrdColdMetadata = indexColdScenario
        , lrdSlowReaders = slowReaderScenario cargoListing "index-cold"
        , lrdAssembledHit = indexScenario "assembled-response-hit" "GET the weighted sparse-index corpus with retained assembled responses. Full public and private indexes are fetched per request, except overlapping public reads share active work." longCacheTtl
        , lrdRevalidate = revalidateScenario
        , lrdCacheFits = cacheFitsScenario
        , lrdCacheEvicts = cacheEvictsScenario
        , lrdPrivateArtifact = crateScenario PrivateCrate
        , lrdPublicArtifact = crateScenario PublicOnboarding
        , lrdArtifactCeiling = crateCeilingScenario
        , lrdHerd = herdScenario
        , lrdWarmUnderCold = warmUnderColdScenario
        , lrdRamp = rampScenario
        , lrdPatternProvider =
            PatternProvider
                { ppCaptureAuthority = "https://static.crates.io"
                , ppRootUrl = (`indexUrl` "")
                , ppListingSuffix = cargoShard
                , ppDiscoveryPath = Just "config.json"
                , ppCapturedRead = \base -> cargoRead <$> parseCargoDiscovery (ecosystemArtifactAuthorities cargoArtifactHosts) base (renderCargoDiscovery (base <> "/crates/{crate}/{crate}-{version}.crate") Nothing False)
                , ppSelectedArtifacts = \selected packages bodies -> case selected of
                    Nothing -> pure Map.empty
                    Just "pinned" -> selectWarmArtifacts packages bodies
                    Just _ -> benchFail "Cargo selected replay requires pinned"
                }
        , lrdPatternPrivate = lsPrivate cargoListing
        , lrdPatternPublic = \knobs bodies artifacts -> do
            rewritten <- newIORef mempty
            pure (indexStubWithArtifacts rewritten (lkUpstreamLatencyMicros knobs) bodies artifacts)
        }

cargoListing :: ListingSetup
cargoListing =
    ListingSetup
        { lsCopy = cargoPrivateCopy
        , lsPrivate = \knobs -> crateStub (lkUpstreamLatencyMicros knobs) (artifactBytes (lkPayloadBytes knobs))
        , lsCapturePackages = id
        , lsPreflight = preflightListings
        , lsEvictionEntries = evictionEntries . lkCacheMaxEntries
        , lsRevalidateError = "cargo/revalidate-not-modified: no URL to drive"
        }

-- | State the effective eviction size and the limits of the service-time attribution.
cargoLoadNotes :: LoadKnobs -> Text
cargoLoadNotes knobs =
    "Captured index rows retain their source fields. HTTP relay fixtures replace only checksums with hashes of owned payloads and preserve wire sizes. Only the private-copy rows read durable mirror state, which records the private rows that match their public source and verifies their archive bytes. These payloads model relays, not native client installation.\n\nService attribution subtracts one public baseline. Extra private or public hops remain in reported overhead, which is not intrinsic proxy CPU time.\n\n"
        <> "Cache working set: "
        <> show count
        <> " projects. Eviction entry bound: "
        <> either id show (evictionEntries (lkCacheMaxEntries knobs) count)
        <> " (configured "
        <> show (lkCacheMaxEntries knobs)
        <> ").\n"
  where
    count = length (listingWorkingSet cargoListing knobs)

indexColdScenario :: Scenario
indexColdScenario =
    indexScenario "index-cold" "GET the weighted sparse-index corpus with public cache TTL 0. Each request merges a live private overlay and filters releases. Concurrent public misses share an in-flight fetch and decode." 0

indexScenario :: Text -> Text -> Int -> Scenario
indexScenario name description ttl =
    scenario name description (listingLoad cargoListing ttl)

-- | The Cargo parts of the private-copy scenarios.
cargoPrivateCopy :: PrivateCopy
cargoPrivateCopy =
    PrivateCopy
        { pcEcosystem = Cargo
        , pcListing = "GET the weighted sparse-index corpus"
        , pcRegistry = "a private index that proxies Cargo"
        , pcPackages = cargoCorpusPackages
        , pcCut = newestCargoShare
        , pcStub = \latency bodies -> do
            rewritten <- newIORef mempty
            pure (indexStub rewritten latency copyBytes bodies)
        , pcUrl = indexUrl
        , pcPreflight = traverse_ (void . checkedIndex) . ordNub
        , pcHerdPackage = herdPackage
        , pcManagedState = Just (ManagedCopyState withCopyState)
        }

revalidateScenario :: Scenario
revalidateScenario =
    scenario
        "revalidate-not-modified"
        "GET the heaviest sparse index with a primed If-None-Match validator. The private index and admission plan remain live, but 304 avoids assembly and encoding."
        (revalidateLoad cargoListing)

cacheFitsScenario :: Scenario
cacheFitsScenario =
    scenario
        "cache-fits-large"
        "GET a uniform sparse-index working set with one cache slot per project, so assembled responses fit by entry count. Full public metadata is always fetched."
        (cacheFitsLoad cargoListing)

cacheEvictsScenario :: Scenario
cacheEvictsScenario =
    scenario
        "cache-evicts-large"
        "GET the same uniform sparse-index working set with min(configured entries, project count - 1) cache slots, forcing repeated assembly. Full public metadata is always fetched."
        (cacheEvictsLoad cargoListing)

data CrateSource = PrivateCrate | PublicOnboarding

crateScenario :: CrateSource -> Scenario
crateScenario source =
    scenario name description $ \knobs k -> do
        let bytes = artifactBytes (lkPayloadBytes knobs)
            latency = lkUpstreamLatencyMicros knobs
            (privateApp, publicApp, ttl) = case source of
                PrivateCrate -> (crateStub latency bytes, missingStub latency, longCacheTtl)
                PublicOnboarding -> (missingStub latency, crateStub latency bytes, 0)
        withProxyOverStubs Cargo knobs ttl Nothing privateApp publicApp crateMix $ \proxy urls -> do
            for_ (zip cargoCorpusPackages urls) $ \(package, url) -> checkCrate bytes (cpName package) url
            httpTarget k proxy urls
  where
    (name, description) = case source of
        PrivateCrate ->
            ( "crate-hot-path"
            , "GET a crate through the private index and relay its bytes. Discovery, the private index and the archive each incur an upstream wait. The public upstream serves nothing."
            )
        PublicOnboarding ->
            ( "crate-onboarding"
            , "GET a crate after private discovery and an index 404, public discovery, admission, and archive fetch. Public cache TTL 0 preserves admission work under load, with concurrent misses coalesced. No mirror job runs."
            )

crateCeilingScenario :: Scenario
crateCeilingScenario =
    (crateScenario PrivateCrate)
        { scenarioName = "crate-ceiling"
        , scenarioDescription = "Measure the private crate relay at four times the base concurrency under load, one connection for service time, and 2 ms upstream latency. The payload knob sets the streamed body size."
        , scenarioConcurrencyScale = 4
        , scenarioUpstreamLatencyMicros = Just 2_000
        , scenarioPublicLegUnavailableReason = Just "the private-hit artifact relay has no public leg"
        }

herdScenario :: Scenario
herdScenario =
    ( scenario
        "herd"
        ("Send " <> show herdSize <> " simultaneous cold GET /cargo/" <> cargoShard herdPackage <> " to an idle proxy, once, with public cache TTL 0 and no warm-up. Any admission that reacts to measured memory sees this burst late.")
        ( \knobs k ->
            withUnprimedIndexProxy knobs 0 Nothing cargoCorpusPackages (\port -> [indexUrl port herdPackage]) $ \proxy -> \case
                url : _ -> k (proxied proxy (DriveBurst herdSize url))
                [] -> benchFail "cargo/herd: no URL to drive"
        )
    )
        { scenarioServiceTime = False
        }

herdPackage :: Text
herdPackage = "aws-sdk-ec2"

warmUnderColdScenario :: Scenario
warmUnderColdScenario =
    (scenario "warm-under-cold" description bootWarm){scenarioServiceTime = False}
  where
    description = "Measure warm listings and selected reads outside the heavy tier while cold clients read the heavy native index. Each heavy public response carries a changed additive field, which prevents assembled-response reuse."
    bootWarm knobs use = do
        changes <- newIORef (0 :: Int)
        pairedLoad
            WarmLoadDriver
                { wldEcosystem = Cargo
                , wldPackages = cargoCorpusPackages
                , wldListingUrl = indexUrl
                , wldArtifactUrl = \port artifact -> localhost port <> "/cargo/" <> saProxyPath artifact
                , wldSelect = selectWarmArtifacts
                , wldPrivate = const crateStub
                , wldPublic = changingIndexStub changes
                }
            knobs
            use

changingIndexStub :: IORef Int -> IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
changingIndexStub changes rewritten latency captures artifacts =
    let fallback = indexStubWithArtifacts rewritten latency captures artifacts
        payload = fromMaybe copyBytes (listToMaybe (Map.elems artifacts))
        heavy = Map.lookup herdPackage captures <&> relayBody payload
     in \request respond -> case shardName (filter (not . T.null) (pathInfo request)) of
            Just name
                | name == herdPackage
                , Just captured <- heavy -> do
                    bytes <- either benchFail pure captured
                    revision <- atomicModifyIORef' changes (\count -> (count + 1, count))
                    let (firstRow, remaining) = LBS.break (== 10) bytes
                    fields <- either (benchFail . toText) pure (eitherDecodeStrict (toStrict firstRow))
                    let varied = encode (withKeys ["ecluse-load-revision" .= revision] fields) <> remaining
                    delayUpstream latency
                    respond (responseLBS status200 [(hContentType, "text/plain")] varied)
            _ -> fallback request respond

rampScenario :: Scenario
rampScenario =
    ( scenario
        "ramp"
        ("GET the weighted sparse-index corpus with public cache TTL 0, holding " <> T.intercalate ", " (map show rampSteps) <> " connections in turn for the configured duration each. Successes should level off past saturation, not fall.")
        ( \knobs k ->
            withUnprimedIndexProxy knobs 0 Nothing cargoCorpusPackages (weightedMix cpWeight indexUrl cargoCorpusPackages) $ \proxy urls ->
                k (proxied proxy (DriveRamp rampSteps (urlLoad urls)))
        )
    )
        { scenarioServiceTime = False
        }

-- | Resolve captured native pins through the production full read before load starts.
selectWarmArtifacts :: [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
selectWarmArtifacts packages captures = do
    discovery <- either (benchFail . show) pure cargoCaptureDiscovery
    selectPinnedArtifacts "cargoPins" (select discovery) packages captures
  where
    select discovery package version bytes = do
        captured <- captureManifest (cargoRead discovery) cargoCaptureUpstream (cpPackage package) [bytes]
        pure $ do
            manifest <- first show captured
            let route = maybeToRight "Cargo workload pin has no archive route" (cargoArtifactPath (cpPackage package) (mkVersion Cargo version))
            selectedArtifact version ("Cargo workload pin absent: " <> cpName package <> "@" <> version) (const route) (manifestInfo manifest)

preflightListings :: [Text] -> IO ()
preflightListings = traverse_ (void . checkedIndex) . ordNub

withUnprimedIndexProxy :: LoadKnobs -> Int -> Maybe Int -> [CorpusPackage] -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withUnprimedIndexProxy = withListingStubs cargoListing

indexUrl :: Int -> Text -> Text
indexUrl port name = localhost port <> "/cargo/" <> if T.null name then "" else cargoShard name

-- Private and public crates share the one merged registry, so the source selects only the stubs.
crateMix :: Int -> [Text]
crateMix port = [localhost port <> "/cargo/crates/" <> cpName package <> "/9999.0.2/" <> crateFilename (cpName package) | package <- cargoCorpusPackages]

crateFilename :: Text -> Text
crateFilename name = name <> "-9999.0.2.crate"

copyBytes :: LByteString
copyBytes = "Owned Cargo listing-copy relay bytes"

indexStub :: IORef (Map Text LByteString) -> Int -> LByteString -> Map Text LByteString -> Application
indexStub _ latency payload captures = \request respond -> do
    delayUpstream latency
    case filter (not . T.null) (pathInfo request) of
        ["config.json"] -> respond (discoveryResponse request)
        ["crates", name, filename] | Just versionKeys <- Map.lookup name capturedVersions -> do
            versions <- either benchFail pure versionKeys
            respond $
                if filename `elem` [name <> "-" <> version <> ".crate" | version <- versions]
                    then responseLBS status200 [(hContentType, "application/octet-stream")] payload
                    else responseLBS status404 [] ""
        segments
            | Just name <- shardName segments
            , Just relayed <- Map.lookup name capturedBodies -> do
                bytes <- either benchFail pure relayed
                respond (responseLBS status200 [(hContentType, "text/plain")] bytes)
        _ -> respond (responseLBS status404 [] "")
  where
    capturedVersions = Map.map (cargoVersionKeys . LBS.toStrict) captures
    capturedBodies = Map.map (relayBody payload) captures

relayBody :: LByteString -> LByteString -> Either Text LByteString
relayBody payload = rewriteLines (\_ line -> replaceLine line)
  where
    digest = hexSha256Of (LBS.toStrict payload)
    replaceLine bytes
        | BS.all (`elem` [9, 13, 32]) bytes = Right bytes
        | otherwise = do
            fields <- case referenceIndex bytes of
                [line] -> indexValue line
                _ -> Left "Cargo relay fixture has ambiguous native framing"
            previous <- first toText (parseEither (.: "cksum") fields)
            swapChecksumPreservingWidth "Cargo" ("\"cksum\":\"" <> previous <> "\"") ("\"cksum\":\"" <> digest <> "\"") bytes

crateStub :: Int -> LByteString -> Application
crateStub = crateStubWith (const (pure []))

-- | Preserve native records while varying a retained additive field for cold response assembly.
crateStubWith :: (Text -> IO [Pair]) -> Int -> LByteString -> Application
crateStubWith extraFields latency bytes request respond = do
    delayUpstream latency
    case filter (not . T.null) (pathInfo request) of
        ["config.json"] -> respond (discoveryResponse request)
        ["crates", name, filename] | knownName name && filename == crateFilename name -> respond (responseLBS status200 [(hContentType, "application/octet-stream")] bytes)
        segments | Just name <- shardName segments -> do
            extra <- extraFields name
            respond (responseLBS status200 [(hContentType, "text/plain")] (encode (withKeys extra (crateRecord name bytes)) <> "\n"))
        _ -> respond (responseLBS status404 [] "")

-- | Selected archive bytes share the native discovery template used by captured listings.
indexStubWithArtifacts :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
indexStubWithArtifacts rewritten latency captures artifacts =
    let payload = fromMaybe copyBytes (listToMaybe (Map.elems artifacts))
        fallback = indexStub rewritten latency payload captures
     in \request respond -> case Map.lookup (rawPathInfo request) artifacts of
            Just bytes -> delayUpstream latency >> respond (responseLBS status200 [(hContentType, "application/octet-stream")] bytes)
            Nothing -> fallback request respond

missingStub :: Int -> Application
missingStub latency request respond = do
    delayUpstream latency
    respond $ case filter (not . T.null) (pathInfo request) of
        ["config.json"] -> discoveryResponse request
        _ -> responseLBS status404 [] ""

discoveryResponse :: Request -> Response
discoveryResponse request = responseLBS status200 [(hContentType, "application/json")] (LBS.fromStrict (renderCargoDiscovery (selfBaseUrl request <> "/crates/{crate}/{crate}-{version}.crate") Nothing False))

knownName :: Text -> Bool
knownName name = name `elem` map cpName cargoCorpusPackages

shardName :: [Text] -> Maybe Text
shardName segments = find (\name -> T.splitOn "/" (cargoShard name) == segments) (map cpName cargoCorpusPackages)

crateRecord :: Text -> LByteString -> Value
crateRecord name bytes =
    object
        [ "name" .= name
        , "vers" .= ("9999.0.2" :: Text)
        , "deps" .= ([] :: [Value])
        , "features" .= object []
        , "cksum" .= hexSha256Of (LBS.toStrict bytes)
        , "pubtime" .= ("2020-01-01T00:00:00Z" :: Text)
        , "yanked" .= False
        ]

checkedIndex :: Text -> IO [Object]
checkedIndex url = do
    response <- fetchChecked status200 [] url
    let rows = referenceIndex (LBS.toStrict (HTTP.responseBody response))
    fields <- traverse (either benchFail pure . indexValue) rows
    unless (not (null fields) && all (KeyMap.member "cksum") fields) (benchFail ("Cargo index preflight returned empty or malformed records: " <> url))
    discovery <- fetchChecked status200 [] (registryRoot url <> "config.json")
    (template, _, _, _) <- either benchFail pure (referenceDiscovery (LBS.toStrict (HTTP.responseBody discovery)))
    unless ("https://bench.proxy/cargo/crates/" `T.isPrefixOf` template) (benchFail "Cargo discovery bypassed the proxy archive route")
    pure fields

-- The proxy's registry root, up to and including /cargo/.
registryRoot :: Text -> Text
registryRoot = fst . T.breakOnEnd "/cargo/"

checkCrate :: LByteString -> Text -> Text -> IO ()
checkCrate bytes name url = do
    response <- fetchChecked status200 [] url
    unless (HTTP.responseBody response == bytes) (benchFail ("Cargo archive bytes differ: " <> url))
    fields <- checkedIndex (registryRoot url <> cargoShard name)
    records <- traverse (either (benchFail . toText) pure . parseEither (\row -> (,,) <$> row .: "name" <*> (row .: "vers" :: Parser Text) <*> row .: "cksum")) fields
    unless (records == [(name, "9999.0.2", hexSha256Of (LBS.toStrict bytes))]) (benchFail "Cargo archive preflight index identity or checksum differs")

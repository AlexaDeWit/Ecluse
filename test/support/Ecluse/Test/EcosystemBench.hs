-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Registered performance inputs for the shipped registry adapters.
Corpus loading reads each capture through the production read driver before a harness measures work.
-}
module Ecluse.Test.EcosystemBench (
    module Ecluse.Test.EcosystemBench.Types,
    ecosystemBenches,
    ecosystemBenchFor,
    benchmarkEcosystems,
    checkBenchmarkCoverage,
) where

import Prelude hiding (universe)

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Universe.Class (universe)
import Network.HTTP.Client (Request)
import Network.HTTP.Types (Method, methodGet, methodPost, methodPut)

import Ecluse.Core.Credential (ClientCredential, bareCredential, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo, PackageName, artUrl, infoVersions, pkgArtifacts, renderPackageName, unscopedName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl), AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, npmCached, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactRequest, cargoConfigRequest, cargoIndexRequest)
import Ecluse.Core.Registry.Cargo.Route.Internal (cargoRoutes)
import Ecluse.Core.Registry.Cargo.Serving (assembleCargoDocument, serialiseCargoDocument)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Registry.Npm.Route.Internal (npmRoutes)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Metadata (pypiRead)
import Ecluse.Core.Registry.PyPI.Project (fcVersionKey, filenameMemo, readCoordinate)
import Ecluse.Core.Registry.PyPI.Route.Internal (pypiRoutes)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifFilename), SimpleIndex (siFiles))
import Ecluse.Core.Registry.Request (BaseRequest, artifactRequestByUrl, mkBaseRequest, sealRequest)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith, prepareGemRead)
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, mkCoordinate, mkGemPlatform)
import Ecluse.Core.Registry.RubyGems.Request (gemArtifactUrl, gemCompactSpecUrl)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRoutes)
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpTier), cargoCaptureUpstream, cargoCorpusPackages, corpusPackages, cpName, npmCaptureUpstream, pypiCaptureUpstream, pypiCorpusPackages, readCapture, rubyGemsCaptureUpstream, rubyGemsCorpusPackages, syntheticProxyBase)
import Ecluse.Test.Corpus.Cargo qualified as Cargo
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Corpus.PyPI (benchProject, syntheticIndexBytes)
import Ecluse.Test.Corpus.RubyGems qualified as RubyGems
import Ecluse.Test.Corpus.Subset (heavyCargoBase, heavyNpmBase, heavyPyPIBase, keepCargoVersions, keepNpmVersions, keepPyPIVersions)
import Ecluse.Test.EcosystemBench.Types
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead, projectCargoIndex)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, captureManifest, captureVersion)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..), renderingOf)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmFull, projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parseVersionBytes)
import Ecluse.Test.Registry.PyPI (separatorHeavySdist)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, projectPyPIIndex, simpleValue)
import Ecluse.Test.Registry.PyPI.Project (readThrough)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Security.Limits (checkNestingDepth)
import Ecluse.Test.Snapshot (readDetails)

-- | Load every registered corpus, failing on missing, malformed, or empty metadata.
ecosystemBenches :: IO [EcosystemBench]
ecosystemBenches = do
    either (fail . toString) pure (checkBenchmarkCoverage benchmarkEcosystems)
    traverse (uncurry loadEcosystem) benchmarkRegistrations

-- | Provider identities without loading corpus files or running measured operations.
benchmarkEcosystems :: [Ecosystem]
benchmarkEcosystems = map (ebEcosystem . fst) benchmarkRegistrations

-- | Compare providers with both reviewed obligations and the production adapter registry.
checkBenchmarkCoverage :: [Ecosystem] -> Either Text ()
checkBenchmarkCoverage actual = do
    checkProviderCoverage actual
    unless (sort actual == sort production) (Left ("measurement providers differ from production adapters: " <> show production))
  where
    production = filter (isJust . adapterFor) universe

checkProviderCoverage :: [Ecosystem] -> Either Text ()
checkProviderCoverage actual =
    unless (sort actual == sort required) (Left ("measurement providers differ from required ecosystems: " <> show actual))
  where
    required = [Cargo, Npm, PyPI, RubyGems]

-- | The explicit provider registry, without loading captures or running measured operations.
ecosystemBenchFor :: Ecosystem -> Maybe EcosystemBench
ecosystemBenchFor = \case
    Npm -> Just npmBench
    PyPI -> Just pypiBench
    Cargo -> Just cargoBench
    RubyGems -> Just rubyGemsBench

benchmarkRegistrations :: [(EcosystemBench, [CorpusPackage])]
benchmarkRegistrations = [(npmBench, corpusPackages), (pypiBench, pypiCorpusPackages), (cargoBench, cargoCorpusPackages), (rubyGemsBench, rubyGemsCorpusPackages)]

loadEcosystem :: EcosystemBench -> [CorpusPackage] -> IO EcosystemBench
loadEcosystem ecosystem packages = do
    when (null packages) (fail ("empty measurement corpus for " <> show (ebEcosystem ecosystem)))
    entries <- traverse loadOne (sortOn cpTier (reverse packages))
    pure ecosystem{ebCorpus = entries}
  where
    loadOne package = do
        raw <- readCapture package
        manifest <- ebRead ecosystem (cpPackage package) (captureChunks raw) >>= either (fail . failure package . show) pure
        when (Map.null (infoVersions (manifestInfo manifest))) (fail (failure package "projected to zero versions"))
        decoded <- either (fail . failure package . toString) pure (ebDecode ecosystem (cpPackage package) raw)
        when (null decoded) (fail (failure package "decoded to zero versions"))
        pure (LoadedEntry package raw (manifestInfo manifest) (Snapshot (manifestDigest manifest) (manifestRaw manifest)))
    failure package reason = "corpus capture " <> toString (cpName package) <> ": " <> reason

npmBench :: EcosystemBench
npmBench =
    EcosystemBench
        { ebEcosystem = Npm
        , ebCorpus = []
        , ebUpstream = npmCaptureUpstream
        , ebOrigin = npmOrigin
        , ebTreeRead = \name -> fmap (second (fst npmCached)) . projectNpmManifest defaultLimits name
        , ebTreeDocument = (fst npmCached <$>) . snd npmCached
        , ebKeepVersions = const keepNpmVersions
        , ebHeavyBase = heavyNpmBase
        , ebSynthetic = syntheticPackumentBytes
        , ebSyntheticName = benchPackageName
        , ebDecode = \_ -> first show . fmap (map renderVersion) . parseVersionBytes
        , ebProject = projectNpmFull defaultLimits
        , ebCheckServed = checkProjectedServed (projectNpmFull defaultLimits) (projectNpmFull defaultLimits)
        , ebRead = captureManifest npmRead npmCaptureUpstream
        , ebSelective = \name version -> fmap (fmap readDetails) . captureVersion npmRead npmCaptureUpstream name version
        , ebReadDocument = readDocument (fst npmCached)
        , ebNestingDepth = nestingDepth (snd npmCached)
        , ebMetadata = renderingOf (adapterMetadata npmAdapter)
        , ebReader = Right npmRead
        , ebReadFactory = metadataRead (adapterMetadata npmAdapter)
        , ebRoutes = [RouteCase "mixed requests" (concat (replicate 1000 npmRequests))]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute npmRoutes method [] segments)
        , ebActionRoutes = [RouteCase "tarball action" (replicate 1000 (methodGet, ["express", "-", "express-4.18.2.tgz"]))]
        , ebAction = \(method, segments) -> snd <$> matchRoute npmRoutes method [] segments
        , ebRouteScaling = []
        , ebRequestBuilds =
            [ RequestBuild "artifact request" (\name -> NpmRequest.artifactRequestByFile npmBase benchCredential name (unscopedName name <> "-1.0.0.tgz"))
            , RequestBuild "metadata request" (originRequest npmRead npmBase)
            ]
        }

pypiBench :: EcosystemBench
pypiBench =
    EcosystemBench
        { ebEcosystem = PyPI
        , ebCorpus = []
        , ebUpstream = pypiCaptureUpstream
        , ebOrigin = pypiOrigin
        , ebTreeRead = \name -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name
        , ebTreeDocument = (fst pypiSimpleCached <$>) . snd pypiSimpleCached
        , ebKeepVersions = keepPyPIVersions
        , ebHeavyBase = heavyPyPIBase
        , ebSynthetic = syntheticIndexBytes
        , ebSyntheticName = benchProject
        , ebDecode = \name raw -> pypiReleaseKeys name . siFiles <$> first toText (eitherDecodeStrict raw)
        , ebProject = \name -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name
        , ebCheckServed = checkProjectedServed (\name -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name) (\name -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name)
        , ebRead = captureManifest pypiRead pypiCaptureUpstream
        , ebSelective = \name version -> fmap (fmap readDetails) . captureVersion pypiRead pypiCaptureUpstream name version
        , ebReadDocument = readDocument (fst pypiSimpleCached . documentFromValue)
        , ebNestingDepth = nestingDepth (fmap simpleValue . snd pypiSimpleCached)
        , ebMetadata = renderingOf (adapterMetadata pypiAdapter)
        , ebReader = Right pypiRead
        , ebReadFactory = metadataRead (adapterMetadata pypiAdapter)
        , ebRoutes =
            [ RouteCase "mixed requests" (concat (replicate 1000 pypiRequests))
            , RouteCase "normal sdist" [distribution "requests-2.34.2.tar.gz"]
            , RouteCase "normal wheel" [distribution "requests-2.34.2-py3-none-any.whl"]
            ]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute pypiRoutes method [] segments)
        , ebActionRoutes = [RouteCase "wheel action" (replicate 1000 (distribution "requests-2.34.2-py3-none-any.whl"))]
        , ebAction = \(method, segments) -> snd <$> matchRoute pypiRoutes method [] segments
        , ebRouteScaling =
            [ RouteScaling "malformed filename separators" (\count -> distribution (separatorHeavySdist "requests" (fromIntegral count) "benchmark"))
            , RouteScaling "valid filename separators" (\count -> distribution ("requests" <> T.replicate (fromIntegral count) "_" <> "1.tar.gz"))
            ]
        , ebRequestBuilds =
            [ RequestBuild "index request" (originRequest pypiRead pypiBase)
            , RequestBuild "artifact request" (artifactByUrl (adapterArtifact pypiAdapter) benchCredential . wheelUrl)
            ]
        }

cargoBench :: EcosystemBench
cargoBench =
    EcosystemBench
        { ebEcosystem = Cargo
        , ebCorpus = []
        , ebUpstream = cargoCaptureUpstream
        , ebOrigin = "https://index.crates.io"
        , ebTreeRead = Cargo.projectCargoTree defaultLimits
        , ebTreeDocument = (fst cargoCached <$>) . snd cargoCached
        , ebKeepVersions = const keepCargoVersions
        , ebHeavyBase = heavyCargoBase
        , ebSynthetic = Cargo.syntheticIndexBytes
        , ebSyntheticName = Cargo.benchCrate
        , ebDecode = const Cargo.cargoVersionKeys
        , ebProject = \name bytes -> Cargo.cargoCaptureDiscovery >>= \discovery -> projectCargoIndex discovery defaultLimits name bytes
        , ebCheckServed = checkProjectedServed (\name bytes -> Cargo.cargoCaptureDiscovery >>= \discovery -> projectCargoIndex discovery defaultLimits name bytes) (\name bytes -> Cargo.cargoServedDiscovery >>= \discovery -> projectCargoIndex discovery defaultLimits name bytes)
        , ebRead = \name chunks -> either (pure . Left) (\nativeRead -> captureManifest nativeRead cargoCaptureUpstream name chunks) capturedCargoRead
        , ebSelective = \name version chunks -> either (pure . Left) (\nativeRead -> fmap (fmap readDetails) (captureVersion nativeRead cargoCaptureUpstream name version chunks)) capturedCargoRead
        , ebReadDocument = fmap snd . first show . Cargo.projectCargoTree defaultLimits Cargo.benchCrate
        , ebNestingDepth = maybe (-1) (either (const (-1)) (const 1) . traverse (checkNestingDepth defaultLimits . snd) . cargoRecords) . snd cargoCached
        , ebMetadata = MetadataRendering assembleCargoDocument serialiseCargoDocument
        , ebReader = capturedCargoRead
        , ebReadFactory = prepareCargoRead
        , ebRoutes = [RouteCase "mixed requests" (concat (replicate 1000 cargoRequests))]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute cargoRoutes method [] segments)
        , ebActionRoutes = [RouteCase "crate action" (replicate 1000 (methodGet, ["crates", "bench-crate", "1.0.0", "bench-crate-1.0.0.crate"]))]
        , ebAction = \(method, segments) -> snd <$> matchRoute cargoRoutes method [] segments
        , ebRouteScaling = [RouteScaling "malformed index name" (\count -> (methodGet, ["be", "nc", "bench" <> T.replicate (fromIntegral count) "-"]))]
        , ebRequestBuilds =
            [ RequestBuild "discovery request" (const (cargoConfigRequest cargoBase benchCredential))
            , RequestBuild "index request" (cargoIndexRequest cargoBase benchCredential)
            , RequestBuild "archive request" (cargoArtifactRequest benchCredential . (\name -> "https://static.crates.io/crates/" <> renderPackageName name <> "/" <> renderPackageName name <> "-1.0.0.crate"))
            ]
        }
  where
    capturedCargoRead = cargoRead <$> Cargo.cargoCaptureDiscovery
    cargoBase = mkBaseRequest (unsafeRegistryUrl "https://index.crates.io")

rubyGemsBench :: EcosystemBench
rubyGemsBench =
    EcosystemBench
        { ebEcosystem = RubyGems
        , ebCorpus = []
        , ebUpstream = rubyGemsCaptureUpstream
        , ebOrigin = rubyGemsOrigin
        , ebTreeRead = RubyGems.projectRubyGemsTree defaultLimits
        , ebTreeDocument = (fst rubyGemsCached <$>) . snd rubyGemsCached
        , ebKeepVersions = const RubyGems.keepRubyGemsVersions
        , ebHeavyBase = RubyGems.heavyRubyGemsBase
        , ebSynthetic = RubyGems.syntheticInfoBytes
        , ebSyntheticName = RubyGems.benchGem
        , ebDecode = const RubyGems.rubyGemsVersionKeys
        , ebProject = RubyGems.projectRubyGemsInfo rubyGemsOrigin defaultLimits
        , ebCheckServed = checkRubyGemsServed
        , ebRead = captureManifest rubyGemsRead rubyGemsCaptureUpstream
        , ebSelective = \name version -> fmap (fmap readDetails) . captureVersion rubyGemsRead rubyGemsCaptureUpstream name version
        , ebReadDocument = first show . fmap snd . RubyGems.projectRubyGemsTree defaultLimits RubyGems.benchGem
        , ebNestingDepth = maybe (-1) (const 1) . snd rubyGemsCached
        , ebMetadata = MetadataRendering assembleRubyGemsDocument serialiseRubyGemsDocument
        , ebReader = Right rubyGemsRead
        , ebReadFactory = prepareGemRead
        , ebRoutes = [RouteCase "mixed requests" (concat (replicate 1000 rubyGemsRequests))]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute rubyGemsRoutes method [] segments)
        , ebActionRoutes =
            [ RouteCase "gem action" (replicate 1000 (methodGet, ["gems", "bench-gem-1.0.0.gem"]))
            , RouteCase "quick specification action" (replicate 1000 (methodGet, ["quick", "Marshal.4.8", "bench-gem-1.0.0.gemspec.rz"]))
            , RouteCase "native info action" (replicate 1000 (methodGet, ["info", "bench-gem"]))
            ]
        , ebAction = \(method, segments) -> snd <$> matchRoute rubyGemsRoutes method [] segments
        , ebRouteScaling = [RouteScaling "malformed native coordinate" (\count -> (methodGet, ["quick", "Marshal.4.8", "bench" <> T.replicate (fromIntegral count) "-" <> ".gemspec.rz"]))]
        , ebRequestBuilds =
            [ RequestBuild "metadata request" (originRequest rubyGemsRead rubyGemsBase)
            , RequestBuild "artifact request" (nativeCoordinateRequest gemArtifactUrl)
            , RequestBuild "compact specification request" (nativeCoordinateRequest gemCompactSpecUrl)
            ]
        }

rubyGemsOrigin :: Text
rubyGemsOrigin = "https://rubygems.org"

rubyGemsRead :: EcosystemRead
rubyGemsRead = gemReadWith (const pass) (const pass) rubyGemsOrigin

rubyGemsBase :: BaseRequest
rubyGemsBase = mkBaseRequest (unsafeRegistryUrl rubyGemsOrigin)

nativeCoordinateRequest :: (Text -> GemCoordinate -> Either UrlFormationError Text) -> PackageName -> Either UrlFormationError Request
nativeCoordinateRequest location name = do
    coordinate <- first (const (UnparseableUrl "invalid native benchmark coordinate")) (mkGemPlatform "ruby" >>= mkCoordinate name (mkVersion RubyGems "1.0.0"))
    url <- location rubyGemsOrigin coordinate
    artifactRequestByUrl rubyReadCredential benchCredential url

rubyGemsRequests :: [(Method, [Text])]
rubyGemsRequests =
    [ (methodGet, ["versions"])
    , (methodGet, ["api", "v1", "dependencies"])
    , (methodGet, ["quick", "Marshal.4.8", "bench-gem-1.0.0.gemspec.rz"])
    , (methodGet, ["gems", "bench-gem-1.0.0.gem"])
    , (methodPost, ["api", "v1", "gems"])
    , (methodGet, ["info", "bench-gem"])
    , (methodGet, [])
    ]

cargoRequests :: [(Method, [Text])]
cargoRequests =
    [ (methodGet, ["config.json"])
    , (methodGet, ["to", "ki", "tokio"])
    , (methodGet, ["wi", "nd", "windows"])
    , (methodGet, ["crates", "tokio", "1.0.0", "tokio-1.0.0.crate"])
    , (methodPut, ["api", "v1", "crates", "new"])
    , (methodGet, ["bad", "prefix", "tokio"])
    , (methodGet, [])
    ]

npmOrigin :: Text
npmOrigin = "https://registry.npmjs.org"

pypiOrigin :: Text
pypiOrigin = "https://pypi.org"

-- The bases a mount parses once, shared by every request a batch builds.
npmBase :: BaseRequest
npmBase = mkBaseRequest (unsafeRegistryUrl npmOrigin)

pypiBase :: BaseRequest
pypiBase = mkBaseRequest (unsafeRegistryUrl pypiOrigin)

-- A private read presents the caller's credential, which the build attaches.
benchCredential :: Maybe ClientCredential
benchCredential = Just (bareCredential (mkSecret "bench-token"))

-- The whole-document request a metadata read seals before it sends.
originRequest :: EcosystemRead -> BaseRequest -> PackageName -> Either UrlFormationError Request
originRequest nativeRead base name = sealRequest <$> erRequest nativeRead base benchCredential name

-- A wheel location as the index lists it, which the artifact read fetches by URL.
wheelUrl :: PackageName -> Text
wheelUrl name = "https://files.pythonhosted.org/packages/a0/" <> renderPackageName name <> "-2.34.2-py3-none-any.whl"

-- The release keys the files name, in the order met, read through the memo a full read keeps.
pypiReleaseKeys :: PackageName -> [IndexFile] -> [Text]
pypiReleaseKeys name = ordNub . mapMaybe (fmap fcVersionKey) . snd . readThrough readCoordinate (filenameMemo name) . map ifFilename

readDocument :: (Value -> CachedDoc) -> ByteString -> Either Text CachedDoc
readDocument inject = fmap inject . first toText . eitherDecodeStrict

nestingDepth :: (CachedDoc -> Maybe Value) -> CachedDoc -> Int
nestingDepth project document =
    maybe (-1) (either (const (-1)) (const 1) . checkNestingDepth defaultLimits) (project document)

routeDepth :: Maybe (Route v, a) -> Int
routeDepth = maybe 0 (nameLength . routeName . fst)
  where
    nameLength (RouteName name) = T.length name

distribution :: Text -> (Method, [Text])
distribution file = (methodGet, ["simple", "requests", file])

npmRequests :: [(Method, [Text])]
npmRequests =
    [ (methodGet, ["express"])
    , (methodGet, ["lodash"])
    , (methodGet, ["@babel", "core"])
    , (methodGet, ["@types", "node"])
    , (methodGet, ["express", "-", "express-4.18.2.tgz"])
    , (methodGet, ["@babel", "core", "-", "core-7.24.0.tgz"])
    , (methodGet, ["-", "ping"])
    , (methodGet, ["-", "v1", "search"])
    , (methodGet, ["favicon.ico"])
    , (methodGet, [])
    , (methodPut, ["@acme", "widget"])
    , (methodPut, ["express", "-", "express-4.18.2.tgz"])
    ]

pypiRequests :: [(Method, [Text])]
pypiRequests =
    [ (methodGet, ["simple", "requests"])
    , distribution "requests-2.34.2.tar.gz"
    , distribution "requests-2.34.2-py3-none-any.whl"
    , (methodGet, ["simple"])
    , (methodGet, ["favicon.ico"])
    , (methodPut, ["simple", "requests"])
    ]

-- Existing embedded-URL protocols retain count, artifact and proxy-origin checks together.
checkProjectedServed :: (PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)) -> (PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)) -> PackageName -> ByteString -> ByteString -> IO (Either Text ())
checkProjectedServed originalProject servedProject name originalBytes servedBytes = pure $ do
    original <- first show (fst <$> originalProject name originalBytes)
    served <- first show (fst <$> servedProject name servedBytes)
    unless (Map.size (infoVersions served) == Map.size (infoVersions original)) (Left "served release count differs")
    unless (length (artifacts served) == length (artifacts original)) (Left "served artifact count differs")
    unless (all (((syntheticProxyBase <> "/") `T.isPrefixOf`) . artUrl) (artifacts served)) (Left "served artifact URL escaped the proxy origin")
  where
    artifacts info = concatMap (toList . pkgArtifacts) (Map.elems (infoVersions info))

checkRubyGemsServed :: PackageName -> ByteString -> ByteString -> IO (Either Text ())
checkRubyGemsServed _ original served = pure $ do
    expected <- referenceInfo original
    actual <- referenceInfo served
    unless (map installationRow actual == map installationRow expected) (Left "served native installation rows differ from the independent source rows")
  where
    installationRow row = row{rowPosition = 0}

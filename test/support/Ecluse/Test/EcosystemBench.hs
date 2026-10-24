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
import Network.HTTP.Types (Method, methodGet, methodPut)

import Ecluse.Core.Credential (ClientCredential, bareCredential, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageName, infoVersions, renderPackageName, unscopedName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl), AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Registry.Npm.Route.Internal (npmRoutes)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Project (fcVersionKey, filenameMemo, readCoordinate)
import Ecluse.Core.Registry.PyPI.Route.Internal (pypiRoutes)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifFilename), SimpleIndex (siFiles))
import Ecluse.Core.Registry.Request (BaseRequest, mkBaseRequest, sealRequest)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpTier), corpusPackages, cpName, npmCaptureUpstream, pypiCaptureUpstream, pypiCorpusPackages, readCapture)
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Corpus.PyPI (benchProject, syntheticIndexBytes)
import Ecluse.Test.Corpus.Subset (heavyNpmBase, heavyPyPIBase, keepNpmVersions, keepPyPIVersions)
import Ecluse.Test.EcosystemBench.Types
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, captureManifest, captureVersion)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmFull, projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parseVersionBytes)
import Ecluse.Test.Registry.PyPI (separatorHeavySdist)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, projectPyPIIndex, simpleValue)
import Ecluse.Test.Registry.PyPI.Project (readThrough)
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
checkBenchmarkCoverage actual
    | sort actual /= sort required = Left ("measurement providers differ from required ecosystems: " <> show actual)
    | sort actual /= sort production = Left ("measurement providers differ from production adapters: " <> show production)
    | otherwise = Right ()
  where
    required = [Npm, PyPI]
    production = filter (isJust . adapterFor) universe

-- | The explicit provider registry, without loading captures or running measured operations.
ecosystemBenchFor :: Ecosystem -> Maybe EcosystemBench
ecosystemBenchFor = \case
    Npm -> Just npmBench
    PyPI -> Just pypiBench
    RubyGems -> Nothing

benchmarkRegistrations :: [(EcosystemBench, [CorpusPackage])]
benchmarkRegistrations = [(npmBench, corpusPackages), (pypiBench, pypiCorpusPackages)]

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
        , ebRead = captureManifest (adapterMetadata npmAdapter) npmCaptureUpstream
        , ebSelective = \name version -> fmap (fmap readDetails) . captureVersion (adapterMetadata npmAdapter) npmCaptureUpstream name version
        , ebReadDocument = readDocument (fst npmCached)
        , ebNestingDepth = nestingDepth (snd npmCached)
        , ebMetadata = adapterMetadata npmAdapter
        , ebRoutes = [RouteCase "mixed requests" (concat (replicate 1000 npmRequests))]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute npmRoutes method [] segments)
        , ebActionRoutes = [RouteCase "tarball action" (replicate 1000 (methodGet, ["express", "-", "express-4.18.2.tgz"]))]
        , ebAction = \(method, segments) -> snd <$> matchRoute npmRoutes method [] segments
        , ebRouteScaling = []
        , ebRequestBuilds =
            [ RequestBuild "artifact request" (\name -> NpmRequest.artifactRequestByFile npmBase benchCredential name (unscopedName name <> "-1.0.0.tgz"))
            , RequestBuild "metadata request" (originRequest (adapterMetadata npmAdapter) npmBase)
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
        , ebRead = captureManifest (adapterMetadata pypiAdapter) pypiCaptureUpstream
        , ebSelective = \name version -> fmap (fmap readDetails) . captureVersion (adapterMetadata pypiAdapter) pypiCaptureUpstream name version
        , ebReadDocument = readDocument (fst pypiSimpleCached . documentFromValue)
        , ebNestingDepth = nestingDepth (fmap simpleValue . snd pypiSimpleCached)
        , ebMetadata = adapterMetadata pypiAdapter
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
            [ RequestBuild "index request" (originRequest (adapterMetadata pypiAdapter) pypiBase)
            , RequestBuild "artifact request" (artifactByUrl (adapterArtifact pypiAdapter) benchCredential . wheelUrl)
            ]
        }

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
originRequest :: AdapterMetadata -> BaseRequest -> PackageName -> Either UrlFormationError Request
originRequest metadata base name = sealRequest <$> erRequest (metadataRead metadata) base benchCredential name

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

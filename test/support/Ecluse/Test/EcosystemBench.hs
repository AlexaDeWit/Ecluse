-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Registered performance inputs for the shipped registry adapters.
Corpus loading validates the native format before a harness starts measuring work.
-}
module Ecluse.Test.EcosystemBench (
    module Ecluse.Test.EcosystemBench.Types,
    ecosystemBenches,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (Method, methodGet, methodPut)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionRead (vrDetails))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)
import Ecluse.Core.Registry.Npm.Project (parseVersionList)
import Ecluse.Core.Registry.Npm.Route (npmRoutes)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIVersion)
import Ecluse.Core.Registry.PyPI.Project (fileVersionKey)
import Ecluse.Core.Registry.PyPI.Route (pypiRoutes)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifFilename), SimpleIndex (siFiles))
import Ecluse.Core.Security (checkNestingDepth, defaultLimits)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath, cpTier), corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Corpus.PyPI (benchProject, syntheticIndexBytes)
import Ecluse.Test.EcosystemBench.Types
import Ecluse.Test.Registry.PyPI (separatorHeavySdist)

-- | Load every registered corpus, failing on missing, malformed, or empty metadata.
ecosystemBenches :: IO [EcosystemBench]
ecosystemBenches = traverse (uncurry loadEcosystem) [(npmBench, corpusPackages), (pypiBench, pypiCorpusPackages)]

loadEcosystem :: EcosystemBench -> [CorpusPackage] -> IO EcosystemBench
loadEcosystem ecosystem packages = do
    entries <- traverse loadOne (sortOn cpTier (reverse packages))
    pure ecosystem{ebCorpus = entries}
  where
    loadOne package = do
        raw <- readFileBS (cpPath package)
        (info, document) <- either (fail . failure package . show) pure (ebProject ecosystem (cpPackage package) raw)
        when (Map.null (infoVersions info)) (fail (failure package "projected to zero versions"))
        decoded <- either (fail . failure package . toString) pure (ebDecode ecosystem (cpPackage package) raw)
        when (null decoded) (fail (failure package "decoded to zero versions"))
        pure (package, raw, info, document)
    failure package reason = "corpus capture " <> toString (cpName package) <> ": " <> reason

npmBench :: EcosystemBench
npmBench =
    EcosystemBench
        { ebEcosystem = Npm
        , ebCorpus = []
        , ebSynthetic = syntheticPackumentBytes
        , ebSyntheticName = benchPackageName
        , ebDecode = \_ -> first show . fmap (map renderVersion) . parseVersionList . RegistryResponse 200
        , ebProject = \name -> fmap (second (fst npmCached)) . projectNpmManifest defaultLimits name
        , ebSelective = \name version -> fmap vrDetails . projectNpmVersion defaultLimits name version
        , ebReadDocument = readDocument (fst npmCached)
        , ebNestingDepth = nestingDepth (snd npmCached)
        , ebMetadata = adapterMetadata npmAdapter
        , ebRoutes = [RouteCase "mixed requests" (concat (replicate 1000 npmRequests))]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute npmRoutes method [] segments)
        , ebRouteScaling = []
        }

pypiBench :: EcosystemBench
pypiBench =
    EcosystemBench
        { ebEcosystem = PyPI
        , ebCorpus = []
        , ebSynthetic = syntheticIndexBytes
        , ebSyntheticName = benchProject
        , ebDecode = \name raw -> ordNub . mapMaybe (fileVersionKey name . ifFilename) . siFiles <$> first toText (eitherDecodeStrict raw)
        , ebProject = \name -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name
        , ebSelective = projectPyPIVersion defaultLimits
        , ebReadDocument = readDocument (fst pypiSimpleCached)
        , ebNestingDepth = nestingDepth (snd pypiSimpleCached)
        , ebMetadata = adapterMetadata pypiAdapter
        , ebRoutes =
            [ RouteCase "mixed requests" (concat (replicate 1000 pypiRequests))
            , RouteCase "normal sdist" [distribution "requests-2.34.2.tar.gz"]
            , RouteCase "normal wheel" [distribution "requests-2.34.2-py3-none-any.whl"]
            ]
        , ebClassify = \(method, segments) -> routeDepth (matchRoute pypiRoutes method [] segments)
        , ebRouteScaling =
            [ RouteScaling "malformed filename separators" (\count -> distribution (separatorHeavySdist "requests" (fromIntegral count) "benchmark"))
            , RouteScaling "valid filename separators" (\count -> distribution ("requests" <> T.replicate (fromIntegral count) "_" <> "1.tar.gz"))
            ]
        }

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

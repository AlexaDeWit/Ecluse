-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Format-specific inputs and adapter operations for performance harnesses.
Raw corpus bytes cross the interface, while prepared serving uses the production opaque document.
-}
module Ecluse.Test.EcosystemBench.Types (
    EcosystemBench (..),
    LoadedEntry,
    RouteCase (..),
    RouteScaling (..),
) where

import Network.HTTP.Types (Method)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (CorpusPackage)

-- | One ecosystem's measured operations and eagerly loaded, validated corpus.
data EcosystemBench = EcosystemBench
    { ebEcosystem :: Ecosystem
    , ebCorpus :: [LoadedEntry]
    , ebSynthetic :: Int -> ByteString
    -- ^ Positive counts produce that many distinct releases under 'ebSyntheticName'.
    , ebSyntheticName :: PackageName
    , ebDecode :: PackageName -> ByteString -> Either Text [Text]
    -- ^ Decode release keys through the adapter's wire parser.
    , ebProject :: PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
    , ebRead :: PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
    -- ^ The production full read of a capture's chunks, as a fetch from the capture's registry runs it.
    , ebSelective :: PackageName -> Version -> [ByteString] -> IO (Either MetadataError (Maybe PackageDetails))
    -- ^ The production selected read of a capture's chunks, on the same terms.
    , ebReadDocument :: ByteString -> Either Text CachedDoc
    -- ^ Prepare a wire guard's native input outside its measured operation.
    , ebNestingDepth :: CachedDoc -> Int
    , ebMetadata :: AdapterMetadata
    , ebRoutes :: [RouteCase]
    , ebClassify :: (Method, [Text]) -> Int
    , ebRouteScaling :: [RouteScaling]
    }

-- | Validated corpus bytes paired with their neutral projection and native cached document.
type LoadedEntry = (CorpusPackage, ByteString, PackageInfo, CachedDoc)

-- | A named request batch, including the ecosystem's supported and refused paths.
data RouteCase = RouteCase
    { rcName :: String
    , rcRequests :: [(Method, [Text])]
    }

-- | A request family whose length controls the input to a routing complexity check.
data RouteScaling = RouteScaling
    { rsName :: String
    , rsRequest :: Word -> (Method, [Text])
    }

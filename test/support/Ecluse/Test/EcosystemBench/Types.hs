-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Format-specific inputs and adapter operations for performance harnesses.
Raw corpus bytes cross the interface, while prepared serving uses the production opaque document.
-}
module Ecluse.Test.EcosystemBench.Types (
    EcosystemBench (..),
    LoadedEntry (..),
    RequestBuild (..),
    RouteCase (..),
    RouteScaling (..),
) where

import Data.Aeson (Value)
import Network.HTTP.Client (Request)
import Network.HTTP.Types (Method)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Server.Context (RouteAction)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (CaptureUpstream, CorpusPackage)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering)

-- | One ecosystem's measured operations and eagerly loaded, validated corpus.
data EcosystemBench = EcosystemBench
    { ebEcosystem :: Ecosystem
    , ebCorpus :: [LoadedEntry]
    , ebUpstream :: CaptureUpstream
    , ebOrigin :: Text
    -- ^ Cold HTTP reads retain their original base URL.
    , ebTreeRead :: PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
    -- ^ Independent native tree reader, separate from the production read driver.
    , ebTreeDocument :: CachedDoc -> Maybe CachedDoc
    , ebKeepVersions :: PackageInfo -> Set Text -> Value -> Value
    , ebHeavyBase :: Int -> Value -> Value
    , ebSynthetic :: Int -> ByteString
    -- ^ Positive counts produce that many distinct releases under 'ebSyntheticName'.
    , ebSyntheticName :: PackageName
    , ebDecode :: PackageName -> ByteString -> Either Text [Text]
    -- ^ Decode release keys through the adapter's wire parser.
    , ebProject :: PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
    -- ^ The pure projection of a synthetic body, under the fixed test key and with no location check.
    , ebCheckServed :: PackageName -> ByteString -> ByteString -> IO (Either Text ())
    -- ^ Independently check observable served fields outside the timed benchmark operation.
    , ebRead :: PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
    -- ^ The production full read of a capture's chunks, as a fetch from the capture's registry runs it.
    , ebSelective :: PackageName -> Version -> [ByteString] -> IO (Either MetadataError (Maybe PackageDetails))
    -- ^ The production selected read of a capture's chunks, on the same terms.
    , ebReadDocument :: ByteString -> Either Text CachedDoc
    -- ^ Prepare a wire guard's native input outside its measured operation.
    , ebNestingDepth :: CachedDoc -> Int
    , ebMetadata :: MetadataRendering
    , ebReader :: Either MetadataError EcosystemRead
    , ebReadFactory :: PrepareRead
    , ebRoutes :: [RouteCase]
    , ebClassify :: (Method, [Text]) -> Int
    , ebActionRoutes :: [RouteCase]
    , ebAction :: (Method, [Text]) -> Maybe RouteAction
    -- ^ The action the route table names for a request, as dispatch receives it.
    , ebRouteScaling :: [RouteScaling]
    , ebRequestBuilds :: [RequestBuild]
    -- ^ The upstream requests a serve builds for one package, as its origin builds them.
    }

-- | A capture and its production read. Lazy fields preserve the original tuple's evaluation boundaries.
data LoadedEntry = LoadedEntry
    { lePackage :: ~CorpusPackage
    , leBytes :: ~ByteString
    , leInfo :: ~PackageInfo
    , leSnapshot :: ~(Snapshot CachedDoc)
    }

-- | A named request batch, including the ecosystem's supported and refused paths.
data RouteCase = RouteCase
    { rcName :: String
    , rcRequests :: [(Method, [Text])]
    }

-- | One upstream request a serve builds for a package, named for the bench report.
data RequestBuild = RequestBuild
    { rbName :: String
    , rbBuild :: PackageName -> Either UrlFormationError Request
    }

-- | A request family whose length controls the input to a routing complexity check.
data RouteScaling = RouteScaling
    { rsName :: String
    , rsRequest :: Word -> (Method, [Text])
    }

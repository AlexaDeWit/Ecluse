-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Caller-owned npm bytes projected through the production incremental parser.
module Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion, fetchMetadataFormBounded) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), RegistryResponse)
import Ecluse.Core.Registry.Exchange (boundedFetch, formThen)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded), VersionRead)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmStream, selectNpmRead)
import Ecluse.Core.Registry.Npm.Request (MetadataForm, metadataRequest)
import Ecluse.Core.Registry.Npm.Streaming (NpmRead (..), npmFields)
import Ecluse.Core.Registry.Npm.StreamingProjection (collectField, emptyProjection)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), originBaseUrl)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)

-- | Feed held bytes in bounded pieces without adding a second transport body ceiling.
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = snd <$> projectBytes limits name FullRead body

-- | Select one release using the production field policy and timestamp join.
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError VersionRead
projectNpmVersion limits name version body = do
    (size, projected) <- projectBytes limits name (SelectedRead (renderVersion version)) body
    pure (selectNpmRead version size projected)

projectBytes :: Limits -> PackageName -> NpmRead -> ByteString -> Either MetadataError (Int, (PackageInfo, Value))
projectBytes limits name mode body = do
    streamed <-
        first
            MetadataBoundExceeded
            ( parseJsonChunks
                (MetadataBodyLimit (BS.length body))
                (npmFields (maxNestingDepth limits) mode)
                (collectField limits name)
                emptyProjection
                [body]
            )
    projected <- projectNpmStream limits name "https://registry.npmjs.org" streamed
    pure (streamBytes streamed, projected)

{- | Fetch a package's metadata in the requested form.
The body read is bounded fail-closed, and every failure is a 'FetchFault' value, never an exception.
-}
fetchMetadataFormBounded ::
    OriginClient ->
    MetadataForm ->
    PackageName ->
    IO (Either FetchFault RegistryResponse)
fetchMetadataFormBounded origin form name =
    formThen
        FetchUrlUnformable
        (boundedFetch (ocManager origin) (MetadataBodyLimit (maxMetadataBytes (ocLimits origin))))
        (metadataRequest (originBaseUrl origin) (ocToken origin) form name)

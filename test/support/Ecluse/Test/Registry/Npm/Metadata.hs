-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Caller-owned npm bytes projected through the production incremental parser.
module Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest, projectNpmFull, projectNpmVersion, fetchMetadataFormBounded) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), RegistryResponse)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmPacked)
import Ecluse.Core.Registry.Exchange (boundedFetch, formThen)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded), VersionRead)
import Ecluse.Core.Registry.Npm.Metadata (npmPackumentWalk, packedWalk, projectNpmPacked, projectNpmStream, selectNpmRead)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (MetadataForm, metadataRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), originBaseUrl)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (progressFloor), maxMetadataBytes)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Registry.JsonStream (testTable, walkJsonChunks)

-- | Feed held bytes in bounded pieces without adding a second transport body ceiling.
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = snd <$> projectBytes limits name WholePackument body

-- | The production full read of held bytes: every kept release packed against the read's table.
projectNpmFull :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectNpmFull limits name body = do
    streamed <- first MetadataBoundExceeded (walkJsonChunks (MetadataBodyLimit (BS.length body)) (packedWalk limits name registry (testTable releaseUniqueFields)) [body])
    second (fst npmPacked) <$> projectNpmPacked limits name registry streamed
  where
    registry = "https://registry.npmjs.org"

-- | Select one release using the production field policy and timestamp join.
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError VersionRead
projectNpmVersion limits name version body = do
    (size, projected) <- projectBytes limits name (OneRelease (renderVersion version)) body
    pure (selectNpmRead version size projected)

projectBytes :: Limits -> PackageName -> PackumentRead -> ByteString -> Either MetadataError (Int, (PackageInfo, Value))
projectBytes limits name mode body = do
    streamed <-
        first
            MetadataBoundExceeded
            ( walkJsonChunks
                (MetadataBodyLimit (BS.length body))
                (npmPackumentWalk limits name mode (testTable releaseUniqueFields))
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
        (boundedFetch (ocManager origin) (progressFloor (ocLimits origin)) (MetadataBodyLimit (maxMetadataBytes (ocLimits origin))))
        (metadataRequest (originBaseUrl origin) (ocToken origin) form name)

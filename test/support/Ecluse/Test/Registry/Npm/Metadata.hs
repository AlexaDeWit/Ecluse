-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Caller-owned npm bytes projected through the production incremental parser.
module Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest, projectNpmFull, npmFullTestWalk, projectNpmVersion, fetchMetadataFormBounded) where

import Control.Monad.ST (ST)
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor)
import Data.Primitive.PrimVar (newPrimVar)
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), RegistryResponse)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmPacked)
import Ecluse.Core.Registry.Exchange (formThen)
import Ecluse.Core.Registry.Json.Walk (Steps)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded), VersionRead)
import Ecluse.Core.Registry.Npm.Metadata (NpmFullRead, npmFullTable, npmFullWalk, npmPackumentWalk, projectNpmPacked, projectNpmStream, selectNpmRead)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (MetadataForm, metadataRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocClient, ocLimits, ocToken), originBaseUrl)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes, progressFloor))
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Registry.JsonStream (testTable, walkJsonChunks, walkWritingChunks)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)

-- | Feed held bytes in bounded pieces without adding a second transport body ceiling.
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = snd <$> projectBytes limits name WholePackument body

-- | The production full-read walk and its finish over held bytes, under the fixed test key.
projectNpmFull :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectNpmFull limits name body = do
    streamed <- first MetadataBoundExceeded (walkWritingChunks (MetadataBodyLimit (BS.length body)) (npmFullTestWalk limits name registry) [body])
    second (fst npmPacked) <$> projectNpmPacked limits name registry streamed
  where
    registry = "https://registry.npmjs.org"

-- | The production full-read walk for a capture from the registry, over a table under the fixed test key.
npmFullTestWalk :: Limits -> PackageName -> Text -> ST st (Cursor -> ST st (Steps (ST st) NpmFullRead))
npmFullTestWalk limits name registry = do
    (table, writer) <- npmFullTable registry name (testTable releaseUniqueFields)
    entries <- newPrimVar 0
    pure (npmFullWalk entries writer limits name table)

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
        (fetchTestBody (ocClient origin) (progressFloor (ocLimits origin)) (MetadataBodyLimit (maxMetadataBytes (ocLimits origin))))
        (metadataRequest (originBaseUrl origin) (ocToken origin) form name)

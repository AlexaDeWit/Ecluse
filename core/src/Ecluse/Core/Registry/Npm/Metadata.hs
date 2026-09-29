-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The reads specialise here. Full laziness would float each member's rarely taken continuation out
-- of the element's continuation, and every member of a read would allocate it.
{-# OPTIONS_GHC -fno-full-laziness #-}

{- | npm metadata reads for full manifests and selected versions.
Both fetch the full packument because publish-age rules need its @time@ map.
Selective reads materialise only the requested version and timestamp. A version read pairs the
typed projection with the selected version object, which the mirror write republishes.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * Per-request read handle
    newNpmMetadataReads,

    -- * npm full-manifest fetch
    fetchNpmManifest,

    -- * The memory budget
    npmChargeFactors,

    -- * Reading a packument
    readNpmPackument,
    npmPackumentWalk,
    readNpmFull,
    NpmFullRead,
    npmFullWalk,
    npmFullTable,

    -- * Pure projection
    projectNpmStream,
    projectNpmPacked,
    selectNpmRead,
    selectNpmVersionDoc,
) where

import Control.Monad.ST (ST, stToIO)
import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (..), PackageName, renderPackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, npmPacked)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, Interned (..), decodedName, internName, newInternTable, newTableKey, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Shape (Trees (..))
import Ecluse.Core.Registry.Json.Walk (Step, Steps, Walked (..), pureStep, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Registry.Json.Writer (Writer, newWriter)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Npm.Document (PackedPackument)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), metadataRequest, npmArtifactHosts, packageUrl)
import Ecluse.Core.Registry.Npm.StreamingProjection (PackedRead, TreeRead, emptyPackedRead, emptyTreeRead, finishPacked, finishTree, keepsPackedRelease, keepsTreeRelease, packedStep, treeStep)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Registry.ServedDocument (objectField)
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), LimitError, Limits (progressFloor), ecosystemArtifactAuthorities, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Server.Metadata (MetadataReads, newMetadataReads)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, renderVersion)

-- | Bind one origin's npm metadata reads to their observers, leaving the caching policy to the caller.
newNpmMetadataReads ::
    TracingPort ->
    MetricsPort ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    OriginFor posture ->
    MetadataReads posture
newNpmMetadataReads tracing metrics logFailure logInvalid logFetch =
    newMetadataReads metrics logFailure logInvalid logFetch (fetchNpmManifest tracing) (fetchNpmVersion tracing)

{- | npm's memory charges, above the largest packed read peak per source byte (0.89, react) and the
largest output working set per basis byte of a realistic merge (1.52, @aws-sdk/client-s3).
-}
npmChargeFactors :: ChargeFactors
npmChargeFactors = ChargeFactors{cfFullReadPermille = 2100, cfOutputPermille = 2000}

-- | Fetch compact installation metadata and the complete source digest inside the response lifetime.
fetchNpmManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing origin name = do
    result <- fetchNpmBody tracing origin name (digestingRead (spanMetadataDecode tracing name . readNpmFull (ocLimits origin) name (originBaseUrl origin)) . chargedRead (ocChargeFullRead origin))
    pure $ do
        (streamed, digest) <- result
        (info, packed) <- projectNpmPacked (ocLimits origin) name (originBaseUrl origin) streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations npmArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst npmPacked packed
                , manifestBodyBytes = streamBytes streamed
                , manifestDigest = digest
                }

fetchNpmBody :: TracingPort -> OriginClient -> PackageName -> (IO ByteString -> IO (Either LimitError r)) -> IO (Either MetadataError r)
fetchNpmBody tracing origin name consume =
    metadataResponse
        <$> spanMetadataFetch
            tracing
            name
            (formThen FetchUrlUnformable (withSuccessBody (ocManager origin) (progressFloor (ocLimits origin)) consume) (metadataRequest (originBaseUrl origin) (ocToken origin) Full name))

decodeNpm :: TracingPort -> OriginClient -> PackageName -> PackumentRead -> IO ByteString -> IO (Either LimitError (StreamResult (Walked TreeRead)))
decodeNpm tracing origin name mode = spanMetadataDecode tracing name . readNpmPackument (ocLimits origin) name mode

-- | Walk a packument's chunks into aeson's tree with the production field policy, over a table keyed afresh for the read.
readNpmPackument :: Limits -> PackageName -> PackumentRead -> IO ByteString -> IO (Either LimitError (StreamResult (Walked TreeRead)))
readNpmPackument limits name mode readChunk = do
    table <- newInternTable <$> newTableKey <*> pure releaseUniqueFields
    readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (npmPackumentWalk limits name mode table) readChunk

-- | The production packument walk into aeson's tree over a caller's intern table.
npmPackumentWalk :: Limits -> PackageName -> PackumentRead -> InternTable -> TokenResult -> Step (Walked TreeRead)
npmPackumentWalk limits name mode table = npmWalk Trees (maxNestingDepth limits) mode (pureStep (treeStep limits name)) keepsTreeRelease table emptyTreeRead

-- | What a full read finishes with: the read's table, and its typed facts and packed releases.
type NpmFullRead = Walked PackedRead

-- | Walk a whole packument's chunks into its packed form, over a table keyed afresh for the read.
readNpmFull :: Limits -> PackageName -> Text -> IO ByteString -> IO (Either LimitError (StreamResult NpmFullRead))
readNpmFull limits name base readChunk = do
    (table, writer) <- newTableKey >>= \key -> stToIO (npmFullTable base name (newInternTable key releaseUniqueFields))
    readJsonWalkST stToIO (MetadataBodyLimit (maxMetadataBytes limits)) (npmFullWalk writer limits name table) readChunk

{- | A full read's table with the source author pointer every served release holds, and the writer
that packs each release against it.
-}
npmFullTable :: Text -> PackageName -> InternTable -> ST st (InternTable, Writer st)
npmFullTable base name table0 = (seeded,) <$> newWriter (Just (authorKey, pointer))
  where
    Interned authorKey withKey = internName (decodedName "author") table0
    Interned pointer seeded = internName (decodedName (authorPointer base name)) withKey

-- | The production full-read walk: each kept release packed by the writer against the caller's table.
npmFullWalk :: Writer st -> Limits -> PackageName -> InternTable -> TokenResult -> ST st (Steps (ST st) NpmFullRead)
npmFullWalk writer limits name table = npmWalk writer (maxNestingDepth limits) WholePackument (packedStep writer limits name) keepsPackedRelease table emptyPackedRead
{-# INLINE npmFullWalk #-}

-- | Finish a full read over its sealed table, keeping its typed error classification.
projectNpmPacked :: Limits -> PackageName -> Text -> StreamResult NpmFullRead -> Either MetadataError (PackageInfo, PackedPackument)
projectNpmPacked limits name base streamed = do
    Walked table acc <- first (streamError limits) (streamValue streamed)
    finishPacked limits name (authorPointer base name) (docTable (tableTexts table)) acc

fetchNpmVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchNpmVersion tracing origin name version = do
    result <- fetchNpmBody tracing origin name (decodeNpm tracing origin name (OneRelease (renderVersion version)))
    pure $ do
        streamed <- result
        projected <- projectNpmStream (ocLimits origin) name (originBaseUrl origin) streamed
        let selected = selectNpmRead version (streamBytes streamed) projected
        pure selected{vrVersion = vrVersion selected >>= locationCheckedDoc (originBaseUrl origin)}

-- | Finish a streamed source, keeping its typed error classification.
projectNpmStream :: Limits -> PackageName -> Text -> StreamResult (Walked TreeRead) -> Either MetadataError (PackageInfo, Value)
projectNpmStream limits name base streamed = do
    Walked _ acc <- first (streamError limits) (streamValue streamed)
    finishTree limits name (authorPointer base name) acc

-- | Pair one release with its compact source object and the same document's latest tag.
selectNpmRead :: Version -> Int -> (PackageInfo, Value) -> VersionRead
selectNpmRead version bodyBytes (info, raw) =
    VersionRead
        { vrVersion = do
            details <- Map.lookup (renderVersion version) (infoVersions info)
            selected <- selectNpmVersionDoc version (fst npmCached raw)
            pure VersionDoc{vdDetails = details, vdRaw = Just selected}
        , vrBodyBytes = bodyBytes
        , vrUpstreamLatest = Map.lookup "latest" (infoDistTags info)
        }

authorPointer :: Text -> PackageName -> Text
authorPointer base name = "See " <> fromRight (base <> "/" <> renderPackageName name) (packageUrl base name)

locationCheckedDoc :: Text -> VersionDoc -> Maybe VersionDoc
locationCheckedDoc upstreamBaseUrl doc =
    (\details -> doc{vdDetails = details})
        <$> enforceArtifactLocationsOf npmArtifactAuthorities upstreamBaseUrl (vdDetails doc)

npmArtifactAuthorities :: AllowedHostPorts
npmArtifactAuthorities = ecosystemArtifactAuthorities npmArtifactHosts

-- | Select the object paired with a warm full projection under the same rendered version key.
selectNpmVersionDoc :: Version -> CachedDoc -> Maybe CachedDoc
selectNpmVersionDoc version doc = do
    Object packument <- snd npmCached doc
    versions <- objectField "versions" packument
    fst npmCached <$> KeyMap.lookup (Key.fromText (renderVersion version)) versions

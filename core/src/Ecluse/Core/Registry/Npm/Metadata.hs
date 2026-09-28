-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

    -- * Pure projection
    projectNpmStream,
    selectNpmRead,
    selectNpmVersionDoc,
) where

import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (..), PackageName, renderPackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, newInternTable, newTableKey)
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), metadataRequest, npmArtifactHosts, packageUrl)
import Ecluse.Core.Registry.Npm.StreamingProjection (NpmProjection, collectField, emptyProjection, finishProjection, keepsRelease)
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

{- | npm's memory charges, above the largest read peak per source byte (1.65, typescript) and output
working set per basis byte (1.52, @aws-sdk/client-s3) from one meter step up.
-}
npmChargeFactors :: ChargeFactors
npmChargeFactors = ChargeFactors{cfFullReadPermille = 2100, cfOutputPermille = 2000}

-- | Fetch compact installation metadata and the complete source digest inside the response lifetime.
fetchNpmManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing origin name = do
    result <- fetchNpmBody tracing origin name (digestingRead (decodeNpm tracing origin name WholePackument) . chargedRead (ocChargeFullRead origin))
    pure $ do
        (streamed, digest) <- result
        (info, raw) <- projectNpmStream (ocLimits origin) name (originBaseUrl origin) streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations npmArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst npmCached raw
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

decodeNpm :: TracingPort -> OriginClient -> PackageName -> PackumentRead -> IO ByteString -> IO (Either LimitError (StreamResult NpmProjection))
decodeNpm tracing origin name mode = spanMetadataDecode tracing name . readNpmPackument (ocLimits origin) name mode

-- | Walk a packument's chunks with the production field policy, over a table keyed afresh for the read.
readNpmPackument :: Limits -> PackageName -> PackumentRead -> IO ByteString -> IO (Either LimitError (StreamResult NpmProjection))
readNpmPackument limits name mode readChunk = do
    table <- newInternTable <$> newTableKey <*> pure releaseUniqueFields
    readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (npmPackumentWalk limits name mode table) readChunk

-- | The production packument walk over a caller's intern table.
npmPackumentWalk :: Limits -> PackageName -> PackumentRead -> InternTable -> TokenResult -> Step NpmProjection
npmPackumentWalk limits name mode table = npmWalk (maxNestingDepth limits) mode (collectField limits name) keepsRelease table emptyProjection

fetchNpmVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchNpmVersion tracing origin name version = do
    result <- fetchNpmBody tracing origin name (decodeNpm tracing origin name (OneRelease (renderVersion version)))
    pure $ do
        streamed <- result
        projected <- projectNpmStream (ocLimits origin) name (originBaseUrl origin) streamed
        let selected = selectNpmRead version (streamBytes streamed) projected
        pure selected{vrVersion = vrVersion selected >>= locationCheckedDoc (originBaseUrl origin)}

-- | Finish a streamed source, keeping its typed error classification.
projectNpmStream :: Limits -> PackageName -> Text -> StreamResult NpmProjection -> Either MetadataError (PackageInfo, Value)
projectNpmStream limits name base streamed =
    first (streamError limits) (streamValue streamed) >>= finishProjection limits name (authorPointer base name)

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

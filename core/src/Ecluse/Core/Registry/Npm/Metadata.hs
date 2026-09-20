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

    -- * Pure projection
    projectNpmStream,
    selectNpmRead,
    selectNpmVersionDoc,
) where

import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (..), PackageName, renderPackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), isAuthorisationFailure)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Exchange (boundedJsonFetchWith, formThen)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataFetchError)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), metadataRequest, npmArtifactHosts, packageUrl)
import Ecluse.Core.Registry.Npm.Streaming (NpmRead (..), npmFields)
import Ecluse.Core.Registry.Npm.StreamingProjection (NpmProjection, collectField, emptyProjection, finishProjection)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), Limits, ecosystemArtifactAuthorities, maxMetadataBytes, maxNestingDepth)
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

-- | Fetch compact installation metadata and the complete source digest inside the response lifetime.
fetchNpmManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing origin name = do
    result <- fetchNpmStream tracing origin name FullRead
    pure $ do
        streamed <- result
        (info, raw) <- projectNpmStream (ocLimits origin) name (originBaseUrl origin) streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations npmArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst npmCached raw
                , manifestBodyBytes = streamBytes streamed
                , manifestDigest = streamDigest streamed
                }

fetchNpmStream :: TracingPort -> OriginClient -> PackageName -> NpmRead -> IO (Either MetadataError (StreamResult NpmProjection))
fetchNpmStream tracing origin name mode =
    spanMetadataFetch tracing name fetch <&> \case
        Left fault -> Left (metadataFetchError fault)
        Right (404, _) -> Left MetadataAbsent
        Right (code, result)
            | isAuthorisationFailure code -> Left (MetadataAuthorisationFailure code)
            | Just parsed <- result -> Right parsed
            | otherwise -> Left (MetadataHttpFailure code)
  where
    limits = ocLimits origin
    fetch =
        formThen
            FetchUrlUnformable
            ( boundedJsonFetchWith
                (spanMetadataDecode tracing name)
                (ocManager origin)
                (MetadataBodyLimit (maxMetadataBytes limits))
                (npmFields (maxNestingDepth limits) mode)
                (collectField limits name)
                emptyProjection
            )
            (metadataRequest (originBaseUrl origin) (ocToken origin) Full name)

fetchNpmVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchNpmVersion tracing origin name version = do
    result <- fetchNpmStream tracing origin name (SelectedRead (renderVersion version))
    pure $ do
        streamed <- result
        projected <- projectNpmStream (ocLimits origin) name (originBaseUrl origin) streamed
        let selected = selectNpmRead version (streamBytes streamed) projected
        pure selected{vrVersion = vrVersion selected >>= locationCheckedDoc (originBaseUrl origin)}

-- | Finish a streamed source while preserving its original identity and typed error classification.
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
    Object versions <- KeyMap.lookup "versions" packument
    fst npmCached <$> KeyMap.lookup (Key.fromText (renderVersion version)) versions

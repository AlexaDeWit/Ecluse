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
    projectNpmManifest,
    projectNpmVersion,
    selectNpmVersionDoc,
) where

import Data.Aeson (Value (Object, String), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    InvalidEntry,
    PackageInfo,
    PackageName,
 )
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault, RegistryResponse)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataError (MetadataBoundExceeded),
    VersionDoc (VersionDoc, vdDetails, vdRaw),
    VersionRead (VersionRead, vrUpstreamLatest, vrVersion),
    digestOf,
    fetchThenProject,
 )
import Ecluse.Core.Registry.Metadata.Projection (projectMetadata, projectionResult, selectiveError, validateReportedName)
import Ecluse.Core.Registry.Npm (fetchMetadataFormBounded)
import Ecluse.Core.Registry.Npm.Project (
    parsePackageInfoFromValue,
    projectName,
    projectVersionEntry,
 )
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), npmArtifactHosts)
import Ecluse.Core.Registry.Npm.SelectiveDecode (
    SelectedVersion (svDistTagLatest, svName, svTime, svVersion, svVersionCount),
    selectVersionFromPackument,
 )
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocLimits), OriginFor)
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (
    AllowedHostPorts,
    Limits,
    checkVersionCountOf,
    ecosystemArtifactAuthorities,
    maxNestingDepth,
 )
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Metadata (MetadataReads, newMetadataReads)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

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
    newMetadataReads metrics logFailure logInvalid logFetch (fetchNpmManifest tracing) (fetchNpmVersion tracing) selectNpmVersionDoc

fetchNpmPackument :: OriginClient -> PackageName -> IO (Either FetchFault RegistryResponse)
fetchNpmPackument origin = fetchMetadataFormBounded origin Full noValidators

-- | Fetch a bounded full packument with the digest that scopes its cached document.
fetchNpmManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing origin name =
    fetchThenProject tracing (fetchNpmPackument origin) name $ \body ->
        manifestOf (digestOf body) . first (enforceArtifactLocations npmArtifactAuthorities (originBaseUrl origin))
            <$> projectNpmManifest (ocLimits origin) name body
  where
    manifestOf digest (info, raw) = Manifest{manifestInfo = info, manifestRaw = fst npmCached raw, manifestDigest = digest}

-- | Project a nesting-checked packument and retain its raw document for assembly.
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name = projectMetadata (parsePackageInfoFromValue name) limits

fetchNpmVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchNpmVersion tracing origin name version =
    fetchThenProject tracing (fetchNpmPackument origin) name $
        fmap locationChecked . projectNpmVersion (ocLimits origin) name version
  where
    locationChecked versionRead =
        versionRead{vrVersion = vrVersion versionRead >>= traverse locationCheckedDoc}
    locationCheckedDoc doc =
        (\details -> doc{vdDetails = details}) <$> enforceArtifactLocationsOf npmArtifactAuthorities (originBaseUrl origin) (vdDetails doc)

-- npm artifacts must use the authority that served the packument.
npmArtifactAuthorities :: AllowedHostPorts
npmArtifactAuthorities = ecosystemArtifactAuthorities npmArtifactHosts

originBaseUrl :: OriginClient -> Text
originBaseUrl = registryUrlText . ocBaseUrl

{- | Project one version without decoding its siblings. Absent or unprojectable versions yield
'Nothing'. The pair carries the selected object as decoded, never a re-rendering of the typed view.
-}
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError VersionRead
projectNpmVersion limits name version body = do
    decoded <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    reported <- validateReportedName projectName (svName decoded)
    selected <- projectionResult (checkNameAgreement name reported decoded)
    first MetadataBoundExceeded (checkVersionCountOf limits (svVersionCount selected))
    publishedAt <- parsePublishTime (svTime selected)
    pure
        VersionRead
            { vrVersion = do
                raw <- svVersion selected
                -- Use the same rendered version key as the full-document projection.
                details <- projectVersionEntry name (mkVersion Npm (renderVersion version)) publishedAt raw
                pure (Snapshot (digestOf body) (VersionDoc{vdDetails = details, vdRaw = Just (fst npmCached raw)}))
            , vrUpstreamLatest = latestTarget (svDistTagLatest selected)
            }

{- | Select one version's object out of a held packument, for a warm full-document read. The
lookup uses the same rendered key the projection used, so the pair cannot name a sibling.
-}
selectNpmVersionDoc :: Version -> CachedDoc -> Maybe CachedDoc
selectNpmVersionDoc version doc = do
    Object packument <- snd npmCached doc
    Object versions <- KeyMap.lookup "versions" packument
    fst npmCached <$> KeyMap.lookup (Key.fromText (renderVersion version)) versions

-- A non-string @latest@ is no known tag, matching the whole-document projection's per-entry drop.
latestTarget :: Maybe Value -> Maybe Version
latestTarget = \case
    Just (String raw) -> Just (mkVersion Npm raw)
    _ -> Nothing

-- An absent or undecodable stamp means no known publish time, never a document failure.
-- The whole-document path drops a malformed @time@ entry the same way.
parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
parsePublishTime = \case
    Nothing -> Right Nothing
    Just timeValue -> Right (parseMaybe parseJSON timeValue)

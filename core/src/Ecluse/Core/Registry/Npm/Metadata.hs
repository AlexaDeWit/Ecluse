{- | The npm realization of the serve-path read operations: fetch a package's full
packument and project it into the domain manifest, reporting every failure as a typed
'MetadataError'.

npm satisfies both serve-path needs from the /same/ full-packument endpoint: the
publish-age rules require the packument's @time@ map, which npm exposes only in the
full form, so even the single-version need fetches the full bytes. This module owns the
npm side of both serve-path operations -- the fetch and the projection -- while the cache,
metrics, and single-version cache topology are wired around them by the serve layer
("Ecluse.Core.Server.Metadata"), which is where the cross-cutting caching policy belongs:

  * 'fetchNpmManifest' \/ 'projectNpmManifest' back the full-manifest operation. The
    projection is the sequence the serve path has always applied to a fetched packument --
    decode, bound the nesting depth, project and validate the self-reported name, bound the
    version count -- re-expressed as a total 'Either' so the serve path maps each cause onto
    a response rather than catching a typed throw.

  * 'fetchNpmVersion' \/ 'projectNpmVersion' back the single-version operation. The full
    bytes are still fetched (npm carries @time@ only in the full form), but they are parsed
    __selectively__ ("Ecluse.Core.Registry.Npm.SelectiveDecode"): only the requested
    version's object and @time@ entry are materialised, the others skipped unallocated, so
    a cold tarball gate no longer pays a whole-packument decode to consult one version. The
    selected version is projected through the /same/ per-version code the full path runs, so
    its 'Ecluse.Core.Package.PackageDetails' is identical to selecting it out of a full
    projection -- the optimization the stable boundary was always meant to admit.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * npm full-manifest fetch
    fetchNpmManifest,

    -- * npm single-version fetch
    fetchNpmVersion,

    -- * Pure projection
    projectNpmManifest,
    projectNpmManifestHybrid,
    projectNpmVersion,
) where

import Data.Aeson (Value, eitherDecodeStrict, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Time (UTCTime)
import UnliftIO.Exception (handle)

import Data.Aeson (Value (Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo,
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (npmBaseUrl, npmLimits),
    ResponseBoundExceeded (ResponseBoundExceeded),
    fetchMetadataForm,
 )
import Ecluse.Core.Registry.Npm.Project (
    Projection (NameMismatch, Projected),
    enforceTarballScheme,
    enforceTarballSchemeDetails,
    parsePackageInfoFromValue,
    projectName,
    projectVersionEntry,
 )
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Abbreviated, Full),
    noValidators,
 )
import Ecluse.Core.Registry.Npm.SelectiveDecode (
    SelectedVersion (svName, svTime, svVersion, svVersionCount),
    SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable),
    selectTimeFromPackument,
    selectVersionFromPackument,
 )
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested, TooManyVersions),
    Limits,
    checkNestingDepth,
    checkVersionCount,
    maxNestingDepth,
    maxVersionCount,
 )
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import UnliftIO.Async (concurrently)

{- | Fetch a package's full packument and project it into @(manifest, raw document)@,
or the typed 'MetadataError' for why it could not.

The body is read bounded against the config's response budget (so an oversized upstream
is refused fail-closed before it is buffered whole); a breach surfaces as
'MetadataBoundExceeded'. A genuine transport fault is left to throw -- the serve path
already brackets the unreachable-upstream case -- so this 'Either' carries only the
parse-and-policy outcomes the serve path renders distinctly.
-}
fetchNpmManifest :: TracingPort -> NpmClientConfig -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
fetchNpmManifest tracing config name =
    handle (\(ResponseBoundExceeded err) -> pure (Left (MetadataBoundExceeded err))) $ do
        (abbrevResp, fullResp) <-
            concurrently
                (spanMetadataFetch tracing name $ fetchMetadataForm config Abbreviated noValidators name)
                (spanMetadataFetch tracing name $ fetchMetadataForm config Full noValidators name)
        spanMetadataDecode tracing name $
            pure (first (enforceTarballScheme (npmBaseUrl config)) <$> projectNpmManifestHybrid (npmLimits config) name (responseBody abbrevResp) (responseBody fullResp))

{- | Project a fetched packument's bytes into @(manifest, raw document)@, applying the
serve path's response bounds and name validation. Pure and total.

The sequence -- decode to a 'Value', bound its nesting depth, project the typed
'PackageInfo' and validate its self-reported name against the request, then bound the
version count -- is the one the serve path has always run; the raw 'Value' returned is
the nesting-checked document the serve path edits in place, so the typed view and the
served bytes describe the same parse. Each refusal maps to the constructor the serve
path renders: a decode failure or an absent\/undecodable name is 'MetadataUndecodable';
a self-reported /different/ name is 'MetadataNameMismatch'; a nesting-depth or
version-count breach is 'MetadataBoundExceeded'.
-}
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = projectNpmManifestHybrid limits name body body

projectNpmManifestHybrid :: Limits -> PackageName -> ByteString -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifestHybrid limits name abbrevBody fullBody = do
    timeValue <- case selectTimeFromPackument (maxNestingDepth limits) fullBody of
        Left SelectiveUndecodable -> Left MetadataUndecodable
        Left SelectiveTooDeeplyNested -> Left (MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits)))
        Right found -> Right found

    abbrevValue <- first (const MetadataUndecodable) (eitherDecodeStrict abbrevBody)

    let mergedValue = case timeValue of
            Just t -> case abbrevValue of
                Object obj -> Object (KeyMap.insert "time" t obj)
                _ -> abbrevValue
            Nothing -> abbrevValue

    bounded <- first MetadataBoundExceeded (checkNestingDepth limits mergedValue)
    info <- case parsePackageInfoFromValue name bounded of
        Left _ -> Left MetadataUndecodable
        Right (NameMismatch reported) -> Left (MetadataNameMismatch reported)
        Right (Projected projected) -> Right projected
    boundedInfo <- first MetadataBoundExceeded (checkVersionCount limits info)
    pure (boundedInfo, bounded)

{- | Fetch a package's full packument and project __only the requested version__ into its
'PackageDetails', or the typed 'MetadataError' for why it could not -- the cheap counterpart
to 'fetchNpmManifest' for the single-version serve operation.

npm carries the @time@ map only in the full document, so the __full bytes are still
fetched__ (bounded against the config's budget, exactly as 'fetchNpmManifest'); the win is
that they are parsed __selectively__ ('projectNpmVersion'), materialising the one requested
version rather than every version. A 'Nothing' is a version genuinely absent from a sound
document (a forwarded miss); a 'MetadataError' is metadata that could not be obtained at all.
A transport fault is left to throw, as 'fetchNpmManifest'.
-}
fetchNpmVersion :: TracingPort -> NpmClientConfig -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
fetchNpmVersion tracing config name version =
    handle (\(ResponseBoundExceeded err) -> pure (Left (MetadataBoundExceeded err))) $ do
        response <- spanMetadataFetch tracing name $ fetchMetadataForm config Full noValidators name
        spanMetadataDecode tracing name $
            pure ((>>= enforceTarballSchemeDetails (npmBaseUrl config)) <$> projectNpmVersion (npmLimits config) name version (responseBody response))

{- | Project a fetched packument's bytes into __one version's__ 'PackageDetails' (or the
typed 'MetadataError'), without decoding the other versions. Pure and total.

The outcome is the same the whole-document path would reach for that one version, computed
selectively: 'Ecluse.Core.Registry.Npm.SelectiveDecode.selectVersionFromPackument' walks the
token stream -- depth-bounding every value (the 'maxNestingDepth' ceiling
'projectNpmManifest' applies through 'checkNestingDepth') and reporting malformed JSON as
'MetadataUndecodable' -- and materialises only the document @name@, the requested version's
object, and its @time@ entry. Those are then validated and projected exactly as
'projectNpmManifest' would:

  * the self-reported @name@ is validated against the request -- an absent\/undecodable name
    is 'MetadataUndecodable', a self-reported /different/ name is 'MetadataNameMismatch' (the
    anti-shadowing distinction);
  * the @versions@ count is bounded against 'maxVersionCount' (the raw entry count -- a
    fail-closed defence-in-depth backstop on this path, which evaluates only the one version
    regardless, so it never needs the projected count the full path bounds);
  * the requested version's object is projected through
    'Ecluse.Core.Registry.Npm.Project.projectVersionEntry' -- the same per-version projection
    the full path runs -- so a present version yields a 'PackageDetails' identical to
    @'Data.Map.Strict.lookup'@-ing it out of a full 'projectNpmManifest', and an
    absent\/unprojectable version yields 'Nothing'.
-}
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectNpmVersion limits name version body = do
    selected <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    -- The self-reported name is the validation authority (anti-shadowing), checked before
    -- the version-count backstop -- the same order 'projectNpmManifest' validates the name
    -- before bounding the count.
    reported <- validateName (svName selected)
    when (reported /= name) (Left (MetadataNameMismatch (renderPackageName reported)))
    when
        (svVersionCount selected > maxVersionCount limits)
        (Left (MetadataBoundExceeded (TooManyVersions (svVersionCount selected) (maxVersionCount limits))))
    publishedAt <- parsePublishTime (svTime selected)
    -- 'mkVersion' over the requested version's rendered key matches the whole-document path,
    -- which keys 'projectVersions' by that same string and so projects the version under it.
    pure (svVersion selected >>= projectVersionEntry name (mkVersion Npm (renderVersion version)) publishedAt)
  where
    -- The document's self-reported name, validated as the whole-document decode does: an
    -- absent name defaults to the empty string and so fails 'projectName' (undecodable), a
    -- present non-string fails the @Text@ decode (undecodable), and a well-formed name is
    -- the 'PackageName' the request is matched against.
    validateName :: Maybe Value -> Either MetadataError PackageName
    validateName = \case
        Nothing -> Left MetadataUndecodable
        Just nameValue -> case parseMaybe parseJSON nameValue of
            Nothing -> Left MetadataUndecodable
            Just raw -> first (const MetadataUndecodable) (projectName raw)

    -- The requested version's publish stamp, folded leniently to match the whole-document
    -- path: absent is no stamp ('Nothing'), and a present-but-un-decodable stamp is also
    -- 'Nothing' (the version has no known publish time) rather than a document failure:
    -- the full path drops a malformed @time@ entry per-entry, so the requested version it
    -- would project there carries no time, and the selective projection must agree. (A
    -- structurally-malformed-JSON stamp is still a 'SelectiveUndecodable' from the walk, as
    -- it is an 'eitherDecodeStrict' failure on the full path.)
    parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
    parsePublishTime = \case
        Nothing -> Right Nothing
        Just timeValue -> Right (parseMaybe parseJSON timeValue)

-- Map a selective-decode refusal onto the 'MetadataError' the whole-document path raises
-- for the same cause: malformed\/non-object bytes are 'MetadataUndecodable', a depth breach
-- is the 'maxNestingDepth' bound 'checkNestingDepth' reports.
selectiveError :: Limits -> SelectiveError -> MetadataError
selectiveError limits = \case
    SelectiveUndecodable -> MetadataUndecodable
    SelectiveTooDeeplyNested -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))

{- | The npm realization of the serve-path read operations: fetch a package's full
packument and project it into the domain manifest, reporting every failure as a typed
'MetadataError'.

npm satisfies both serve-path needs from the /same/ full-packument endpoint: the
publish-age rules require the packument's @time@ map, which npm exposes only in the
full form, so even the single-version need fetches the full bytes. This module owns
the npm side of 'Ecluse.Core.Registry.Metadata.fetchFullManifest' — the fetch and the
projection; the cache, metrics, and single-version selection are wired around it by
the serve layer ("Ecluse.Core.Server.Metadata"), which is where the cross-cutting
caching policy belongs.

The projection is the same sequence the serve path has always applied to a fetched
packument — decode, bound the nesting depth, project and validate the self-reported
name, bound the version count — re-expressed as a total 'Either' so the serve path
maps each cause onto a response rather than catching a typed throw. Keeping the
current full-decode behaviour is deliberate: a selective single-version parse is a
later optimization the stable boundary admits without disturbing this projection.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * npm full-manifest fetch
    fetchNpmManifest,

    -- * Pure projection
    projectNpmManifest,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import UnliftIO.Exception (handle)

import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm (
    MetadataForm (Full),
    NpmClientConfig (npmLimits),
    ResponseBoundExceeded (ResponseBoundExceeded),
    fetchMetadataForm,
    noValidators,
 )
import Ecluse.Core.Registry.Npm.Project (
    Projection (NameMismatch, Projected),
    parsePackageInfoFromValue,
 )
import Ecluse.Core.Security (Limits, checkNestingDepth, checkVersionCount)

{- | Fetch a package's full packument and project it into @(manifest, raw document)@,
or the typed 'MetadataError' for why it could not.

The body is read bounded against the config's response budget (so an oversized upstream
is refused fail-closed before it is buffered whole); a breach surfaces as
'MetadataBoundExceeded'. A genuine transport fault is left to throw — the serve path
already brackets the unreachable-upstream case — so this 'Either' carries only the
parse-and-policy outcomes the serve path renders distinctly.
-}
fetchNpmManifest :: NpmClientConfig -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
fetchNpmManifest config name =
    handle (\(ResponseBoundExceeded err) -> pure (Left (MetadataBoundExceeded err))) $ do
        response <- fetchMetadataForm config Full noValidators name
        pure (projectNpmManifest (npmLimits config) name (responseBody response))

{- | Project a fetched packument's bytes into @(manifest, raw document)@, applying the
serve path's response bounds and name validation. Pure and total.

The sequence — decode to a 'Value', bound its nesting depth, project the typed
'PackageInfo' and validate its self-reported name against the request, then bound the
version count — is the one the serve path has always run; the raw 'Value' returned is
the nesting-checked document the serve path edits in place, so the typed view and the
served bytes describe the same parse. Each refusal maps to the constructor the serve
path renders: a decode failure or an absent\/undecodable name is 'MetadataUndecodable';
a self-reported /different/ name is 'MetadataNameMismatch'; a nesting-depth or
version-count breach is 'MetadataBoundExceeded'.
-}
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = do
    value <- first (const MetadataUndecodable) (eitherDecodeStrict body)
    bounded <- first MetadataBoundExceeded (checkNestingDepth limits value)
    info <- case parsePackageInfoFromValue name bounded of
        Left _ -> Left MetadataUndecodable
        Right (NameMismatch reported) -> Left (MetadataNameMismatch reported)
        Right (Projected projected) -> Right projected
    boundedInfo <- first MetadataBoundExceeded (checkVersionCount limits info)
    pure (boundedInfo, bounded)

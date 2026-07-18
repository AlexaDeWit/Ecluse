-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared npm fixtures: faithful packument and version-object builders plus the
public-registry defaults the suites build 'NpmClientConfig' values from.

'VersionSpec', 'versionValue', and 'packumentValue' keep cross-suite JSON fixtures
structurally aligned while leaving each suite's axis-specific values at the call site.
'publishedDaysAgo' gives age-sensitive fixtures the same clock-relative timestamp
calculation without owning their clock.

'defaultNpmConfig' is the anonymous public-registry config a suite hands to the npm data
plane ("Ecluse.Core.Registry.Npm"); 'publicRegistryBaseUrl' and 'publicRegistryUrl' are the
canonical public npm registry as text and as an https 'RegistryUrl' (built through
'Ecluse.Test.Package.unsafeRegistryUrl', over the https-only
'Ecluse.Core.Security.Egress.mkRegistryUrl').
-}
module Ecluse.Test.Registry.Npm (
    -- * Packument fixtures
    VersionSpec (..),
    versionSpec,
    versionValue,
    packumentValue,
    publishedDaysAgo,

    -- * Client fixtures
    defaultNpmConfig,
    publicRegistryBaseUrl,
    publicRegistryUrl,
) where

import Data.Aeson (Value (Object), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Client (Manager)

import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Test.Package (unsafeRegistryUrl)

{- | The common fields of an npm version object. 'vsExtraPairs' carries fields whose
exact representation belongs to a particular test axis, such as dependencies or an
explicit @hasInstallScript@ signal; an extra pair overrides the common field with the
same key.
-}
data VersionSpec = VersionSpec
    { vsName :: Text
    -- ^ The package name self-reported by the version object.
    , vsVersion :: Text
    -- ^ The version string self-reported by the version object.
    , vsTarballUrl :: Text
    -- ^ The upstream artifact URL under @dist.tarball@.
    , vsIntegrity :: Maybe Text
    -- ^ The optional Subresource Integrity value under @dist.integrity@.
    , vsShasum :: Maybe Text
    -- ^ The optional legacy SHA-1 value under @dist.shasum@.
    , vsHasInstallScript :: Bool
    -- ^ Whether to include a representative @scripts.postinstall@ entry.
    , vsExtraPairs :: [Pair]
    -- ^ Site-specific version fields, applied after the common fields.
    }
    deriving stock (Eq, Show)

{- | Start a version fixture with its identity and tarball URL; optional digests,
install scripts, and site-specific fields are absent until the caller opts into them.
-}
versionSpec :: Text -> Text -> Text -> VersionSpec
versionSpec name version tarballUrl =
    VersionSpec
        { vsName = name
        , vsVersion = version
        , vsTarballUrl = tarballUrl
        , vsIntegrity = Nothing
        , vsShasum = Nothing
        , vsHasInstallScript = False
        , vsExtraPairs = []
        }

{- | Build the faithful npm version-object shape shared by the test suites. Optional
digest fields are absent when unspecified, matching registry metadata rather than
encoding them as @null@.
-}
versionValue :: VersionSpec -> Value
versionValue spec =
    objectWithExtraPairs
        [ "name" .= vsName spec
        , "version" .= vsVersion spec
        , "dist"
            .= object
                ( ["tarball" .= vsTarballUrl spec]
                    <> maybe [] (pure . ("integrity" .=)) (vsIntegrity spec)
                    <> maybe [] (pure . ("shasum" .=)) (vsShasum spec)
                )
        ]
        ( ["scripts" .= object ["postinstall" .= ("node build.js" :: Text)] | vsHasInstallScript spec]
            <> vsExtraPairs spec
        )

{- | Build an npm packument from its decision-bearing fields. The caller supplies the
complete @time@ map because created/modified bookkeeping differs by fixture; extra
top-level pairs preserve site-specific relay or benchmark fields.
-}
packumentValue ::
    -- | The package name self-reported by the packument.
    Text ->
    -- | The target of @dist-tags.latest@.
    Text ->
    -- | Version keys paired with their version objects.
    [(Text, Value)] ->
    -- | The complete contents of the top-level @time@ object.
    [Pair] ->
    -- | Site-specific top-level fields, applied after the common fields.
    [Pair] ->
    Value
packumentValue name latest versions times =
    objectWithExtraPairs
        [ "name" .= name
        , "dist-tags" .= object ["latest" .= latest]
        , "versions" .= object [(Key.fromText version, value) | (version, value) <- versions]
        , "time" .= object times
        ]

{- | Render an npm @time@ instant the given number of whole days before the caller's
fixture clock.
-}
publishedDaysAgo :: UTCTime -> Integer -> Text
publishedDaysAgo now ageDays =
    toText (iso8601Show (addUTCTime (negate (fromInteger ageDays * nominalDay)) now))

-- Apply site-specific fields last so their exact representation wins deliberately.
objectWithExtraPairs :: [Pair] -> [Pair] -> Value
objectWithExtraPairs common extra =
    Object (foldl' insertPair (KeyMap.fromList common) extra)
  where
    insertPair fields (key, value) = KeyMap.insert key value fields

{- | The canonical public npm registry base URL, @https://registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | The canonical public npm registry as an https 'RegistryUrl': the
'publicRegistryBaseUrl' text validated through 'unsafeRegistryUrl' (the https-only
'Ecluse.Core.Security.Egress.mkRegistryUrl').
-}
publicRegistryUrl :: RegistryUrl
publicRegistryUrl = unsafeRegistryUrl publicRegistryBaseUrl

{- | An anonymous client config against the public registry ('publicRegistryBaseUrl'),
using the given shared 'Manager' and the secure-default response bounds
('Ecluse.Core.Security.defaultLimits'). Override 'npmBaseUrl'/'npmToken'/'npmLimits' for
a managed backend or a per-deployment budget.
-}
defaultNpmConfig :: Manager -> NpmClientConfig
defaultNpmConfig manager =
    NpmClientConfig
        { npmBaseUrl = publicRegistryBaseUrl
        , npmManager = manager
        , npmToken = Nothing
        , npmLimits = defaultLimits
        }

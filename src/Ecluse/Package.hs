{- | The package domain model — ecosystem-agnostic vocabulary for the rules
engine.

These types capture everything the proxy needs to reason about a package
version while staying decoupled from any registry's wire format. Registry
adapters (npm today, others later) are responsible for projecting their
responses into these types; nothing above the registry layer sees
registry-specific structures.
-}
module Ecluse.Package (
    -- * Scopes
    Scope,
    mkScope,
    unScope,
    renderScope,

    -- * Package identity
    PackageName (..),
    renderPackageName,

    -- * Versions
    Version,
    mkVersion,
    unVersion,
    renderVersion,

    -- * Per-version details
    Dist (..),
    Maintainer (..),
    PackageDetails (..),
) where

import Data.Text qualified as T
import Data.Time (UTCTime)

{- | An npm scope, stored without its leading @\'@\'@ (the scope of
@\@myorg\/pkg@ is @"myorg"@). Construct via 'mkScope', which normalises away
a leading @\'@\'@ so equality is independent of how the scope was written.
-}
newtype Scope = Scope Text
    deriving stock (Eq, Ord, Show)

-- | Build a 'Scope', tolerating an optional leading @\'@\'@.
mkScope :: Text -> Scope
mkScope raw = Scope (fromMaybe raw (T.stripPrefix "@" raw))

-- | The bare scope text, without the leading @\'@\'@.
unScope :: Scope -> Text
unScope (Scope s) = s

-- | Render a scope in npm wire form, with the leading @\'@\'@.
renderScope :: Scope -> Text
renderScope (Scope s) = "@" <> s

-- | A package identity, decoupled from any registry's wire format.
data PackageName = PackageName
    { packageScope :: Maybe Scope
    -- ^ The scope, if the package is scoped (@\@scope\/name@).
    , packageBaseName :: Text
    -- ^ The unscoped name (the part after any scope).
    }
    deriving stock (Eq, Ord, Show)

-- | Render a package name in npm wire form: @\@scope\/name@ or @name@.
renderPackageName :: PackageName -> Text
renderPackageName (PackageName mScope name) =
    case mScope of
        Just scope -> renderScope scope <> "/" <> name
        Nothing -> name

{- | A package version string (e.g. @"1.2.3"@). Kept opaque: we do not parse
semver here. Rules that need version ordering can layer it on later.
-}
newtype Version = Version Text
    deriving stock (Eq, Ord, Show)

mkVersion :: Text -> Version
mkVersion = Version

unVersion :: Version -> Text
unVersion (Version v) = v

renderVersion :: Version -> Text
renderVersion = unVersion

{- | Distribution / artifact information for a single version. The integrity
fields are what later rules will verify a downloaded tarball against.
-}
data Dist = Dist
    { distTarball :: Text
    -- ^ URL of the artifact tarball.
    , distIntegrity :: Maybe Text
    -- ^ Subresource-integrity string (e.g. @"sha512-..."@), if provided.
    , distShasum :: Maybe Text
    -- ^ Legacy SHA-1 checksum, if provided.
    }
    deriving stock (Eq, Show)

-- | A package maintainer. Captured for future provenance / allow-list rules.
data Maintainer = Maintainer
    { maintainerName :: Text
    , maintainerEmail :: Maybe Text
    }
    deriving stock (Eq, Ord, Show)

{- | The ecosystem-agnostic snapshot of a single package /version/ that the
rules engine evaluates. A registry adapter projects its wire format into
this; the rules engine never sees anything else.
-}
data PackageDetails = PackageDetails
    { pkgName :: PackageName
    , pkgVersion :: Version
    , pkgPublishedAt :: UTCTime
    -- ^ When this version was published to the source registry.
    , pkgHasInstallScripts :: Bool
    -- ^ Whether the version declares install / pre- / post-install scripts.
    , pkgDeprecated :: Maybe Text
    -- ^ Deprecation message, if the version is deprecated.
    , pkgDist :: Dist
    , pkgLicense :: Maybe Text
    -- ^ SPDX license expression, if declared.
    , pkgMaintainers :: [Maintainer]
    , pkgDependencies :: Map Text Text
    -- ^ Runtime dependency name -> version range, as declared.
    }
    deriving stock (Eq, Show)

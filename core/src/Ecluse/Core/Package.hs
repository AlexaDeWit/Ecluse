-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem-neutral package model used by admission and rules.
Artifact entry keys retain source coordinates while adapters keep ownership of wire formats.
-}
module Ecluse.Core.Package (
    -- * Scopes
    Scope,
    mkScope,
    unScope,
    renderScope,

    -- * Package identity
    PackageName,
    mkPackageName,
    pkgEcosystem,
    pkgNamespace,
    pkgCanonical,
    pkgBaseName,
    renderPackageName,
    unscopedName,
    WireNameRefusal (..),
    unnamespacedWireName,

    -- * The name charset boundary
    isAsciiNameComponent,

    -- * Canonical keys
    canonicalise,

    -- * Normalised signals
    CodeExecSignal (..),
    DependencySources (..),
    Availability (..),

    -- * Artifacts
    Artifact (..),
    CoreMetadata (..),
    Hash,
    hashAlg,
    hashValue,
    mkHash,
    mkSriHashes,
    HashAlg (..),

    -- * Algorithm vocabulary
    renderHashAlg,
    parseHashAlg,
    sriPrefix,
    sriBody,
    sriAlgorithm,

    -- * Digest computation
    computeDigest,
    isComputable,

    -- * Per-version details
    PackageDetails (..),

    -- * Packument-level view
    PackageInfo (..),

    -- * Dropped entries
    InvalidEntry (invalidKind, invalidKey, invalidValue, invalidReason),
    mkInvalidEntry,
    InvalidEntryKind (..),
    renderInvalidEntryKind,
    dropCountsByKind,
) where

import Data.Char (isAscii, isControl)
import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package.Cargo (cargoNameKey)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Package.Hash (
    Hash,
    HashAlg (..),
    computeDigest,
    hashAlg,
    hashValue,
    isComputable,
    mkHash,
    mkSriHashes,
    parseHashAlg,
    renderHashAlg,
    sriAlgorithm,
    sriBody,
    sriPrefix,
 )
import Ecluse.Core.Package.InvalidEntry (
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    InvalidEntryKind (..),
    dropCountsByKind,
    mkInvalidEntry,
    renderInvalidEntryKind,
 )
import Ecluse.Core.Package.Pep503 (normalisePyPI)
import Ecluse.Core.Version (Version)

{- | An npm scope without its leading @\@@. 'mkScope' removes that prefix,
so equality does not depend on how the scope was written.
-}
newtype Scope = Scope ShortText
    deriving stock (Eq, Ord, Show)

-- | Build a 'Scope', tolerating an optional leading @\'\@\'@.
mkScope :: Text -> Scope
mkScope raw = Scope (TS.fromText (fromMaybe raw (T.stripPrefix "@" raw)))

-- | The bare scope text, without the leading @\'\@\'@.
unScope :: Scope -> Text
unScope (Scope s) = TS.toText s

-- | Render a scope in npm wire form, with the leading @\'\@\'@.
renderScope :: Scope -> Text
renderScope (Scope s) = "@" <> TS.toText s

{- | A registry-independent identity built with 'mkPackageName'. Equality and ordering use
@('pkgEcosystem', 'pkgNamespace', 'pkgCanonical')@, excluding the display spelling.
-}
data PackageName = PackageName
    { pkgEcosystem :: Ecosystem
    -- ^ The ecosystem this name belongs to.
    , pkgNamespace :: Maybe Scope
    -- ^ The npm scope. Unscoped ecosystems use 'Nothing'.
    , pkgCanonical :: ShortText
    -- ^ The lookup key under the ecosystem's name grammar.
    , pkgDisplay :: ShortText
    , pkgBaseName :: ShortText
    {- ^ The base name with any @\@scope\/@ prefix dropped. It is not part of identity. Read it
    back through 'unscopedName'.
    -}
    }
    deriving stock (Show)

nameKey :: PackageName -> (Ecosystem, Maybe Scope, ShortText)
nameKey n = (pkgEcosystem n, pkgNamespace n, pkgCanonical n)

instance Eq PackageName where
    a == b = nameKey a == nameKey b

instance Ord PackageName where
    compare a b = compare (nameKey a) (nameKey b)

-- | Build a name with its ecosystem lookup key, retaining the published spelling.
mkPackageName :: Ecosystem -> Maybe Scope -> Text -> PackageName
mkPackageName eco ns raw =
    PackageName
        { pkgEcosystem = eco
        , pkgNamespace = ns
        , pkgCanonical = TS.fromText (canonicalise eco display)
        , pkgDisplay = TS.fromText display
        , pkgBaseName = TS.fromText raw
        }
  where
    display = case ns of
        Just s -> renderScope s <> "/" <> raw
        Nothing -> raw

{- | Normalise a display name into its canonical matching key for an ecosystem. An
ecosystem with a normalisation grammar keeps it in its own module.
-}
canonicalise :: Ecosystem -> Text -> Text
canonicalise = \case
    Npm -> id
    RubyGems -> id
    PyPI -> normalisePyPI
    Cargo -> cargoNameKey

-- | Render a package name in its native wire form (the display name).
renderPackageName :: PackageName -> Text
renderPackageName = TS.toText . pkgDisplay

-- | The unscoped (base) name as 'Text': @\@babel\/code-frame@ reads back as @code-frame@.
unscopedName :: PackageName -> Text
unscopedName = TS.toText . pkgBaseName

-- | Why a wire name did not yield an unnamespaced package name.
data WireNameRefusal e
    = -- | The wire carried a non-empty namespace.
      WireNamespaced
    | -- | The ecosystem's grammar refused the bare name.
      WireNameUnusable e
    deriving stock (Eq, Show)

-- | Refuse a non-empty wire namespace, then read the bare name through the ecosystem's grammar.
unnamespacedWireName :: (Text -> Either e PackageName) -> Maybe Text -> Text -> Either (WireNameRefusal e) PackageName
unnamespacedWireName parse namespace raw = unless (maybe True T.null namespace) (Left WireNamespaced) *> first WireNameUnusable (parse raw)

{- | Whether one component of a package name is ASCII with no control character: the boundary
every ecosystem's grammar rests on, because an invisible codepoint renders two names as one.
-}
isAsciiNameComponent :: Text -> Bool
isAsciiNameComponent = T.all (\ch -> isAscii ch && not (isControl ch))

-- | Whether installation executes package-supplied code, including builds and native extensions.
data CodeExecSignal
    = -- | Determined: installation runs no code.
      NoCodeOnInstall
    | -- | Determined: installation runs code. The text says how, for the audit trail.
      RunsCodeOnInstall Text
    | {- | Not yet determined (e.g. nothing has fetched the RubyGems gemspec yet).
      Pure rules abstain, and the effectful tier may resolve it.
      -}
      CodeExecUnknown
    deriving stock (Eq, Show)

-- | Whether installing a version fetches a dependency from somewhere other than the registry.
data DependencySources
    = -- | Every installable dependency resolves through the registry.
      RegistryDependencies
    | -- | A dependency installs from elsewhere, such as a git repository. The text names it, for the audit trail.
      FetchesOutsideRegistry Text
    | -- | Not determined: the adapter does not read dependency sources. Pure rules abstain.
      DependencySourcesUnknown
    deriving stock (Eq, Show)

{- | Whether a version is offered, advisory-deprecated, or withdrawn. The registry's notice text
stays in the served document.
-}
data Availability
    = -- | Offered normally.
      Available
    | -- | Advisory deprecation (npm), still resolvable.
      Deprecated
    | -- | Withdrawn from resolution (PyPI yank keeps the file, RubyGems yank removes it).
      Yanked
    deriving stock (Eq, Show)

-- | The upstream companion declaration, preserving raw digest names and values.
data CoreMetadata
    = -- | The index declares availability without a digest.
      MetadataAvailable Bool
    | -- | Algorithm names and digest strings exactly as declared by the index.
      MetadataDigests (Map Text Text)
    deriving stock (Eq, Show)

-- | One distribution file with its source coordinate and optional metadata companion.
data Artifact = Artifact
    { artEntryKey :: EntryKey
    -- ^ The coordinate in its source snapshot. Admission preserves it unchanged.
    , artFilename :: Text
    , artUrl :: Text
    , artHashes :: [Hash]
    -- ^ Integrity digests. The client verifies the download against these.
    , artCoreMetadata :: Maybe CoreMetadata
    -- ^ The optional companion declaration, independent of distribution integrity.
    , artSize :: Maybe Int
    {- ^ The registry-declared size, if reported. Not always the tarball byte count: npm populates
    it from @dist.unpackedSize@, the size of the unpacked tree.
    -}
    }
    deriving stock (Eq, Show)

{- | The ecosystem-agnostic snapshot of one package /version/: the signals a rule sees and the
artifact facts that merge, admission, serving and the mirror read. Adapters project into it.
-}
data PackageDetails = PackageDetails
    { pkgName :: PackageName
    -- ^ The package identity this snapshot belongs to.
    , pkgVersion :: Version
    -- ^ The specific version this snapshot describes.
    , pkgPublishedAt :: Maybe UTCTime
    {- ^ When this version was published, if known (absent from some cheap
    metadata views).
    -}
    , pkgInstallCode :: CodeExecSignal
    -- ^ Whether installing the version executes code.
    , pkgDependencySources :: DependencySources
    -- ^ Whether installing the version fetches a dependency from outside the registry.
    , pkgAvailability :: Availability
    -- ^ Whether the version is offered, deprecated, or withdrawn.
    , pkgArtifacts :: NonEmpty Artifact
    -- ^ The non-empty distribution set for this version.
    }
    deriving stock (Eq, Show)

{- | A registry-independent package view containing per-version 'PackageDetails'.
Adapters own the wire format and project into this type.
-}
data PackageInfo = PackageInfo
    { infoName :: PackageName
    -- ^ The package identity this document describes.
    , infoVersions :: Map Text PackageDetails
    {- ^ Every published version, keyed by its __raw version string__. A 'Version' has no 'Ord',
    so ordering goes through 'Ecluse.Core.Version.compareVersions', never a derived instance.
    -}
    , infoDistTags :: Map Text Version
    -- ^ Distribution tags (e.g. @"latest"@, @"next"@) to the 'Version' they point at.
    , infoInvalidEntries :: [InvalidEntry]
    {- ^ The malformed entries the projection __dropped__ rather than failing the whole
    document, kept so the serve path can surface them to an operator.
    -}
    }
    deriving stock (Eq, Show)

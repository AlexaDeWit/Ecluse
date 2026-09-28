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

    -- * The name charset boundary
    isAsciiNameComponent,

    -- * Canonical keys
    canonicalise,

    -- * Normalised signals
    CodeExecSignal (..),
    Availability (..),

    -- * Artifacts
    Artifact (..),
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

{- | An npm scope, stored without its leading @\'\@\'@ (the scope of @\@myorg\/pkg@ is
@"myorg"@). 'mkScope' normalises away a leading @\'\@\'@, so equality does not depend on
how the scope was written.
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

{- | A package identity, decoupled from any registry's wire format and built with
'mkPackageName'. Equality and ordering read @('pkgEcosystem', 'pkgNamespace',
'pkgCanonical')@ only, so @Flask@ and @flask@ are one PyPI package and two npm ones.
-}
data PackageName = PackageName
    { pkgEcosystem :: Ecosystem
    -- ^ The ecosystem this name belongs to.
    , pkgNamespace :: Maybe Scope
    -- ^ The scope, if scoped (npm @\@scope\/name@). 'Nothing' for PyPI/RubyGems.
    , pkgCanonical :: ShortText
    -- ^ The normalised matching key: PEP 503 for PyPI, verbatim for npm and RubyGems.
    , pkgDisplay :: ShortText
    -- ^ The name as published, read back as 'Text' through 'renderPackageName'.
    , pkgBaseName :: ShortText
    {- ^ The base name with any @\@scope\/@ prefix dropped. It is not part of identity. Read it
    back through 'unscopedName'.
    -}
    }
    deriving stock (Show)

-- The fields that constitute identity: the display form is not one of them.
nameKey :: PackageName -> (Ecosystem, Maybe Scope, ShortText)
nameKey n = (pkgEcosystem n, pkgNamespace n, pkgCanonical n)

instance Eq PackageName where
    a == b = nameKey a == nameKey b

instance Ord PackageName where
    compare a b = compare (nameKey a) (nameKey b)

{- | Build a 'PackageName', normalising the canonical key for the ecosystem: PEP 503 for
PyPI, verbatim for npm and RubyGems.
-}
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

-- | Render a package name in its native wire form (the display name).
renderPackageName :: PackageName -> Text
renderPackageName = TS.toText . pkgDisplay

-- | The unscoped (base) name as 'Text': @\@babel\/code-frame@ reads back as @code-frame@.
unscopedName :: PackageName -> Text
unscopedName = TS.toText . pkgBaseName

{- | Whether one component of a package name is ASCII with no control character: the boundary
every ecosystem's grammar rests on, because an invisible codepoint renders two names as one.
-}
isAsciiNameComponent :: Text -> Bool
isAsciiNameComponent = T.all (\ch -> isAscii ch && not (isControl ch))

{- | Whether installing a version executes code (the cross-ecosystem unification
of npm install scripts, PyPI sdist builds, and RubyGems native extensions).
-}
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

-- | Whether a version is offered, advisory-deprecated, or withdrawn.
data Availability
    = -- | Offered normally.
      Available
    | -- | Advisory deprecation (npm), still resolvable. Carries the message.
      Deprecated Text
    | {- | Withdrawn from resolution (PyPI yank keeps the file, RubyGems yank
      removes it). Carries the reason, if given.
      -}
      Yanked (Maybe Text)
    deriving stock (Eq, Show)

{- | One distribution file for a version. A version owns a 'NonEmpty' list of
these: npm has exactly one, PyPI has an sdist plus many wheels, RubyGems has one
per platform.
-}
data Artifact = Artifact
    { artEntryKey :: EntryKey
    -- ^ The coordinate in its source snapshot. Admission preserves it unchanged.
    , artFilename :: Text
    , artUrl :: Text
    , artHashes :: [Hash]
    -- ^ Integrity digests. The client verifies the download against these.
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
    , pkgAvailability :: Availability
    -- ^ Whether the version is offered, deprecated, or withdrawn.
    , pkgArtifacts :: NonEmpty Artifact
    -- ^ The version's distribution files (one for npm, many for PyPI/RubyGems).
    }
    deriving stock (Eq, Show)

{- | The packument-level view of a package ('PackageDetails' is the per-/version/ snapshot
embedded within it). A registry adapter projects its packument into this type, so the proxy
core never sees the wire format.
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

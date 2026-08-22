-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The package domain model: the ecosystem-agnostic vocabulary for the rules
engine.

These types capture everything the proxy needs to reason about a package version
while staying decoupled from any registry's wire format. A registry adapter (npm,
PyPI, RubyGems) projects its responses into these types, so nothing above the
registry layer sees a registry-specific structure.

Three pieces of this vocabulary earn their own sibling module. The 'Ecosystem' tag
lives in "Ecluse.Core.Ecosystem", shared with the version engine and the registry
adapters. Version identity and ordering live in "Ecluse.Core.Version", and
'PackageDetails' embeds a 'Version' here. The integrity-digest vocabulary ('Hash',
'HashAlg', and the Subresource-Integrity forms) lives in
"Ecluse.Core.Package.Hash", re-exported here in full. Import those modules directly
when you need to name or build their types.

The design follows two principles synthesised from the protocol research (see
@docs\/research\/synthesis.md@):

* __Rules consume normalised signals, not raw fields__. The risky behaviours
  differ on the wire (npm install scripts, PyPI sdist builds, RubyGems native
  extensions) but collapse to one signal, 'CodeExecSignal'. Trust likewise
  collapses to 'Trust'. A rule never learns which ecosystem it is looking at.

* __Signal availability is explicit__. A signal the adapter has not determined, or
  cannot determine cheaply, is 'CodeExecUnknown' \/ 'TrustUnknown' \/ 'Nothing'. A
  pure rule then abstains rather than guessing, and the effectful tier can resolve
  it later (see @docs\/architecture.md@ → "Rules Engine").
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
    pkgDisplay,
    pkgBaseName,
    renderPackageName,
    unscopedName,

    -- * Normalised signals
    CodeExecSignal (..),
    Trust (..),
    TrustEvidence (..),
    Availability (..),

    -- * Artifacts
    Artifact (..),
    ArtifactKind (..),
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

    -- * Dependencies

    -- * People
    Person (..),

    -- * Per-version details
    PackageDetails (..),

    -- * Packument-level view
    PackageInfo (..),
    InvalidEntry (..),
    InvalidEntryKind (..),
) where

import Data.Aeson (Value)
import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (..))
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
import Ecluse.Core.Version (Version)

{- | An npm scope, stored without its leading @\'\@\'@ (the scope of
@\@myorg\/pkg@ is @"myorg"@). Construct it with 'mkScope', which normalises away a
leading @\'\@\'@, so equality does not depend on how the scope was written.

A scope is a bulk-stored, equality-only identifier: an allow-list key, and part of
'PackageName' identity. It is therefore held as 'ShortText'. The
@'Text' -> 'ShortText'@ conversion happens once in 'mkScope' and the reverse once in
'unScope'\/'renderScope', never in a hot loop (see STYLE.md §6).
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

{- | A package identity, decoupled from any registry's wire format.

Identity differs by ecosystem: npm has scopes and is case-sensitive, PyPI
normalises per PEP 503, and RubyGems is verbatim. The type is therefore __opaque__.
Build it with 'mkPackageName', which records the ecosystem, computes the
'pkgCanonical' key for equality and matching, and keeps a 'pkgDisplay' form for
faithful rendering. Equality and ordering use
@('pkgEcosystem', 'pkgNamespace', 'pkgCanonical')@ only, never the display or base
form. So @Flask@ and @flask@ are the same PyPI package but different npm ones.
-}
data PackageName = PackageName
    { pkgEcosystem :: Ecosystem
    -- ^ The ecosystem this name belongs to.
    , pkgNamespace :: Maybe Scope
    -- ^ The scope, if scoped (npm @\@scope\/name@). 'Nothing' for PyPI/RubyGems.
    , pkgCanonical :: ShortText
    {- ^ The normalised key for equality and matching: PEP 503 for PyPI, verbatim
    for npm and RubyGems. Held as 'ShortText', because it is an equality and 'Ord'
    key that 'mkPackageName' normalises once and nothing slices afterwards.
    -}
    , pkgDisplay :: ShortText
    {- ^ The name as published, for rendering and round-tripping. Held as
    'ShortText'. Read it back as 'Text' through 'renderPackageName'.
    -}
    , pkgBaseName :: ShortText
    {- ^ The unscoped base name: the published name with any @\@scope\/@ prefix
    dropped (@\@babel\/code-frame@ → @code-frame@). That is exactly the bare name the
    constructor receives, so 'mkPackageName' stores it structurally. The npm
    tarball\/path layer and the mirror queue then read it as a field rather than
    re-slicing the display form. It is not part of identity, like 'pkgDisplay'. Held
    as 'ShortText' and read back as 'Text' through 'unscopedName'.
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

{- | Build a 'PackageName', normalising the canonical key for the ecosystem.

The display form is the scope-joined raw name (@\@scope\/name@ when scoped). The
canonical key is that form normalised: PEP 503 lower-casing and @[-_.]+@→@-@
collapsing for PyPI, verbatim for npm and RubyGems.
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

-- Normalise a display name into its canonical matching key for an ecosystem.
canonicalise :: Ecosystem -> Text -> Text
canonicalise = \case
    Npm -> id
    RubyGems -> id
    PyPI -> normalisePyPI

{- PEP 503 name normalisation: lower-case, and collapse each run of
@\'-\'@\/@\'_\'@\/@\'.\'@ to a single @\'-\'@.
-}
normalisePyPI :: Text -> Text
normalisePyPI t =
    T.intercalate "-"
        . filter (not . T.null)
        . T.splitOn "-"
        $ T.map (\c -> if c == '_' || c == '.' then '-' else c) (T.toLower t)

-- | Render a package name in its native wire form (the display name).
renderPackageName :: PackageName -> Text
renderPackageName = TS.toText . pkgDisplay

{- | The unscoped (base) name as 'Text' (@\@babel\/code-frame@ → @code-frame@): the
'ShortText' 'pkgBaseName' field read back. This is the single home for the bare name
the npm tarball/path layer and the mirror queue both need. 'mkPackageName' stores it
structurally rather than reconstructing it by rendering and then string-stripping the
scope.
-}
unscopedName :: PackageName -> Text
unscopedName = TS.toText . pkgBaseName

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

{- | The trust\/provenance signal for a version. The /how/ of trust differs by
ecosystem (npm @dist.signatures@, PyPI PEP 740 attestations, RubyGems signed
gems\/MFA), but 'TrustEvidence' captures it, so rules stay ecosystem-blind.
-}
data Trust
    = -- | Determined trusted, with the evidence supporting it.
      Trusted (NonEmpty TrustEvidence)
    | -- | Determined: no trust signal established.
      Untrusted
    | -- | Not yet determined (e.g. signature verification needs a fetch).
      TrustUnknown
    deriving stock (Eq, Show)

{- | A normalised reason a version is trusted. The adapter maps its ecosystem's
mechanism onto this vocabulary.
-}
data TrustEvidence
    = -- | The artifact is cryptographically signed.
      Signed
    | -- | The artifact carries a provenance attestation (e.g. Sigstore).
      Attested
    | -- | The version was published under enforced multi-factor auth.
      MfaPublished
    | -- | An ecosystem mechanism not yet in this vocabulary (escape hatch).
      OtherEvidence Text
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

-- | What kind of distribution file an artifact is.
data ArtifactKind
    = -- | An npm tarball.
      Tarball
    | -- | A PyPI source distribution (building it may execute code).
      Sdist
    | -- | A PyPI wheel. Carries its compatibility tag (e.g. @"cp310-…"@).
      Wheel Text
    | -- | A RubyGems gem. Carries its platform (@"ruby"@ = pure).
      Gem Text
    deriving stock (Eq, Show)

{- | One distribution file for a version. A version owns a 'NonEmpty' list of
these: npm has exactly one, PyPI has an sdist plus many wheels, RubyGems has one
per platform.
-}
data Artifact = Artifact
    { artFilename :: Text
    , artUrl :: Text
    , artKind :: ArtifactKind
    , artHashes :: [Hash]
    -- ^ Integrity digests. The client verifies the download against these.
    , artSize :: Maybe Int
    {- ^ The registry-declared size, if reported. Not guaranteed to be the tarball
    byte count: npm populates it from @dist.unpackedSize@, the size of the unpacked
    tree.
    -}
    , artInterpreter :: Maybe Text
    -- ^ Interpreter constraint (@requires-python@ \/ @required_ruby_version@).
    , artYanked :: Bool
    {- ^ Whether this individual file is yanked (PyPI per-file yank). For
    ecosystems that yank whole versions this stays 'False' and
    'pkgAvailability' carries the status instead.
    -}
    , artProvenance :: Maybe Text
    -- ^ URL of a provenance\/attestation bundle, if any.
    }
    deriving stock (Eq, Show)

-- | A person associated with a package (author, maintainer, or publisher).
data Person = Person
    { personName :: Text
    -- ^ The person's name, as declared by the package.
    , personEmail :: Maybe Text
    -- ^ Their email address, if given.
    , personUrl :: Maybe Text
    -- ^ A homepage / profile URL, if given.
    }
    deriving stock (Eq, Ord, Show)

{- | The ecosystem-agnostic snapshot of a single package /version/ that the
rules engine evaluates. A registry adapter projects its wire format into this. The
rules engine never sees anything else, and never branches on the ecosystem.
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
    , pkgTrust :: Trust
    -- ^ The trust\/provenance signal for the version.
    , pkgAvailability :: Availability
    -- ^ Whether the version is offered, deprecated, or withdrawn.
    , pkgArtifacts :: NonEmpty Artifact
    -- ^ The version's distribution files (one for npm, many for PyPI/RubyGems).
    , pkgLicenses :: [Text]
    -- ^ Declared licenses (SPDX expressions/ids). There may be several.
    , pkgPublisher :: Maybe Person
    {- ^ Who published __this__ version, if known (provenance).

    Dependencies and maintainers are __deliberately not modelled__. Dependencies
    are structurally redundant on the decision surface. A dependency only ever
    matters when a client fetches it, and that fetch comes back through this same
    gate and receives its own verdict. Gating a parent's dependency /list/ would
    therefore duplicate the gate that already sits on every child request. Not
    modelling them means the wire layer does not even parse them. A heavy packument
    carries thousands of per-version dependency entries, and parsing them would be
    pure cost on the hot path. A malformed entry there can no longer drop the
    version either: it degrades. The raw document still carries everything to the
    client untouched, so the served surface is lossless whatever the decision
    surface models. If a dependency-reading rule ever genuinely lands, restore the
    @Dependency@\/@DepKind@ vocabulary from history and re-model then.
    -}
    }
    deriving stock (Eq, Show)

{- | The packument-level view of a package: the whole-package metadata document
('PackageDetails' is the per-/version/ snapshot embedded within it). A registry
adapter projects a registry's packument, the npm full-metadata document, into this
type. The proxy core then reasons over it without ever seeing the wire format.
-}
data PackageInfo = PackageInfo
    { infoName :: PackageName
    -- ^ The package identity this document describes.
    , infoVersions :: Map Text PackageDetails
    {- ^ Every published version, keyed by its __raw version string__ (the
    packument's own key). Each 'PackageDetails' still carries its parsed 'Version'.
    The map is keyed by 'Text' because a 'Version' has no 'Ord': ordering goes
    through 'Ecluse.Core.Version.compareVersions', never a derived instance. See
    "Ecluse.Core.Version".
    -}
    , infoDistTags :: Map Text Version
    {- ^ Distribution tags (e.g. @"latest"@, @"next"@) to the 'Version' they
    point at.
    -}
    , infoInvalidEntries :: [InvalidEntry]
    {- ^ The malformed entries the projection __dropped__ rather than failing the
    whole document on, kept so the serve path can surface them to an operator. A
    version's publish time lives on its 'PackageDetails.pkgPublishedAt', and
    serialisation reconstructs the npm @time@ object from it, so this field does
    __not__ duplicate it. Only the /dropped/ entries appear here.
    -}
    }
    deriving stock (Eq, Show)

{- | A single packument entry a registry projection __dropped__ as malformed rather
than failing the entire document. It is kept so the drop is observable rather than
silent: an operator can see that an upstream served a malformed entry, and which one.
Each ecosystem's projection populates this from its own wire shape, so the
drop-and-track contract is the same across npm, PyPI, and RubyGems.
-}
data InvalidEntry = InvalidEntry
    { invalidKind :: InvalidEntryKind
    -- ^ Which kind of packument entry the projection dropped.
    , invalidKey :: Text
    {- ^ The map key the dropped entry sat under: the raw version string for a
    version manifest or publish time, the tag name for a dist-tag.
    -}
    , invalidValue :: Value
    {- ^ The __offending value__ ('Value' is lossless), so an operator can see what the
    upstream sent rather than only a reason string. A dropped publish time keeps its raw
    bad date here even though the version's 'pkgPublishedAt' folds to 'Nothing'. That
    keeps the gating value (absent) separate from the diagnostic (the raw bytes). Render
    it at log time, truncating if it is large. A projection reduces a __URL__ value to its
    authority before it records the entry
    ('Ecluse.Core.Security.Authority.authorityLabel'). This record reaches a log line, and
    an upstream-supplied @dist.tarball@ can carry a credential in its userinfo or query
    string.
    -}
    , invalidReason :: Text
    -- ^ Why the entry could not be projected (the decode error), for the operator log.
    }
    deriving stock (Eq, Show)

{- | Which kind of registry-document entry a dropped 'InvalidEntry' came from. A version
manifest drop removes a serve candidate, which is fail-closed for that one version. A
dist-tag or publish-time drop loses only that advisory datum, and the version it referred
to still resolves.
-}
data InvalidEntryKind
    = -- | A @versions@ entry whose manifest did not project (no @dist@\/@tarball@, an unusable @version@).
      InvalidVersionManifest
    | -- | A @dist-tags@ entry whose target was not a usable version string.
      InvalidDistTag
    | -- | A @time@ entry, keyed by a present version, that was not a decodable instant.
      InvalidPublishTime
    deriving stock (Eq, Show)

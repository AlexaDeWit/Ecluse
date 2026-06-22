{- | The package domain model — ecosystem-agnostic vocabulary for the rules
engine.

These types capture everything the proxy needs to reason about a package
version while staying decoupled from any registry's wire format. Registry
adapters (npm, PyPI, RubyGems) are responsible for projecting their responses
into these types; nothing above the registry layer sees registry-specific
structures.

Two pieces of this vocabulary earn their own sibling module: the 'Ecosystem' tag
lives in "Ecluse.Ecosystem" (shared with the version engine and the registry
adapters), and version identity and ordering live in "Ecluse.Version" (a
'Version' is embedded here in 'PackageDetails'). Import those modules directly
when you need to name or build their types.

The design follows two principles synthesised from the protocol research (see
@docs\/research\/synthesis.md@):

* __Rules consume normalised signals, not raw fields.__ The risky behaviours
  differ on the wire (npm install scripts, PyPI sdist builds, RubyGems native
  extensions) but collapse to one signal — 'CodeExecSignal'. Trust likewise
  collapses to 'Trust'. A rule never learns which ecosystem it is looking at.

* __Signal availability is explicit.__ A signal the adapter has not (or cannot
  cheaply) determine is 'CodeExecUnknown' \/ 'TrustUnknown' \/ 'Nothing', so a
  pure rule abstains rather than guessing and the effectful tier can resolve it
  later (see @docs\/architecture.md@ → "Rules Engine").
-}
module Ecluse.Package (
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
    renderPackageName,

    -- * Normalised signals
    CodeExecSignal (..),
    Trust (..),
    TrustEvidence (..),
    Availability (..),

    -- * Artifacts
    Artifact (..),
    ArtifactKind (..),
    Hash (..),
    HashAlg (..),

    -- * Dependencies
    Dependency (..),
    DepKind (..),

    -- * People
    Person (..),

    -- * Per-version details
    PackageDetails (..),

    -- * Packument-level view
    PackageInfo (..),
) where

import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Version (Version)

{- | An npm scope, stored without its leading @\'\@\'@ (the scope of
@\@myorg\/pkg@ is @"myorg"@). Construct via 'mkScope', which normalises away
a leading @\'\@\'@ so equality is independent of how the scope was written.
-}
newtype Scope = Scope Text
    deriving stock (Eq, Ord, Show)

-- | Build a 'Scope', tolerating an optional leading @\'\@\'@.
mkScope :: Text -> Scope
mkScope raw = Scope (fromMaybe raw (T.stripPrefix "@" raw))

-- | The bare scope text, without the leading @\'\@\'@.
unScope :: Scope -> Text
unScope (Scope s) = s

-- | Render a scope in npm wire form, with the leading @\'\@\'@.
renderScope :: Scope -> Text
renderScope (Scope s) = "@" <> s

{- | A package identity, decoupled from any registry's wire format.

Identity differs by ecosystem — npm has scopes and is case-sensitive, PyPI
normalises per PEP 503, RubyGems is verbatim — so the type is __opaque__:
build it with 'mkPackageName', which records the ecosystem, computes a
'pkgCanonical' key used for equality\/matching, and keeps a 'pkgDisplay' form
for faithful rendering. Equality and ordering are on
@('pkgEcosystem', 'pkgNamespace', 'pkgCanonical')@ only — never the display
form — so @Flask@ and @flask@ are the same PyPI package but different npm ones.
-}
data PackageName = PackageName
    { pkgEcosystem :: Ecosystem
    -- ^ The ecosystem this name belongs to.
    , pkgNamespace :: Maybe Scope
    -- ^ The scope, if scoped (npm @\@scope\/name@). 'Nothing' for PyPI/RubyGems.
    , pkgCanonical :: Text
    {- ^ The normalised key for equality and matching (PEP 503 for PyPI;
    verbatim for npm/RubyGems).
    -}
    , pkgDisplay :: Text
    -- ^ The name as published, for rendering and round-tripping.
    }
    deriving stock (Show)

-- | The fields that constitute identity (the display form is excluded).
nameKey :: PackageName -> (Ecosystem, Maybe Scope, Text)
nameKey n = (pkgEcosystem n, pkgNamespace n, pkgCanonical n)

instance Eq PackageName where
    a == b = nameKey a == nameKey b

instance Ord PackageName where
    compare a b = compare (nameKey a) (nameKey b)

{- | Build a 'PackageName', normalising the canonical key for the ecosystem.

The display form is the scope-joined raw name (@\@scope\/name@ when scoped);
the canonical key is that form normalised: PEP 503 lower-casing and
@[-_.]+@→@-@ collapsing for PyPI, verbatim for npm and RubyGems.
-}
mkPackageName :: Ecosystem -> Maybe Scope -> Text -> PackageName
mkPackageName eco ns raw =
    PackageName
        { pkgEcosystem = eco
        , pkgNamespace = ns
        , pkgCanonical = canonicalise eco display
        , pkgDisplay = display
        }
  where
    display = case ns of
        Just s -> renderScope s <> "/" <> raw
        Nothing -> raw

-- | Normalise a display name into its canonical matching key for an ecosystem.
canonicalise :: Ecosystem -> Text -> Text
canonicalise = \case
    Npm -> id
    RubyGems -> id
    PyPI -> normalisePyPI

{- | PEP 503 name normalisation: lower-case, and collapse each run of
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
renderPackageName = pkgDisplay

-- ── normalised signals ───────────────────────────────────────────────────────

{- | Whether installing a version executes code (the cross-ecosystem unification
of npm install scripts, PyPI sdist builds, and RubyGems native extensions).
-}
data CodeExecSignal
    = -- | Determined: installation runs no code.
      NoCodeOnInstall
    | -- | Determined: installation runs code; the text says how (audit trail).
      RunsCodeOnInstall Text
    | {- | Not yet determined (e.g. the RubyGems gemspec has not been fetched).
      Pure rules abstain; the effectful tier may resolve it.
      -}
      CodeExecUnknown
    deriving stock (Eq, Show)

{- | The trust\/provenance signal for a version. The /how/ of trust differs by
ecosystem (npm @dist.signatures@, PyPI PEP 740 attestations, RubyGems signed
gems\/MFA) but is captured as 'TrustEvidence' so rules stay ecosystem-blind.
-}
data Trust
    = -- | Determined trusted, with the evidence supporting it.
      Trusted (NonEmpty TrustEvidence)
    | -- | Determined: no trust signal established.
      Untrusted
    | -- | Not yet determined (e.g. signature verification needs a fetch).
      TrustUnknown
    deriving stock (Eq, Show)

{- | A normalised reason a version is trusted; the adapter maps its ecosystem's
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
    | -- | Advisory deprecation (npm); still resolvable. Carries the message.
      Deprecated Text
    | {- | Withdrawn from resolution (PyPI yank keeps the file; RubyGems yank
      removes it). Carries the reason, if given.
      -}
      Yanked (Maybe Text)
    deriving stock (Eq, Show)

-- ── artifacts ────────────────────────────────────────────────────────────────

-- | A hash algorithm an integrity digest is computed with.
data HashAlg
    = SHA1
    | SHA256
    | SHA512
    | MD5
    | Blake2b
    | {- | A Subresource-Integrity string (npm @dist.integrity@), e.g.
      @"sha512-…"@, carried whole.
      -}
      SRI
    deriving stock (Eq, Ord, Show)

-- | An integrity digest of an artifact.
data Hash = Hash
    { hashAlg :: HashAlg
    -- ^ The algorithm the digest was computed with.
    , hashValue :: Text
    {- ^ The digest itself, in the algorithm's wire encoding (e.g. hex, or the
    whole @sha512-…@ string for 'SRI').
    -}
    }
    deriving stock (Eq, Show)

-- | What kind of distribution file an artifact is.
data ArtifactKind
    = -- | An npm tarball.
      Tarball
    | -- | A PyPI source distribution (building it may execute code).
      Sdist
    | -- | A PyPI wheel; carries its compatibility tag (e.g. @"cp310-…"@).
      Wheel Text
    | -- | A RubyGems gem; carries its platform (@"ruby"@ = pure).
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
    -- ^ Integrity digests; the client verifies the download against these.
    , artSize :: Maybe Int
    -- ^ Size in bytes, if known.
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

-- ── dependencies ─────────────────────────────────────────────────────────────

-- | The role a dependency plays.
data DepKind
    = Runtime
    | Dev
    | Optional
    | Peer
    deriving stock (Eq, Show)

{- | A declared dependency. The constraint is kept as __raw text__ (semver range
\/ PEP 508 \/ @Gem::Requirement@) — lossless and ecosystem-agnostic — and is
parsed only if a rule ever needs to compare it.
-}
data Dependency = Dependency
    { depName :: Text
    -- ^ The dependency's name.
    , depConstraint :: Text
    -- ^ The raw version constraint, as declared.
    , depKind :: DepKind
    -- ^ The role the dependency plays (runtime, dev, optional, peer).
    , depMarker :: Maybe Text
    -- ^ A raw environment marker \/ extras qualifier (PEP 508), if any.
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
rules engine evaluates. A registry adapter projects its wire format into this;
the rules engine never sees anything else, and never branches on the ecosystem.
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
    -- ^ The version's distribution files (one for npm; many for PyPI/RubyGems).
    , pkgLicenses :: [Text]
    -- ^ Declared licenses (SPDX expressions/ids); may be several.
    , pkgPublisher :: Maybe Person
    -- ^ Who published __this__ version, if known (provenance).
    , pkgMaintainers :: [Person]
    -- ^ The package's maintainers (distinct from the per-version publisher).
    , pkgDependencies :: [Dependency]
    -- ^ Declared dependencies, constraints kept raw.
    }
    deriving stock (Eq, Show)

{- | The packument-level view of a package: the whole-package metadata document
('PackageDetails' is the per-/version/ snapshot embedded within it). A registry
adapter projects a registry's packument (the npm full-metadata document) into
this; the proxy core reasons over it without ever seeing the wire format.
-}
data PackageInfo = PackageInfo
    { infoName :: PackageName
    -- ^ The package identity this document describes.
    , infoVersions :: Map Text PackageDetails
    {- ^ Every published version, keyed by its __raw version string__ (the
    packument's own key). Each 'PackageDetails' still carries its parsed
    'Version'; the map is keyed by 'Text' because a 'Version' has no 'Ord'
    (ordering goes through 'Ecluse.Version.compareVersions', never a derived
    instance) — see "Ecluse.Version".
    -}
    , infoDistTags :: Map Text Version
    {- ^ Distribution tags (e.g. @"latest"@, @"next"@) to the 'Version' they
    point at.
    -}
    , infoPublishedAt :: Map Text UTCTime
    {- ^ Per-version publish times (the npm @time@ object), keyed by raw version
    string, when known.
    -}
    }
    deriving stock (Eq, Show)

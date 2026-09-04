-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The package domain model: the ecosystem-agnostic vocabulary the rules engine reasons
over. A registry adapter (npm, PyPI, RubyGems) projects its wire responses into these types,
so nothing above the registry layer sees a registry-specific structure. Three pieces live in
sibling modules and are only named here: 'Ecosystem' in "Ecluse.Core.Ecosystem", 'Version' in
"Ecluse.Core.Version", and the integrity-digest vocabulary in "Ecluse.Core.Package.Hash",
re-exported in full.

== Design principles

The protocol research (@docs\/research\/synthesis.md@) settled two. Rules consume normalised
signals, not raw fields: npm install scripts, PyPI sdist builds, and RubyGems native
extensions differ on the wire and collapse to one 'CodeExecSignal', so a rule never learns
which ecosystem it is looking at. Signal availability is explicit: what an adapter has not
determined, or cannot determine cheaply, is 'CodeExecUnknown', 'TrustUnknown', or 'Nothing',
so a pure rule abstains rather than guessing and the effectful tier resolves it later
(the "Rules Engine" section of @docs\/architecture.md@).
-}
module Ecluse.Core.Package (
    -- * Scopes
    Scope,
    mkScope,
    unScope,
    renderScope,

    -- * PyPI name prefixes
    PyPIPrefix,
    mkPyPIPrefix,
    underPyPIPrefix,

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
import Data.Char (isAlphaNum, isAscii, isControl)
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

{- | A package identity, decoupled from any registry's wire format. Build it with
'mkPackageName'.

Equality and ordering use @('pkgEcosystem', 'pkgNamespace', 'pkgCanonical')@ only, never
the display or base form. So @Flask@ and @flask@ are the same PyPI package but different
npm ones.
-}
data PackageName = PackageName
    { pkgEcosystem :: Ecosystem
    -- ^ The ecosystem this name belongs to.
    , pkgNamespace :: Maybe Scope
    -- ^ The scope, if scoped (npm @\@scope\/name@). 'Nothing' for PyPI/RubyGems.
    , pkgCanonical :: ShortText
    {- ^ The normalised key for equality and matching: PEP 503 for PyPI, verbatim for npm
    and RubyGems.
    -}
    , pkgDisplay :: ShortText
    {- ^ The name as published, for rendering and round-tripping. Held as
    'ShortText'. Read it back as 'Text' through 'renderPackageName'.
    -}
    , pkgBaseName :: ShortText
    {- ^ The unscoped base name, with any @\@scope\/@ prefix dropped (@\@babel\/code-frame@ →
    @code-frame@). It is not part of identity. Read it back through 'unscopedName'.
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

{- | A PyPI name prefix, held in PEP 503 canonical form. PyPI has no structural namespace the
way npm has a 'Scope', so a deployment that owns a family of distributions owns a prefix of
their names.
-}
newtype PyPIPrefix = PyPIPrefix ShortText
    deriving stock (Eq, Show)

{- | Build a prefix, canonicalising it the way 'mkPackageName' canonicalises a name. 'Nothing'
for text no PyPI name can start with: outside PEP 503's alphabet, or canonicalising to nothing,
which would cover every name on PyPI.
-}
mkPyPIPrefix :: Text -> Maybe PyPIPrefix
mkPyPIPrefix raw
    | T.null canonical || not (T.all canonicalPyPIChar canonical) = Nothing
    | otherwise = Just (PyPIPrefix (TS.fromText canonical))
  where
    canonical = normalisePyPI raw

-- PEP 503's canonical alphabet, the form 'normalisePyPI' leaves a legal name in.
canonicalPyPIChar :: Char -> Bool
canonicalPyPIChar c = c == '-' || (isAscii c && isAlphaNum c)

{- | Whether a name sits under a prefix. The match ends at PEP 503's separator, so @acme@ covers
@acme-tools@ and not @acmeco@, which is a name the deployment does not own. The bare prefix is
not itself covered: a deployment owning that distribution declares it as a name.
-}
underPyPIPrefix :: PyPIPrefix -> PackageName -> Bool
underPyPIPrefix (PyPIPrefix prefix) name =
    pkgEcosystem name == PyPI && TS.isPrefixOf (prefix <> "-") (pkgCanonical name)

-- | Render a package name in its native wire form (the display name).
renderPackageName :: PackageName -> Text
renderPackageName = TS.toText . pkgDisplay

-- | The unscoped (base) name as 'Text' (@\@babel\/code-frame@ → @code-frame@).
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

    Dependencies and maintainers are __deliberately not modelled__. A dependency is gated when
    the client fetches it, through this same gate, so the wire layer never parses the thousands
    of per-version dependency entries a heavy packument carries.
    -}
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
    {- ^ Every published version, keyed by its __raw version string__ (the packument's own
    key). A 'Version' has no 'Ord', so ordering goes through
    'Ecluse.Core.Version.compareVersions', never a derived instance.
    -}
    , infoDistTags :: Map Text Version
    {- ^ Distribution tags (e.g. @"latest"@, @"next"@) to the 'Version' they
    point at.
    -}
    , infoInvalidEntries :: [InvalidEntry]
    {- ^ The malformed entries the projection __dropped__ rather than failing the whole
    document, kept so the serve path can surface them to an operator. Only /dropped/ entries
    appear here: a version's own publish time lives on 'PackageDetails.pkgPublishedAt'.
    -}
    }
    deriving stock (Eq, Show)

{- | A single packument entry a registry projection __dropped__ as malformed rather than
failing the entire document. It is kept so the drop is observable: an operator can see that
an upstream served a malformed entry, and which one.
-}
data InvalidEntry = InvalidEntry
    { invalidKind :: InvalidEntryKind
    -- ^ Which kind of packument entry the projection dropped.
    , invalidKey :: Text
    {- ^ The map key the dropped entry sat under: the raw version string for a
    version manifest or publish time, the tag name for a dist-tag.
    -}
    , invalidValue :: Value
    {- ^ The __offending value__, so an operator can see what the upstream sent rather than
    only a reason string. Render it at log time, truncating if it is large. A projection
    reduces a __URL__ value to its authority before recording the entry
    ('Ecluse.Core.Security.Authority.authorityLabel'): this record reaches a log line, and an
    upstream-supplied @dist.tarball@ can carry a credential in its userinfo or query string.
    -}
    , invalidReason :: Text
    -- ^ Why the entry could not be projected (the decode error), for the operator log.
    }
    deriving stock (Eq, Show)

{- | Which kind of registry-document entry a dropped 'InvalidEntry' came from. A version
manifest drop removes a serve candidate, fail-closed for that one version. A dist-tag or
publish-time drop loses only that advisory datum, and the version still resolves.
-}
data InvalidEntryKind
    = -- | A @versions@ entry whose manifest did not project (no @dist@\/@tarball@, an unusable @version@).
      InvalidVersionManifest
    | -- | A @dist-tags@ entry whose target was not a usable version string.
      InvalidDistTag
    | -- | A @time@ entry, keyed by a present version, that was not a decodable instant.
      InvalidPublishTime
    deriving stock (Eq, Show)

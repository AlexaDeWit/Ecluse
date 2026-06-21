{- | The package domain model — ecosystem-agnostic vocabulary for the rules
engine.

These types capture everything the proxy needs to reason about a package
version while staying decoupled from any registry's wire format. Registry
adapters (npm, PyPI, RubyGems) are responsible for projecting their responses
into these types; nothing above the registry layer sees registry-specific
structures.

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
    -- * Ecosystems
    Ecosystem (..),

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

    -- * Versions
    Version,
    mkVersion,
    unVersion,
    renderVersion,
    compareVersion,

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
) where

import Data.Char (isAlpha, isDigit)
import Data.Text qualified as T
import Data.Time (UTCTime)

-- | The package ecosystem an identity, version, or snapshot belongs to.
data Ecosystem
    = Npm
    | PyPI
    | RubyGems
    deriving stock (Eq, Ord, Show)

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
    -- ^ The normalised key for equality and matching (PEP 503 for PyPI;
    -- verbatim for npm/RubyGems).
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
normalisePyPI = T.toLower . T.pack . collapse . toString
  where
    collapse [] = []
    collapse (c : cs)
        | isSep c = '-' : collapse (dropWhile isSep cs)
        | otherwise = c : collapse cs
    isSep c = c == '-' || c == '_' || c == '.'

-- | Render a package name in its native wire form (the display name).
renderPackageName :: PackageName -> Text
renderPackageName = pkgDisplay

{- | A package version string (e.g. @"1.2.3"@). Kept opaque: the raw text is
stored verbatim and __no__ 'Ord' is derived, because lexicographic ordering is
wrong for every version grammar (@"10.0.0" < "9.0.0"@). Use 'compareVersion',
which knows the ecosystem's grammar.
-}
newtype Version = Version Text
    deriving stock (Eq, Show)

-- | Wrap raw version text. No parsing or validation is performed.
mkVersion :: Text -> Version
mkVersion = Version

-- | The raw version text.
unVersion :: Version -> Text
unVersion (Version v) = v

-- | Render a version in wire form (the raw text).
renderVersion :: Version -> Text
renderVersion = unVersion

{- | Compare two versions using the ordering rules of the given ecosystem:
semver (npm), PEP 440 (PyPI), or @Gem::Version@ (RubyGems). Total: malformed
input is parsed leniently (non-numeric segments default to zero) rather than
failing. The PEP 440 parser assumes the registry-normalised form PyPI serves
(e.g. @"1.0a1"@, @"1.0.post1"@, @"1.0.dev1"@).
-}
compareVersion :: Ecosystem -> Version -> Version -> Ordering
compareVersion eco (Version a) (Version b) = case eco of
    Npm -> compareSemver a b
    PyPI -> comparePep440 a b
    RubyGems -> compareGem a b

-- | Parse a text segment as a non-negative integer, defaulting to @0@.
parseInt :: Text -> Integer
parseInt t = fromMaybe 0 (readMaybe (toString t))

-- | Compare two integer lists element-wise, zero-padding the shorter.
compareInts :: [Integer] -> [Integer] -> Ordering
compareInts [] [] = EQ
compareInts (x : xs) (y : ys) = compare x y <> compareInts xs ys
compareInts (x : xs) [] = compare x 0 <> compareInts xs []
compareInts [] (y : ys) = compare 0 y <> compareInts [] ys

{- | A version token: a numeric run or a textual run. Its 'Ord' is the
RubyGems\/PEP 440 local rule — numeric tokens outrank textual ones, numerics
compare numerically, text compares lexically. (Semver prerelease ordering is the
opposite and is handled separately in 'compareSemver'.)
-}
data VToken = VNum Integer | VStr Text
    deriving stock (Eq, Show)

instance Ord VToken where
    compare (VNum m) (VNum n) = compare m n
    compare (VStr s) (VStr t) = compare s t
    compare (VNum _) (VStr _) = GT
    compare (VStr _) (VNum _) = LT

-- | Compare semver versions (npm).
compareSemver :: Text -> Text -> Ordering
compareSemver a b =
    compareInts (coreOf a) (coreOf b) <> comparePre (preOf a) (preOf b)
  where
    -- Drop build metadata (after '+'), then split core from prerelease on '-'.
    noBuild = T.takeWhile (/= '+')
    coreText t = T.takeWhile (/= '-') (noBuild t)
    preText t = T.drop 1 (T.dropWhile (/= '-') (noBuild t))
    coreOf t = map parseInt (T.splitOn "." (coreText t))
    preOf t = let p = preText t in if T.null p then [] else T.splitOn "." p

{- | Compare semver prerelease identifier lists. An empty list (no prerelease)
outranks a non-empty one; among prereleases, fewer identifiers rank lower and
numeric identifiers rank below alphanumeric ones (semver §11).
-}
comparePre :: [Text] -> [Text] -> Ordering
comparePre a b = case (null a, null b) of
    (True, True) -> EQ
    (True, False) -> GT
    (False, True) -> LT
    (False, False) -> idents a b
  where
    idents [] [] = EQ
    idents [] (_ : _) = LT
    idents (_ : _) [] = GT
    idents (x : xs) (y : ys) = ident x y <> idents xs ys
    ident x y = case (asNum x, asNum y) of
        (Just nx, Just ny) -> compare nx ny
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> compare x y
    asNum t =
        if not (T.null t) && T.all isDigit t
            then readMaybe (toString t) :: Maybe Integer
            else Nothing

-- | Compare @Gem::Version@ versions (RubyGems).
compareGem :: Text -> Text -> Ordering
compareGem a b = go (gemTokens a) (gemTokens b)
  where
    go [] [] = EQ
    go (x : xs) (y : ys) = compare x y <> go xs ys
    go (x : xs) [] = compare x (VNum 0) <> go xs []
    go [] (y : ys) = compare (VNum 0) y <> go [] ys

{- | Tokenise a gem version: split on @\'.\'@, then split each segment into
maximal digit and non-digit runs.
-}
gemTokens :: Text -> [VToken]
gemTokens = concatMap segTokens . T.splitOn "."
  where
    segTokens = map classify . T.groupBy (\c1 c2 -> isDigit c1 == isDigit c2)
    classify g = if not (T.null g) && T.all isDigit g then VNum (parseInt g) else VStr g

-- | Compare PEP 440 versions (PyPI) via the canonical sort key.
comparePep440 :: Text -> Text -> Ordering
comparePep440 a b = compare (pep440Key a) (pep440Key b)

{- | The PEP 440 ordering key:
@(epoch, release, pre, post, dev, local)@. Release has trailing zeros stripped
(@1.0 == 1.0.0@). The @pre@\/@post@\/@dev@ ranks encode PEP 440's None-handling:
a final release outranks any prerelease, a post-release outranks a final, and a
dev release ranks below its non-dev sibling.
-}
pep440Key ::
    Text ->
    (Integer, [Integer], (Int, Integer), (Int, Integer), (Int, Integer), [VToken])
pep440Key raw =
    (epoch, release, pre, post, dev, localTokens)
  where
    normalised = fromMaybe lowered (T.stripPrefix "v" lowered)
    lowered = T.toLower (T.strip raw)

    (mainPart, localPart) = T.breakOn "+" normalised
    localTokens =
        if T.null localPart
            then []
            else map localTok (T.split (`elem` ['.', '-', '_']) (T.drop 1 localPart))
    localTok seg =
        if not (T.null seg) && T.all isDigit seg then VNum (parseInt seg) else VStr seg

    (epoch, afterEpoch) = case T.breakOn "!" mainPart of
        (e, rest)
            | T.null rest -> (0, mainPart)
            | otherwise -> (parseInt e, T.drop 1 rest)

    releaseText = T.takeWhile (\c -> isDigit c || c == '.') afterEpoch
    suffix = T.drop (T.length releaseText) afterEpoch
    release =
        stripTrailingZeros
            (map parseInt (filter (not . T.null) (T.splitOn "." releaseText)))

    (afterDev, dev) = case T.breakOn "dev" suffix of
        (before, rest)
            | T.null rest -> (suffix, (1, 0))
            | otherwise -> (before, (0, parseInt (T.takeWhile isDigit (T.drop 3 rest))))
    (afterPost, post) = case T.breakOn "post" afterDev of
        (before, rest)
            | T.null rest -> (afterDev, (0, 0))
            | otherwise -> (before, (1, parseInt (T.takeWhile isDigit (T.drop 4 rest))))
    pre = parsePre afterPost

    stripTrailingZeros = reverse . dropWhile (== 0) . reverse

{- | Parse a PEP 440 prerelease label into a @(stageRank, number)@ key. A final
release (no prerelease) is @(3, 0)@ so it outranks @a@ (0), @b@ (1), @rc@ (2).
-}
parsePre :: Text -> (Int, Integer)
parsePre s0 =
    let s = T.dropWhile (\c -> not (isDigit c) && not (isAlpha c)) s0
        label = T.takeWhile isAlpha s
        num = parseInt (T.takeWhile isDigit (T.dropWhile isAlpha s))
     in if T.null label then (3, 0) else (rankOf label, num)
  where
    rankOf l
        | l == "a" || l == "alpha" = 0
        | l == "b" || l == "beta" = 1
        | otherwise = 2 -- "rc", "c", "pre", "preview"

{- | Whether installing a version executes code (the cross-ecosystem unification
of npm install scripts, PyPI sdist builds, and RubyGems native extensions).
-}
data CodeExecSignal
    = -- | Determined: installation runs no code.
      NoCodeOnInstall
    | -- | Determined: installation runs code; the text says how (audit trail).
      RunsCodeOnInstall Text
    | -- | Not yet determined (e.g. the RubyGems gemspec has not been fetched).
      -- Pure rules abstain; the effectful tier may resolve it.
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
    | -- | Withdrawn from resolution (PyPI yank keeps the file; RubyGems yank
      -- removes it). Carries the reason, if given.
      Yanked (Maybe Text)
    deriving stock (Eq, Show)

-- | A hash algorithm an integrity digest is computed with.
data HashAlg
    = SHA1
    | SHA256
    | SHA512
    | MD5
    | Blake2b
    | -- | A Subresource-Integrity string (npm @dist.integrity@), e.g.
      -- @"sha512-…"@, carried whole.
      SRI
    deriving stock (Eq, Ord, Show)

-- | An integrity digest of an artifact.
data Hash = Hash
    { hashAlg :: HashAlg
    , hashValue :: Text
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
    -- ^ Whether this individual file is yanked (PyPI per-file yank). For
    -- ecosystems that yank whole versions this stays 'False' and
    -- 'pkgAvailability' carries the status instead.
    , artProvenance :: Maybe Text
    -- ^ URL of a provenance\/attestation bundle, if any.
    }
    deriving stock (Eq, Show)

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
    , depMarker :: Maybe Text
    -- ^ A raw environment marker \/ extras qualifier (PEP 508), if any.
    }
    deriving stock (Eq, Show)

-- | A person associated with a package (author, maintainer, or publisher).
data Person = Person
    { personName :: Text
    , personEmail :: Maybe Text
    , personUrl :: Maybe Text
    }
    deriving stock (Eq, Ord, Show)

{- | The ecosystem-agnostic snapshot of a single package /version/ that the
rules engine evaluates. A registry adapter projects its wire format into this;
the rules engine never sees anything else, and never branches on the ecosystem.
-}
data PackageDetails = PackageDetails
    { pkgName :: PackageName
    , pkgVersion :: Version
    , pkgPublishedAt :: Maybe UTCTime
    -- ^ When this version was published, if known (absent from some cheap
    -- metadata views).
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
    , pkgDependencies :: [Dependency]
    -- ^ Declared dependencies, constraints kept raw.
    }
    deriving stock (Eq, Show)

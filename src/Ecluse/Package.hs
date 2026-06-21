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
    versionKey,
    mkVersion,
    unVersion,
    renderVersion,
    compareVersions,
    VersionKey,
    parseVersionKey,
    VersionError (..),

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

import Data.Char (isAlphaNum, isDigit)
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

{- | A package version.

The raw text is kept verbatim for faithful round-trip (version strings are
embedded in artifact URLs and re-served), while a parsed, canonical
'VersionKey' — present only when the raw text parses for its ecosystem — is what
ordering uses. Build with 'mkVersion' (total: an unparseable version is still
represented, just with no key, so a proxy never drops a version over a parser
gap) or 'parseVersionKey' when you want the parse error.

There is deliberately __no__ 'Ord' on 'Version': comparison goes through
'compareVersions', which is defined only on parsed keys, so non-canonical text
can never reach the comparator.
-}
data Version = Version
    { versionRaw :: Text
    -- ^ The version as published — used for rendering and round-tripping only,
    -- never for ordering decisions.
    , versionKey :: Maybe VersionKey
    -- ^ The parsed, canonical ordering key; 'Nothing' if the raw text could not
    -- be parsed for its ecosystem (ordering rules then abstain).
    }
    deriving stock (Eq, Show)

{- | Build a 'Version', parsing the raw text into a canonical key when possible.
Total: a version that does not parse is still represented (with no key) rather
than rejected, so a proxy never drops a version over a parser gap.
-}
mkVersion :: Ecosystem -> Text -> Version
mkVersion eco raw = Version raw (rightToMaybe (parseVersionKey eco raw))

-- | The raw version text.
unVersion :: Version -> Text
unVersion = versionRaw

-- | Render a version in wire form (the raw text).
renderVersion :: Version -> Text
renderVersion = versionRaw

{- | Compare two versions by their canonical keys. 'Nothing' if either version
did not parse (its key is absent) — an ordering-based rule should then abstain,
mirroring the other "unknown signal" cases ('CodeExecUnknown', 'TrustUnknown').
-}
compareVersions :: Version -> Version -> Maybe Ordering
compareVersions a b = compare <$> versionKey a <*> versionKey b

-- | Why a version string failed to parse.
newtype VersionError = VersionError
    { versionErrorMessage :: Text
    }
    deriving stock (Eq, Show)

{- | The parsed, canonical, comparable form of a version. __Opaque__: the only
way to obtain one is 'parseVersionKey', so a 'VersionKey' always holds a
well-formed, normalised version — the comparator structurally cannot see
non-canonical input (parse, don't validate). Its 'Ord' is meaningful only within
a single ecosystem, which is the only case that ever arises (one compares
versions of one package).
-}
data VersionKey
    = NpmKey SemverKey
    | PyPIKey Pep440Key
    | RubyGemsKey GemKey
    deriving stock (Eq, Ord, Show)

{- | Parse raw version text into a canonical 'VersionKey' for its ecosystem, or
report why it could not be parsed. This is the parsing boundary: downstream code
holds a 'VersionKey' and relies on it being valid.
-}
parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey
parseVersionKey eco raw = case eco of
    Npm -> note (NpmKey <$> parseSemver raw)
    PyPI -> note (PyPIKey <$> parsePep440 raw)
    RubyGems -> note (RubyGemsKey <$> parseGem raw)
  where
    note = maybe (Left (VersionError ("unparseable version: " <> raw))) Right

{- | A version token: a numeric run or a textual run. Its 'Ord' is the RubyGems
\/ PEP 440-local rule — numeric tokens outrank textual ones, numerics compare
numerically, text compares lexically. (Semver prerelease ordering is the
opposite and is handled in 'parseSemver'.)
-}
data VToken = VNum Integer | VStr Text
    deriving stock (Eq, Show)

instance Ord VToken where
    compare (VNum m) (VNum n) = compare m n
    compare (VStr s) (VStr t) = compare s t
    compare (VNum _) (VStr _) = GT
    compare (VStr _) (VNum _) = LT

-- | Parse a non-empty, all-digit segment as an integer.
parseNumSeg :: Text -> Maybe Integer
parseNumSeg t
    | not (T.null t) && T.all isDigit t = readMaybe (toString t)
    | otherwise = Nothing

-- | Read an all-digit (already validated) run as an integer, defaulting to 0.
numOr0 :: Text -> Integer
numOr0 t = if T.null t then 0 else fromMaybe 0 (readMaybe (toString t))

-- | The first non-'Nothing' result of applying @f@ across the list.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = asum . map f

-- ── semver (npm) ───────────────────────────────────────────────────────────

{- | A semver prerelease identifier; numeric identifiers rank below alphanumeric
ones (semver §11), encoded by the constructor order.
-}
data SemverPreId = SemverNum Integer | SemverText Text
    deriving stock (Eq, Ord, Show)

{- | A semver prerelease: an actual prerelease ranks below the final release, so
'SemverPre' is ordered before 'SemverFinal'.
-}
data SemverPre = SemverPre [SemverPreId] | SemverFinal
    deriving stock (Eq, Ord, Show)

-- | A parsed semver version: numeric core, then prerelease.
data SemverKey = SemverKey [Integer] SemverPre
    deriving stock (Eq, Ord, Show)

{- | Parse a semver version (numeric core, optional @-prerelease@, ignoring
@+build@ metadata). Fails on a non-numeric core or malformed identifiers.
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver raw = do
    let core0 = T.takeWhile (/= '+') raw
        (coreText, preRest) = T.break (== '-') core0
        preText = T.drop 1 preRest
    core <- traverse parseNumSeg (T.splitOn "." coreText)
    guard (not (null core))
    pre <-
        if T.null preText
            then pure SemverFinal
            else SemverPre <$> traverse parsePreId (T.splitOn "." preText)
    pure (SemverKey core pre)
  where
    parsePreId t
        | T.null t = Nothing
        | T.all isDigit t = SemverNum <$> readMaybe (toString t)
        | T.all isIdentChar t = Just (SemverText t)
        | otherwise = Nothing
    isIdentChar c = isAlphaNum c || c == '-'

-- ── Gem::Version (RubyGems) ──────────────────────────────────────────────────

{- | A parsed @Gem::Version@: a flat token list compared with zero-padding, with
numeric tokens outranking textual ones (see 'VToken').
-}
newtype GemKey = GemKey [VToken]
    deriving stock (Eq, Show)

instance Ord GemKey where
    compare (GemKey a) (GemKey b) = compareGemTokens a b

-- | Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

{- | Parse a @Gem::Version@: dot-separated alphanumeric segments, each split into
maximal digit and letter runs. Fails on empty or non-alphanumeric segments.
-}
parseGem :: Text -> Maybe GemKey
parseGem raw = do
    let trimmed = T.strip raw
        segs = T.splitOn "." trimmed
    guard (not (T.null trimmed))
    guard (all validSeg segs)
    let toks = concatMap segTokens segs
    guard (not (null toks))
    pure (GemKey toks)
  where
    validSeg s = not (T.null s) && T.all isAlphaNum s
    segTokens = map classify . T.groupBy (\c1 c2 -> isDigit c1 == isDigit c2)
    classify g = if T.all isDigit g then VNum (numOr0 g) else VStr g

-- ── PEP 440 (PyPI) ───────────────────────────────────────────────────────────

{- | A parsed PEP 440 version as its canonical ordering key:
@(epoch, release, pre, post, dev, local)@. Release has trailing zeros stripped
(@1.0 == 1.0.0@). The rank tuples encode PEP 440's None-handling:

* @p440Pre@ is @(band, stage, n)@ where @band@ is __0__ for a dev release with
  no prerelease and no post (it sorts /before/ all prereleases, e.g.
  @1.0.dev1 < 1.0a1@), __1__ for an actual prerelease (with @stage@ a\/b\/rc and
  its number), and __2__ for a final or post release (sorts after prereleases).
* @p440Post@ is @(0,0)@ when absent, so a final sorts below any post-release.
* @p440Dev@ is @(0,n)@ when present and @(1,0)@ when absent, so a dev release
  sorts below its non-dev sibling.
-}
data Pep440Key = Pep440Key
    { p440Epoch :: Integer
    , p440Release :: [Integer]
    , p440Pre :: (Int, Int, Integer)
    , p440Post :: (Int, Integer)
    , p440Dev :: (Int, Integer)
    , p440Local :: [VToken]
    }
    deriving stock (Eq, Ord, Show)

{- | Parse a PEP 440 version, canonicalising non-normalised spellings
(@1.0ALPHA1@, @1.0-1@, trailing zeros, …). Fails if the string is not a valid
PEP 440 version (e.g. no release, or unrecognised trailing text).
-}
parsePep440 :: Text -> Maybe Pep440Key
parsePep440 raw = do
    let lowered = T.toLower (T.strip raw)
        noV = fromMaybe lowered (T.stripPrefix "v" lowered)
        (mainPart, localRaw) = T.breakOn "+" noV
    guard (T.all isMainChar mainPart)
    let (epochText, afterEpoch) = case T.breakOn "!" mainPart of
            (e, rest)
                | T.null rest -> ("", mainPart)
                | otherwise -> (e, T.drop 1 rest)
    epoch <- if T.null epochText then pure 0 else parseNumSeg epochText
    let releaseText = T.takeWhile (\c -> isDigit c || c == '.') afterEpoch
        suffix = T.drop (T.length releaseText) afterEpoch
    release <- traverse parseNumSeg (filter (not . T.null) (T.splitOn "." releaseText))
    guard (not (null release))
    (mPre, mPost, mDev) <- parsePep440Suffix suffix
    localToks <- parseLocal localRaw
    let pre = case mPre of
            Just (stage, n) -> (1, stage, n)
            Nothing
                | isJust mDev && isNothing mPost -> (0, 0, 0)
                | otherwise -> (2, 0, 0)
        post = case mPost of
            Nothing -> (0, 0)
            Just n -> (1, n)
        dev = case mDev of
            Nothing -> (1, 0)
            Just n -> (0, n)
    pure
        Pep440Key
            { p440Epoch = epoch
            , p440Release = stripTrailingZeros release
            , p440Pre = pre
            , p440Post = post
            , p440Dev = dev
            , p440Local = localToks
            }
  where
    isMainChar c = isAlphaNum c || c == '.' || c == '!' || c == '-' || c == '_'
    stripTrailingZeros = reverse . dropWhile (== 0) . reverse
    parseLocal lr
        | T.null lr = Just []
        | otherwise =
            let segs = T.split (`elem` ['.', '-', '_']) (T.drop 1 lr)
             in if all (\s -> not (T.null s) && T.all isAlphaNum s) segs
                    then Just (map localTok segs)
                    else Nothing
    localTok s = if T.all isDigit s then VNum (numOr0 s) else VStr s

{- | Consume a PEP 440 suffix into its prerelease\/post\/dev parts (each absent
or present), failing if any text is left unconsumed (so trailing garbage is
rejected). The banding into a sort key happens in 'parsePep440'.
-}
parsePep440Suffix ::
    Text -> Maybe (Maybe (Int, Integer), Maybe Integer, Maybe Integer)
parsePep440Suffix s0 =
    let (pre, s1) = consumePre s0
        (post, s2) = consumePost s1
        (dev, s3) = consumeDev s2
     in if T.null s3 then Just (pre, post, dev) else Nothing

-- | Drop one optional separator (@.@\/@-@\/@_@) from the front.
dropSep :: Text -> Text
dropSep s = case T.uncons s of
    Just (c, rest) | c == '.' || c == '-' || c == '_' -> rest
    _ -> s

{- | Consume an optional prerelease label into @Just (stage, n)@ (stage 0\/1\/2
for a\/b\/rc); 'Nothing' if absent.
-}
consumePre :: Text -> (Maybe (Int, Integer), Text)
consumePre s =
    case firstJust (\(lbl, rk) -> (,) rk <$> T.stripPrefix lbl (dropSep s)) preLabels of
        Nothing -> (Nothing, s)
        Just (rk, afterLabel) ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (rk, numOr0 digits), rest)
  where
    preLabels =
        [ ("alpha", 0)
        , ("beta", 1)
        , ("preview", 2)
        , ("pre", 2)
        , ("rc", 2)
        , ("a", 0)
        , ("b", 1)
        , ("c", 2)
        ]

{- | Consume an optional post-release (@.postN@, @.revN@, or @-N@) into @Just n@;
'Nothing' if absent.
-}
consumePost :: Text -> (Maybe Integer, Text)
consumePost s =
    case firstJust (\lbl -> T.stripPrefix lbl (dropSep s)) ["post", "rev"] of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> case T.stripPrefix "-" s of
            Just afterDash ->
                let (digits, rest) = T.span isDigit afterDash
                 in if T.null digits then (Nothing, s) else (Just (numOr0 digits), rest)
            Nothing -> (Nothing, s)

-- | Consume an optional dev-release (@.devN@) into @Just n@; 'Nothing' if absent.
consumeDev :: Text -> (Maybe Integer, Text)
consumeDev s =
    case T.stripPrefix "dev" (dropSep s) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> (Nothing, s)

-- ── normalised signals ───────────────────────────────────────────────────────

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

-- ── artifacts ────────────────────────────────────────────────────────────────

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
    -- ^ The algorithm the digest was computed with.
    , hashValue :: Text
    -- ^ The digest itself, in the algorithm's wire encoding (e.g. hex, or the
    -- whole @sha512-…@ string for 'SRI').
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
    -- ^ The package's maintainers (distinct from the per-version publisher).
    , pkgDependencies :: [Dependency]
    -- ^ Declared dependencies, constraints kept raw.
    }
    deriving stock (Eq, Show)

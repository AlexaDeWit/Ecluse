{- | Version identity and ordering.

A 'Version' carries the raw text verbatim (version strings are embedded in
artifact URLs and re-served, so fidelity matters) alongside a parsed, canonical
'VersionKey' — present only when the raw text parses for its ecosystem. Ordering
goes through 'compareVersions', which is defined __only__ on parsed keys, so
non-canonical text can never reach the comparator (/parse, don't validate/).

Parsing is per-ecosystem and selected by the 'Ecosystem' tag from
"Ecluse.Ecosystem": semver for npm, PEP 440 for PyPI, @Gem::Version@ for
RubyGems. The three grammars and their ordering rules are the bulk of this
module; they are kept __private__ — callers build with 'mkVersion' (total) or
'parseVersionKey' (reports the parse error) and compare with 'compareVersions'.

This vocabulary is consumed by "Ecluse.Package" (@PackageDetails@ holds a
'Version') and the rules engine ("Ecluse.Rules"). See
@docs\/architecture\/domain-model.md@ → "Version".
-}
module Ecluse.Version (
    -- * Versions
    Version,
    versionKey,
    mkVersion,
    unVersion,
    renderVersion,
    compareVersions,

    -- * Canonical ordering keys
    VersionKey,
    parseVersionKey,
    VersionError (..),
) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text qualified as T

import Ecluse.Ecosystem (Ecosystem (..))

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
    { -- The version as published — used for rendering and round-tripping only,
      -- never for ordering decisions.
      versionRaw :: Text
    , versionKey :: Maybe VersionKey
    {- ^ The parsed, canonical ordering key; 'Nothing' if the raw text could not
    be parsed for its ecosystem (ordering rules then abstain).
    -}
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
mirroring the other "unknown signal" cases (@CodeExecUnknown@, @TrustUnknown@).
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

{- A version token: a numeric run or a textual run. Its 'Ord' is the RubyGems
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

-- Parse a non-empty, all-digit segment as an integer.
parseNumSeg :: Text -> Maybe Integer
parseNumSeg t
    | not (T.null t) && T.all isDigit t = readMaybe (toString t)
    | otherwise = Nothing

-- Read an all-digit (already validated) run as an integer, defaulting to 0.
numOr0 :: Text -> Integer
numOr0 t = if T.null t then 0 else fromMaybe 0 (readMaybe (toString t))

-- The first non-'Nothing' result of applying @f@ across the list.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = asum . map f

-- ── semver (npm) ───────────────────────────────────────────────────────────

{- A semver prerelease identifier; numeric identifiers rank below alphanumeric
ones (semver §11), encoded by the constructor order.
-}
data SemverPreId = SemverNum Integer | SemverText Text
    deriving stock (Eq, Ord, Show)

{- A semver prerelease: an actual prerelease ranks below the final release, so
'SemverPre' is ordered before 'SemverFinal'.
-}
data SemverPre = SemverPre [SemverPreId] | SemverFinal
    deriving stock (Eq, Ord, Show)

-- A parsed semver version: numeric core, then prerelease.
data SemverKey = SemverKey [Integer] SemverPre
    deriving stock (Eq, Ord, Show)

{- Parse a semver version (numeric core, optional @-prerelease@, ignoring
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

{- A parsed @Gem::Version@: a flat token list compared with zero-padding, with
numeric tokens outranking textual ones (see 'VToken').
-}
newtype GemKey = GemKey [VToken]
    deriving stock (Eq, Show)

instance Ord GemKey where
    compare (GemKey a) (GemKey b) = compareGemTokens a b

-- Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

{- Parse a @Gem::Version@: dot-separated alphanumeric segments, each split into
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

{- A parsed PEP 440 version as its canonical ordering key:
@(epoch, release, pre, post, dev, local)@. Release has trailing zeros stripped
(@1.0 == 1.0.0@). The rank tuples encode PEP 440's None-handling:

\* @p440Pre@ is @(band, stage, n)@ where @band@ is __0__ for a dev release with
  no prerelease and no post (it sorts /before/ all prereleases, e.g.
  @1.0.dev1 < 1.0a1@), __1__ for an actual prerelease (with @stage@ a\/b\/rc and
  its number), and __2__ for a final or post release (sorts after prereleases).
\* @p440Post@ is @(0,0)@ when absent, so a final sorts below any post-release.
\* @p440Dev@ is @(0,n)@ when present and @(1,0)@ when absent, so a dev release
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

{- Parse a PEP 440 version, canonicalising non-normalised spellings
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

{- Consume a PEP 440 suffix into its prerelease\/post\/dev parts (each absent
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

-- Drop one optional separator (@.@\/@-@\/@_@) from the front.
dropSep :: Text -> Text
dropSep s = case T.uncons s of
    Just (c, rest) | c == '.' || c == '-' || c == '_' -> rest
    _ -> s

{- Consume an optional prerelease label into @Just (stage, n)@ (stage 0\/1\/2
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

{- Consume an optional post-release (@.postN@, @.revN@, or @-N@) into @Just n@;
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

-- Consume an optional dev-release (@.devN@) into @Just n@; 'Nothing' if absent.
consumeDev :: Text -> (Maybe Integer, Text)
consumeDev s =
    case T.stripPrefix "dev" (dropSep s) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> (Nothing, s)

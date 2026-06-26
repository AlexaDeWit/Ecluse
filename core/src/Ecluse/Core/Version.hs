{- | Version identity and ordering.

A 'Version' carries the raw text verbatim (version strings are embedded in
artifact URLs and re-served, so fidelity matters) alongside a parsed, canonical
'VersionKey' — present only when the raw text parses for its ecosystem. Ordering
goes through 'compareVersions', which is defined __only__ on parsed keys, so
non-canonical text can never reach the comparator (/parse, don't validate/).

Parsing is per-ecosystem and selected by the 'Ecosystem' tag from
"Ecluse.Core.Ecosystem": semver for npm ("Ecluse.Core.Version.Semver"), PEP 440 for PyPI
("Ecluse.Core.Version.Pep440"), @Gem::Version@ for RubyGems ("Ecluse.Core.Version.Gem").
Each grammar and its ordering rules live in its own module; this module is the
agnostic abstraction that dispatches to them on the 'Ecosystem' tag. The grammar
modules are kept __private__ — callers build with 'mkVersion' (total) or
'parseVersionKey' (reports the parse error) and compare with 'compareVersions'.

This vocabulary is consumed by "Ecluse.Core.Package" (@PackageDetails@ holds a
'Version') and the rules engine ("Ecluse.Core.Rules"). See
@docs\/architecture\/domain-model.md@ → "Version".
-}
module Ecluse.Core.Version (
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
    isStable,

    -- * Resolving @dist-tags.latest@
    selectLatest,
) where

import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Version.Gem (GemKey, isGemStable, parseGem)
import Ecluse.Core.Version.Pep440 (Pep440Key, isPep440Stable, parsePep440)
import Ecluse.Core.Version.Semver (SemverKey, isSemverStable, parseSemver)

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

{- | Whether a parsed version is a __stable__ (final, non-prerelease) release.
The notion is ecosystem-specific, dispatched on the key's constructor:

* __semver (npm)__ — stable iff there is no @-prerelease@ component (the
  prerelease is 'SemverFinal'). So @1.0.0@ is stable; @1.0.0-rc.1@ and
  @2.0.0-beta@ are not.
* __PEP 440 (PyPI)__ — stable iff it is neither a pre-release (@a@\/@b@\/@rc@)
  nor a dev release. Post-releases /are/ stable. So @1.0@ and @1.0.post1@ are
  stable; @1.0a1@, @1.0rc1@, @1.0.dev1@ and @1.0a1.dev2@ are not.
* __RubyGems__ — stable iff no segment contains a letter (the version is
  all-numeric). So @1.0.0@ is stable; @1.0.0.pre@ and @1.2.0.rc1@ are not.

Used by 'selectLatest' to prefer a stable release when @dist-tags.latest@ must
be repointed.

>>> isStable <$> parseVersionKey Npm "1.0.0"
Right True
>>> isStable <$> parseVersionKey Npm "1.0.0-rc.1"
Right False
>>> isStable <$> parseVersionKey PyPI "1.0.post1"
Right True
>>> isStable <$> parseVersionKey PyPI "1.0a1.dev2"
Right False
>>> isStable <$> parseVersionKey RubyGems "1.0.0.pre"
Right False
-}
isStable :: VersionKey -> Bool
isStable = \case
    NpmKey k -> isSemverStable k
    PyPIKey k -> isPep440Stable k
    RubyGemsKey k -> isGemStable k

{- | Resolve @dist-tags.latest@ for a packument after denied\/undecidable
versions have been filtered out — the __keep-unless-denied, stable-preferring__
rule from @docs\/architecture\/rules-engine.md@ ("Applying verdicts to a
packument"). @chosen@ is the source's currently-tagged @latest@ (if any);
@survivors@ is the surviving versions. The result, when present, is always one
of @survivors@, so the caller can use its 'unVersion' as the tag string.

The resolution, in order:

* If @survivors@ is empty, there is nothing to point at — 'Nothing'.
* __Keep:__ if @chosen@ survives (by raw text), return it unchanged. This is the
  identity on a single-input packument and never /promotes/ a prerelease over a
  maintainer's chosen stable @latest@.
* __Repoint__ (only when the chosen @latest@ did not survive): among survivors
  with a parseable key, prefer the maximum __stable__ one; if none are stable,
  the maximum __prerelease__ one. (Within one ecosystem parseable keys are
  totally ordered, so 'compareVersions' is total over them.)
* __No parseable survivor:__ to keep the result naming a present version, fall
  back to the lexicographically-smallest survivor by 'unVersion'. An unparseable
  version never outranks a parseable one.
-}
selectLatest :: Maybe Version -> [Version] -> Maybe Version
selectLatest chosen survivors = case nonEmpty survivors of
    Nothing -> Nothing
    Just survivors1
        | Just v <- chosen, survives v -> Just v
        | otherwise -> Just (repoint survivors1)
  where
    survives v = any ((== unVersion v) . unVersion) survivors

    repoint :: NonEmpty Version -> Version
    repoint ne =
        let keyed = [(v, k) | v <- toList ne, Just k <- [versionKey v]]
            stable = [vk | vk@(_, k) <- keyed, isStable k]
         in case nonEmpty stable of
                Just s -> fst (maxByKey s)
                Nothing -> case nonEmpty keyed of
                    Just ks -> fst (maxByKey ks)
                    -- No parseable survivor: deterministic, present fallback.
                    Nothing -> NE.head (NE.sortWith unVersion ne)

    -- Greatest by canonical key; total because every element carries a key.
    maxByKey :: NonEmpty (Version, VersionKey) -> (Version, VersionKey)
    maxByKey = maximumBy (comparing snd)

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

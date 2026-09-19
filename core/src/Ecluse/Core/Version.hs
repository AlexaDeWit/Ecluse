-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Version identity and ordering.

A 'Version' keeps the raw text verbatim, because version strings are embedded in artifact
URLs and re-served. Ordering goes through 'compareVersions' on the parsed 'VersionKey',
which exists only when the raw text parses for its ecosystem, so non-canonical text can
never reach a comparator. Parsing is per-ecosystem and the grammar modules stay private:
callers build with 'mkVersion' or 'parseVersionKey'. See
@docs\/architecture\/domain-model.md@, "Version".
-}
module Ecluse.Core.Version (
    -- * Versions
    Version,
    versionKey,
    mkVersion,
    renderVersion,
    compareVersions,

    -- * Canonical ordering keys
    VersionKey,
    parseVersionKey,
    VersionError (..),
    isStable,

    -- * Canonical PEP 440 spelling
    canonicalPep440,

    -- * Resolving @dist-tags.latest@
    selectLatest,
) where

import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Version.Gem (GemKey, isGemStable, parseGem)
import Ecluse.Core.Version.Pep440 (Pep440Key, isPep440Stable, parsePep440, renderPep440)
import Ecluse.Core.Version.Semver (SemverKey, isSemverStable, parseSemver)

{- | A package version: the raw text as published, plus the parsed ordering key when the text
parses. There is deliberately __no__ 'Ord'. Comparison goes through 'compareVersions'.
-}
data Version = Version
    { -- The version as published: for rendering and round-tripping, never for ordering.
      versionRaw :: Text
    , versionKey :: Maybe VersionKey
    {- ^ The parsed, canonical ordering key. 'Nothing' if the raw text did not parse
    for its ecosystem, in which case ordering rules abstain.
    -}
    }
    deriving stock (Eq, Show)

{- | Build a 'Version', parsing the raw text into a canonical key when possible. Total: an
unparseable version is still represented, keyless, so a proxy never drops one over a parser gap.
-}
mkVersion :: Ecosystem -> Text -> Version
mkVersion eco raw = Version raw (rightToMaybe (parseVersionKey eco raw))

-- | Render a version in wire form: the raw text, verbatim as published.
renderVersion :: Version -> Text
renderVersion = versionRaw

{- | Compare two versions by their canonical keys. 'Nothing' when either version has no
key, in which case an ordering-based rule abstains.
-}
compareVersions :: Version -> Version -> Maybe Ordering
compareVersions a b = compare <$> versionKey a <*> versionKey b

{- | The parsed, canonical, comparable form of a version. The type is __opaque__ and
'parseVersionKey' is its only constructor, so the comparator cannot see non-canonical input.
-}
data VersionKey
    = NpmKey SemverKey
    | PyPIKey Pep440Key
    | RubyGemsKey GemKey
    deriving stock (Eq, Ord, Show)

{- | Parse raw version text into a canonical 'VersionKey' for its ecosystem, or report why it
did not parse. The 'Ord' on the result is meaningful only within one ecosystem.
-}
parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey
parseVersionKey eco raw = case eco of
    Npm -> note (NpmKey <$> parseSemver raw)
    PyPI -> note (PyPIKey <$> parsePep440 raw)
    RubyGems -> note (RubyGemsKey <$> parseGem raw)
  where
    note = maybe (Left (VersionError ("unparseable version: " <> raw))) Right

-- | Why a version string failed to parse.
newtype VersionError = VersionError
    { versionErrorMessage :: Text
    }
    deriving stock (Eq, Show)

{- | Whether a parsed version is a __stable__ (final, non-prerelease) release, under its own
ecosystem's notion of one.

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

{- | The one spelling a PEP 440 version canonicalises to, which a PyPI projection keys by so two
spellings of one release merge. The raw spelling survives per artifact through the filename.

>>> canonicalPep440 "1.0.0"
Just "1"
>>> canonicalPep440 "not-a-version"
Nothing
-}
canonicalPep440 :: Text -> Maybe Text
canonicalPep440 = fmap renderPep440 . parsePep440

{- | Resolve @dist-tags.latest@ over the survivors the caller left, keeping @chosen@ when it
survives so a prerelease never displaces a maintainer's stable tag. The result is a survivor.
-}
selectLatest :: Maybe Version -> [Version] -> Maybe Version
selectLatest chosen survivors = case nonEmpty survivors of
    Nothing -> Nothing
    Just survivors1
        | Just v <- chosen, survives v -> Just v
        | otherwise -> Just (repointLatest survivors1)
  where
    survives v = any ((== renderVersion v) . renderVersion) survivors

-- The repoint arm: the greatest stable key, else the greatest key of any kind, else the
-- lexicographically smallest survivor, so an unparseable set still names a present version.
repointLatest :: NonEmpty Version -> Version
repointLatest survivors =
    let keyed = [(v, k) | v <- toList survivors, Just k <- [versionKey v]]
        stable = [vk | vk@(_, k) <- keyed, isStable k]
     in case nonEmpty stable of
            Just s -> fst (maxByKey s)
            Nothing -> case nonEmpty keyed of
                Just ks -> fst (maxByKey ks)
                Nothing -> NE.head (NE.sortWith renderVersion survivors)

-- Greatest by canonical key. Total, because every element carries a key.
maxByKey :: NonEmpty (Version, VersionKey) -> (Version, VersionKey)
maxByKey = maximumBy (comparing snd)

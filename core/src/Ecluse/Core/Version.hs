-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Version identity and ecosystem ordering.
Wire text remains verbatim. Advisory identity uses 'renderVersionKey'.
Opaque keys carry parsed versions under bounded ecosystem grammars.
-}
module Ecluse.Core.Version (
    -- * Versions
    Version,
    versionKey,
    versionKeyIn,
    mkVersion,
    renderVersion,
    compareVersions,

    -- * Canonical ordering keys
    VersionKey,
    parseVersionKey,
    parseAdvisoryVersionKey,
    renderVersionKey,
    VersionError (..),
    isStable,

    -- * Canonical PEP 440 spelling
    canonicalPep440,

    -- * Resolving @dist-tags.latest@
    selectLatest,
) where

import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Version.Gem (GemKey, isGemStable, parseGem, renderGem)
import Ecluse.Core.Version.Pep440 (Pep440Key, isPep440Stable, parsePep440, renderPep440)
import Ecluse.Core.Version.Semver (SemverKey, isSemverStable, parseSemver, renderSemver)
import Ecluse.Core.Version.Token (withinVersionLength)

-- | The raw text as published, plus its ordering key when it parses. No 'Ord': compare with 'compareVersions'.
data Version = Version
    { -- The version as published: for rendering and round-tripping, never for ordering.
      versionRaw :: Text
    , -- Private, so the key a version carries is always the one 'mkVersion' builds from its text.
      versionKeyField :: Maybe VersionKey
    }
    deriving stock (Eq, Show)

-- | Build a 'Version' with its evaluated key. An unparseable version stays, keyless, so a parser gap drops nothing.
mkVersion :: Ecosystem -> Text -> Version
mkVersion eco raw = Version raw $ case parseVersionKey eco raw of
    Right key -> Just $! key
    Left _ -> Nothing

-- | Render a version in wire form: the raw text, verbatim as published.
renderVersion :: Version -> Text
renderVersion = versionRaw

-- | The canonical ordering key, or 'Nothing' when the text did not parse, so ordering rules abstain.
versionKey :: Version -> Maybe VersionKey
versionKey = versionKeyField

-- | The key 'mkVersion' builds under an ecosystem's grammar, reusing the carried key when it is that ecosystem's.
versionKeyIn :: Ecosystem -> Version -> Maybe VersionKey
versionKeyIn eco version = case versionKey version of
    Just key | keyEcosystem key == eco -> Just key
    _ -> rightToMaybe (parseVersionKey eco (versionRaw version))

-- | Compare by canonical keys. 'Nothing' when either version has no key, so an ordering rule abstains.
compareVersions :: Version -> Version -> Maybe Ordering
compareVersions a b = compare <$> versionKey a <*> versionKey b

-- | The canonical, comparable form of a version. It is __opaque__ and built only by parsing, so no comparator sees non-canonical input.
data VersionKey
    = NpmKey SemverKey
    | PyPIKey Pep440Key
    | RubyGemsKey GemKey
    deriving stock (Eq, Ord, Show)

-- | Parse version text under its ecosystem's grammar. The key's 'Ord' holds only within one ecosystem.
parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey
parseVersionKey eco raw = case eco of
    Npm -> note (NpmKey <$> parseSemver raw)
    PyPI -> note (PyPIKey <$> parsePep440 raw)
    RubyGems -> note (RubyGemsKey <$> parseGem raw)
  where
    note = maybe (Left (VersionError ("unparseable version: " <> raw))) Right

-- | Parse an advisory spelling, accepting one npm @v@ prefix within the raw length bound.
parseAdvisoryVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey
parseAdvisoryVersionKey eco raw
    | not (withinVersionLength raw) = Left (VersionError ("unparseable version: " <> raw))
    | otherwise = parseVersionKey eco (if eco == Npm then fromMaybe raw (T.stripPrefix "v" raw) else raw)

-- | Render canonical advisory identity, retaining semver build metadata.
renderVersionKey :: VersionKey -> Text
renderVersionKey = \case
    NpmKey key -> renderSemver key
    PyPIKey key -> renderPep440 key
    RubyGemsKey key -> renderGem key

-- 'parseVersionKey' builds each ecosystem's key with its own constructor.
keyEcosystem :: VersionKey -> Ecosystem
keyEcosystem = \case
    NpmKey _ -> Npm
    PyPIKey _ -> PyPI
    RubyGemsKey _ -> RubyGems

-- | Why a version string failed to parse.
newtype VersionError = VersionError
    { versionErrorMessage :: Text
    }
    deriving stock (Eq, Show)

-- | Whether a version is a final release under its ecosystem's grammar.
isStable :: VersionKey -> Bool
isStable = \case
    NpmKey k -> isSemverStable k
    PyPIKey k -> isPep440Stable k
    RubyGemsKey k -> isGemStable k

{- | Build the canonical PEP 440 spelling used to merge equivalent PyPI releases.

>>> renderVersion <$> canonicalPep440 "1.0.0"
Just "1"
>>> map (fmap renderVersion . canonicalPep440) ["2!1.0", "1.0-RC.1", "1.0-1", "1.0dev", "1.0+Ubuntu-1"]
[Just "2!1",Just "1rc1",Just "1.post1",Just "1.dev0",Just "1+ubuntu.1"]
>>> renderVersion <$> canonicalPep440 "not-a-version"
Nothing
-}
canonicalPep440 :: Text -> Maybe Version
-- Inlined so a caller that wraps the version leaves one unevaluated closure, not two.
{-# INLINE canonicalPep440 #-}
canonicalPep440 raw = do
    key <- parsePep440 raw
    let spelling = renderPep440 key
    -- A spelling can be longer than its text, and 'mkVersion' reads no key past the length bound.
    pure (Version spelling (if withinVersionLength spelling then Just $! PyPIKey key else Nothing))

-- | Resolve @dist-tags.latest@ to a survivor, keeping a surviving @chosen@ so a prerelease never displaces the maintainer's tag.
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

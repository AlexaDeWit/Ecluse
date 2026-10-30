-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Hedgehog generators of version strings, per ecosystem, and predicates over their digit runs.
'genNpm', 'genPyPI' and 'genGem' emit structurally valid versions that
'Ecluse.Core.Version.mkVersion' parses to a key. Their draw space is small, so two draws
collide often enough to reach the @EQ@ branch of an ordering law, and wide enough to span
@LT@ and @GT@.
-}
module Ecluse.Test.Version (
    genNpm,
    genPyPI,
    genGem,
    genCargo,
    genNpmNearPlain,
    genPyPINearPlain,
    digitRunIs,
    leadsWithZero,
) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | npm semver: three numbers, an optional prerelease of numeric or alphanumeric ids, and optional build metadata.
genNpm :: Gen Text
genNpm = do
    core <- T.intercalate "." <$> Gen.list (Range.singleton 3) numSeg
    pre <- Gen.maybe (("-" <>) . T.intercalate "." <$> Gen.list (Range.linear 1 3) preId)
    build <- Gen.maybe (("+" <>) <$> alnumRun)
    pure (core <> fromMaybe "" pre <> fromMaybe "" build)
  where
    preId = Gen.choice [numSeg, alnumId]

-- | PEP 440 in canonical spelling: a release, then an optional @aN@, @bN@ or @rcN@, @.postN@, and @.devN@.
genPyPI :: Gen Text
genPyPI = do
    release <- T.intercalate "." <$> Gen.list (Range.linear 1 3) numSeg
    pre <- Gen.maybe ((<>) <$> Gen.element ["a", "b", "rc"] <*> numSeg)
    post <- Gen.maybe ((".post" <>) <$> numSeg)
    dev <- Gen.maybe ((".dev" <>) <$> numSeg)
    pure (release <> fromMaybe "" pre <> fromMaybe "" post <> fromMaybe "" dev)

-- | @Gem::Version@: dot-separated numbers and an optional letter-led prerelease segment, as in @1.2.3.beta1@.
genGem :: Gen Text
genGem = do
    nums <- Gen.list (Range.linear 1 4) numSeg
    pre <- Gen.maybe gemPreSeg
    pure (T.intercalate "." (nums <> maybeToList pre))
  where
    gemPreSeg = do
        c <- Gen.element ['a' .. 'z']
        rest <- Gen.text (Range.linear 0 4) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))
        pure (T.cons c rest)

-- | Cargo SemVer: 'genNpm' beside the @u64@ boundary and one past it, a prerelease, and build-only twins.
genCargo :: Gen Text
genCargo = Gen.choice [genNpm, Gen.element ["18446744073709551615.0.0", "18446744073709551616.0.0", "1.2.3+build.1", "1.2.3+build.2", "1.2.3-rc.1", "1.2.3"]]

-- | Plain npm releases (@N.N.N@), and near misses: two or four numbers, a misspelt number, or text before or after.
genNpmNearPlain :: Gen Text
genNpmNearPlain =
    nearPlain
        (Gen.frequency [(6, pure 3), (1, pure 2), (1, pure 4)])
        ["v", "V", "=", " "]
        ["-0", "-rc.1", "+build.7", "-", "+", ".", " ", "x"]

-- | Plain PyPI releases (dotted numbers), and near misses: an epoch, a pre, post, dev or local part, a misspelt number, or a separator.
genPyPINearPlain :: Gen Text
genPyPINearPlain =
    nearPlain
        (Gen.int (Range.linear 1 5))
        ["v", "V", " ", "1!", "!"]
        ["a1", "rc1", ".post1", "-1", ".dev0", "+local.7", ".", "-", "+", " "]

-- | Whether a run is all ASCII digits, with a length that compares to 18 as given. A plain release holds numbers of at most 18 digits.
digitRunIs :: Ordering -> Text -> Bool
digitRunIs comparison run = T.all isDigit run && T.compareLength run 18 == comparison

-- | Whether a run is two or more ASCII digits led by a zero.
leadsWithZero :: Text -> Bool
leadsWithZero run = T.all isDigit run && T.isPrefixOf "0" run && T.compareLength run 1 == GT

nearPlain :: Gen Int -> [Text] -> [Text] -> Gen Text
nearPlain numbers prefixes suffixes = do
    count <- numbers
    release <- T.intercalate "." <$> Gen.list (Range.singleton count) releaseNumber
    Gen.frequency
        [ (4, pure release)
        , (1, (<> release) <$> Gen.element prefixes)
        , (1, (release <>) <$> Gen.element suffixes)
        ]

-- Mostly a plain number, else a leading zero, 18 digits, 17 to 20 digits, no digit, or a digit outside ASCII.
releaseNumber :: Gen Text
releaseNumber =
    Gen.frequency
        [ (16, show <$> Gen.word (Range.exponential 0 99999))
        , (1, ("0" <>) <$> Gen.text (Range.linear 1 3) Gen.digit)
        , (1, T.cons <$> Gen.element ['1' .. '9'] <*> Gen.text (Range.singleton 17) Gen.digit)
        , (1, Gen.text (Range.constant 17 20) Gen.digit)
        , (1, pure "")
        , (1, Gen.element ["\x663", "1\xFF11", "\xB2"])
        ]

-- A small non-negative integer segment.
numSeg :: Gen Text
numSeg = show <$> Gen.integral (Range.linear 0 (15 :: Integer))

-- A non-empty alphanumeric run, for build metadata.
alnumRun :: Gen Text
alnumRun = Gen.text (Range.linear 1 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))

-- A letter-led alphanumeric identifier: a non-numeric semver prerelease id.
alnumId :: Gen Text
alnumId = do
    c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
    rest <- Gen.text (Range.linear 0 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-"))
    pure (T.cons c rest)

{- | Hedgehog generators of structurally valid version strings, per ecosystem.

This mirrors the module under test: generators that support exercising
@Ecluse.Core.Version@ live here, under the @Ecluse.X → Ecluse.Test.X@ convention this
support library follows. Each generator emits a /structurally valid/ version
string for its ecosystem — one 'Ecluse.Core.Version.mkVersion' parses to a key — drawn
from a deliberately small space so two independent draws collide often enough to
exercise the @EQ@ branch of an ordering law, while still ranging widely enough to
span @LT@\/@GT@.

A suite that wants /invalid/ inputs too (the smoke oracle differential mixes in
deliberately malformed strings) composes these with its own adversarial generator
at the use site; that malformed-input generator is single-use and stays local to
the suite that needs it.
-}
module Ecluse.Test.Version (
    genNpm,
    genPyPI,
    genGem,
) where

import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

{- | npm semver: @MAJOR.MINOR.PATCH@ + optional @-prerelease@ (dot-separated
numeric or alphanumeric ids) + optional @+build@ metadata (the parser strips it).
-}
genNpm :: Gen Text
genNpm = do
    core <- T.intercalate "." <$> Gen.list (Range.singleton 3) numSeg
    pre <- Gen.maybe (("-" <>) . T.intercalate "." <$> Gen.list (Range.linear 1 3) preId)
    build <- Gen.maybe (("+" <>) <$> alnumRun)
    pure (core <> fromMaybe "" pre <> fromMaybe "" build)
  where
    -- Either a numeric id or a letter-led alphanumeric id; both are accepted by
    -- the semver parser.
    preId = Gen.choice [numSeg, alnumId]

{- | PEP 440 (PyPI): a release tuple + optional @aN@\/@bN@\/@rcN@ prerelease +
optional @.postN@ + optional @.devN@. All in canonical spelling so it parses.
-}
genPyPI :: Gen Text
genPyPI = do
    release <- T.intercalate "." <$> Gen.list (Range.linear 1 3) numSeg
    pre <- Gen.maybe ((<>) <$> Gen.element ["a", "b", "rc"] <*> numSeg)
    post <- Gen.maybe ((".post" <>) <$> numSeg)
    dev <- Gen.maybe ((".dev" <>) <$> numSeg)
    pure (release <> fromMaybe "" pre <> fromMaybe "" post <> fromMaybe "" dev)

{- | @Gem::Version@ (RubyGems): dot-separated numeric segments + an optional
trailing letter-led prerelease segment (e.g. @1.2.3.beta1@).
-}
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

-- | A small non-negative integer segment.
numSeg :: Gen Text
numSeg = show <$> Gen.integral (Range.linear 0 (15 :: Integer))

-- | A non-empty alphanumeric run (build metadata).
alnumRun :: Gen Text
alnumRun = Gen.text (Range.linear 1 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))

-- | A letter-led alphanumeric identifier (a non-numeric semver prerelease id).
alnumId :: Gen Text
alnumId = do
    c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
    rest <- Gen.text (Range.linear 0 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-"))
    pure (T.cons c rest)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The semver grammar and ordering (npm).

'SemverKey' wraps the [@versions@](https://hackage.haskell.org/package/versions) library's
'Data.Versions.SemVer', so parsing and precedence are the library's: semver 11 ordering, with
@+build@ metadata excluded from it. Among prerelease identifiers numeric ones rank below
alphanumeric ones, the opposite of the rule in "Ecluse.Core.Version.Token". A semver version
is stable iff it carries no prerelease.
-}
module Ecluse.Core.Version.Semver (
    SemverKey (..),
    parseSemver,
    isSemverStable,
) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Versions (SemVer (..))
import Data.Versions qualified as V

import Ecluse.Core.Version.Token (digitRuns, withinVersionLength)

-- | A parsed semver version, ordered by the @versions@ library's semver 11 precedence.
newtype SemverKey = SemverKey SemVer
    deriving stock (Show)
    deriving newtype (Eq, Ord)

{- | Parse a semver version, or 'Nothing' so an ordering rule abstains rather than drops it.
The length and digit-run bounds refuse input that would overflow the @versions@ library.
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver raw = do
    guard (withinVersionLength raw)
    guard (not (hasOverlongNumericRun raw))
    SemverKey <$> rightToMaybe (V.semver raw)

-- | Whether a semver version is stable: a final release with no prerelease component.
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey sv) = isNothing (_svPreRel sv)

{- The longest digit run guaranteed to fit the @versions@ library's fixed-width numeric
components: 18 digits is at most @10^18 - 1 < 2^63@, and a longer run might overflow silently. -}
maxNumericRun :: Int
maxNumericRun = 18

hasOverlongNumericRun :: Text -> Bool
hasOverlongNumericRun = any overlongRun . digitRuns

overlongRun :: Text -> Bool
overlongRun run = T.all isDigit run && T.compareLength run maxNumericRun == GT

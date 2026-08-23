-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The semver grammar and ordering (npm).

The [@versions@](https://hackage.haskell.org/package/versions) library backs it: a
'SemverKey' wraps its 'Data.Versions.SemVer', so parsing and precedence are the
library's. A semver version is a numeric @major.minor.patch@ core followed by an
optional @-prerelease@. It ignores @+build@ metadata, which semver §10 excludes from
precedence. Ordering follows semver §11. The numeric core compares field-by-field, a
prerelease ranks below the corresponding final release, and among prerelease
identifiers numeric ones rank below alphanumeric ones. That last rule is the opposite
of the RubyGems\/PEP 440-local rule in "Ecluse.Core.Version.Token".

A semver version is __stable__ iff it carries no prerelease.
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

{- | A parsed semver version. Its 'Ord' is the @versions@ library's semver §11 precedence, which
excludes build metadata.
-}
newtype SemverKey = SemverKey SemVer
    deriving stock (Show)
    deriving newtype (Eq, Ord)

{- | Parse a semver version. A parse failure becomes 'Nothing', so an ordering rule abstains
rather than dropping the version. 'withinVersionLength' bounds the text, and @maxNumericRun@
refuses a digit run that would silently overflow the @versions@ library's fixed-width components.
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver raw = do
    guard (withinVersionLength raw)
    guard (not (hasOverlongNumericRun raw))
    SemverKey <$> rightToMaybe (V.semver raw)

{- The longest digit run guaranteed to fit the @versions@ library's fixed-width numeric
components: 18 digits is at most @10^18 - 1 < 2^63@. A longer run might overflow silently, so
'parseSemver' refuses it. Real semver numbers are tiny, so the bound only rejects hostile input. -}
maxNumericRun :: Int
maxNumericRun = 18

{- Whether @raw@ holds a digit run long enough to overflow the @versions@ library's fixed-width
numeric components. -}
hasOverlongNumericRun :: Text -> Bool
hasOverlongNumericRun = any overlong . digitRuns
  where
    overlong run = T.all isDigit run && T.compareLength run maxNumericRun == GT

-- | Whether a semver version is stable: a final release with no prerelease component.
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey sv) = isNothing (_svPreRel sv)

{- | The semver grammar and ordering (npm).

Backed by the [@versions@](https://hackage.haskell.org/package/versions) library:
a 'SemverKey' wraps its 'Data.Versions.SemVer', so parsing and precedence are the
library's. A semver version is a numeric @major.minor.patch@ core followed by an
optional @-prerelease@, with @+build@ metadata ignored (semver §10 — build
metadata does not affect precedence). Ordering follows semver §11: the numeric
core compares field-by-field, a prerelease ranks below the corresponding final
release, and among prerelease identifiers numeric ones rank below alphanumeric
ones (the opposite of the RubyGems\/PEP 440-local rule in
"Ecluse.Core.Version.Token").

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

import Ecluse.Core.Version.Token (maxVersionLength)

{- | A parsed semver version, wrapping the @versions@ library's
'Data.Versions.SemVer'. Its 'Ord' is the library's semver §11 precedence
(build metadata excluded), derived through the newtype.
-}
newtype SemverKey = SemverKey SemVer
    deriving stock (Show)
    deriving newtype (Eq, Ord)

{- | Parse a semver version via @versions@' 'Data.Versions.semver' (numeric
@major.minor.patch@ core, optional @-prerelease@, ignoring @+build@ metadata).
A parse failure becomes 'Nothing' — no key, so an ordering rule abstains rather
than dropping a version over a parser gap.

The raw text is bounded first, as the PEP 440 ("Ecluse.Core.Version.Pep440") and
Gem ("Ecluse.Core.Version.Gem") grammars bound it, so hostile registry metadata
cannot inflict an algorithmic-complexity DoS through an unbounded version string.
Semver carries a second hazard those grammars do not: they read numeric segments
into an unbounded 'Integer', whereas @versions@ stores 'Data.Versions.SemVer''s
numeric components in fixed-width machine words that overflow __silently__
(wrapping, never failing). A numeric run long enough to overflow would key a huge
version as a small one — corrupting 'Ecluse.Core.Version.compareVersions' and so
@dist-tags.latest@ selection — so a run too long to fit is refused here as well.
A refused version is served raw without an ordering key, exactly as any other
unparseable one.
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver raw = do
    guard (T.compareLength raw maxVersionLength /= GT)
    guard (not (hasOverlongNumericRun raw))
    SemverKey <$> rightToMaybe (V.semver raw)

{- The largest run of consecutive decimal digits guaranteed to fit the
@versions@ library's fixed-width numeric components: an 18-digit run is at most
@10^18 - 1 < 2^63@, so it fits any 64-bit signed or unsigned word and never
overflows. A run of 19+ digits might, and is refused. Real semver numbers are
tiny — node-semver itself caps a component at @2^53 - 1@ (16 digits) — so this
bound only ever rejects adversarial input. -}
maxNumericRun :: Int
maxNumericRun = 18

{- Whether @raw@ contains a maximal run of decimal digits long enough that the
@versions@ library's fixed-width numeric components could overflow on it. Total:
'T.groupBy' partitions the text into maximal same-class runs, and a digit run
longer than 'maxNumericRun' fails the bound. -}
hasOverlongNumericRun :: Text -> Bool
hasOverlongNumericRun = any overlong . T.groupBy sameClass
  where
    sameClass a b = isDigit a == isDigit b
    -- A run is homogeneous by 'sameClass', so 'T.all' 'isDigit' identifies a
    -- digit run; an over-long one fails the bound.
    overlong run = T.all isDigit run && T.compareLength run maxNumericRun == GT

{- | Whether a semver version is stable: a final release with no prerelease
component. So @1.0.0@ is stable; @1.0.0-rc.1@ and @2.0.0-beta@ are not.
-}
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey sv) = isNothing (_svPreRel sv)

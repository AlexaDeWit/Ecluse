{- | The semver grammar and ordering (npm).

Backed by the [@versions@](https://hackage.haskell.org/package/versions) library:
a 'SemverKey' wraps its 'Data.Versions.SemVer', so parsing and precedence are the
library's. A semver version is a numeric @major.minor.patch@ core followed by an
optional @-prerelease@, with @+build@ metadata ignored (semver §10 — build
metadata does not affect precedence). Ordering follows semver §11: the numeric
core compares field-by-field, a prerelease ranks below the corresponding final
release, and among prerelease identifiers numeric ones rank below alphanumeric
ones (the opposite of the RubyGems\/PEP 440-local rule in
"Ecluse.Version.Token").

A semver version is __stable__ iff it carries no prerelease.
-}
module Ecluse.Version.Semver (
    SemverKey (..),
    parseSemver,
    isSemverStable,
) where

import Data.Versions (SemVer (..))
import Data.Versions qualified as V

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
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver = fmap SemverKey . rightToMaybe . V.semver

{- | Whether a semver version is stable: a final release with no prerelease
component. So @1.0.0@ is stable; @1.0.0-rc.1@ and @2.0.0-beta@ are not.
-}
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey sv) = isNothing (_svPreRel sv)

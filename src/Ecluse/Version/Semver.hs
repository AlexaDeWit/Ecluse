{- | The semver grammar and ordering (npm).

Parses a semver version into a 'SemverKey': a numeric release core followed by an
optional prerelease, with @+build@ metadata ignored (semver §10 — build metadata
does not affect precedence). Ordering follows semver §11: the numeric core
compares field-by-field, a prerelease ranks below the corresponding final
release, and among prereleases numeric identifiers rank below alphanumeric ones
(the opposite of the RubyGems\/PEP 440-local rule in "Ecluse.Version.Token").

A semver version is __stable__ iff it carries no prerelease ('SemverFinal').
-}
module Ecluse.Version.Semver (
    SemverKey (..),
    SemverPre (..),
    SemverPreId (..),
    parseSemver,
    isSemverStable,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text qualified as T

import Ecluse.Version.Token (parseNumSeg)

{- | A semver prerelease identifier; numeric identifiers rank below alphanumeric
ones (semver §11), encoded by the constructor order.
-}
data SemverPreId = SemverNum Integer | SemverText Text
    deriving stock (Eq, Ord, Show)

{- | A semver prerelease: an actual prerelease ranks below the final release, so
'SemverPre' is ordered before 'SemverFinal'.
-}
data SemverPre = SemverPre [SemverPreId] | SemverFinal
    deriving stock (Eq, Ord, Show)

-- | A parsed semver version: numeric core, then prerelease.
data SemverKey = SemverKey [Integer] SemverPre
    deriving stock (Eq, Ord, Show)

{- | Parse a semver version (numeric core, optional @-prerelease@, ignoring
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

{- | Whether a semver version is stable: a final release with no prerelease
component. So @1.0.0@ is stable; @1.0.0-rc.1@ and @2.0.0-beta@ are not.
-}
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey _ pre) = pre == SemverFinal

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The lexical atoms and the length bound the per-ecosystem version grammars share.

A 'VToken' is a single numeric or textual run. Its ordering rule is the one the RubyGems
and PEP 440-local grammars have in common: numeric tokens outrank textual ones, numerics
compare numerically, and text compares lexically. The semver prerelease rule is the
opposite, so it lives with the semver grammar. Everything here is purely lexical.
-}
module Ecluse.Core.Version.Token (
    VToken (..),
    parseNumSeg,
    numOr0,
    isAsciiAlphaNum,
    digitRuns,
    classifyRun,
    maxVersionLength,
    withinVersionLength,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T

{- | A version token: a numeric run or a textual run. Its 'Ord' is the RubyGems
\/ PEP 440-local rule: numeric tokens outrank textual ones, numerics compare
numerically, and text compares lexically. Semver prerelease ordering is the
opposite, and "Ecluse.Core.Version.Semver" handles it.
-}
data VToken = VNum Integer | VStr Text
    deriving stock (Eq, Show)

instance Ord VToken where
    compare (VNum m) (VNum n) = compare m n
    compare (VStr s) (VStr t) = compare s t
    compare (VNum _) (VStr _) = GT
    compare (VStr _) (VNum _) = LT

{- | The maximum length of a version string the version grammars parse. It bounds the quadratic
cost of reading a digit run into an 'Integer', which hostile registry metadata could otherwise
turn into an algorithmic-complexity DoS. A version past it gets no ordering key and is served raw.
-}
maxVersionLength :: Int
maxVersionLength = 1024

{- | Whether a raw version string is inside 'maxVersionLength'. Every grammar applies it
before any numeric parsing, so no digit run is read into an 'Integer' unbounded.
-}
withinVersionLength :: Text -> Bool
withinVersionLength raw = T.compareLength raw maxVersionLength /= GT

-- | Parse a non-empty, all-digit segment as an integer.
parseNumSeg :: Text -> Maybe Integer
parseNumSeg t
    | not (T.null t) && T.all isDigit t = readMaybe (toString t)
    | otherwise = Nothing

-- | Read an all-digit (already validated) run as an integer, defaulting to 0.
numOr0 :: Text -> Integer
numOr0 t = if T.null t then 0 else fromMaybe 0 (readMaybe (toString t))

{- | An ASCII letter or ASCII digit. The PEP 440 and @Gem::Version@ grammars gate \"alphanumeric\"
with this, not the Unicode-aware 'Data.Char.isAlphaNum'. Python's @packaging@ and Ruby's
@Gem::Version@ are ASCII-only, so a Unicode gate over-accepts and mis-orders a non-ASCII digit.
-}
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

-- | Split text into its maximal digit and non-digit runs.
digitRuns :: Text -> [Text]
digitRuns = T.groupBy (\c1 c2 -> isDigit c1 == isDigit c2)

-- | Classify one run: all-digit reads as a 'VNum', anything else stays a 'VStr'.
classifyRun :: Text -> VToken
classifyRun run = if T.all isDigit run then VNum (numOr0 run) else VStr run

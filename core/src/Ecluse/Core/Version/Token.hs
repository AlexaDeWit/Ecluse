-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The low-level lexical atoms the per-ecosystem version grammars share.

A 'VToken' is a single numeric or textual run. Its ordering rule is the one the
RubyGems and PEP 440-local grammars have in common. Numeric tokens outrank textual
ones, numerics compare numerically, and text compares lexically. The semver
prerelease rule is the opposite, ranking numeric identifiers /below/ alphanumeric
ones, so it lives with the semver grammar instead.

More than one grammar calls the two segment readers, 'parseNumSeg' (validating) and
'numOr0' (total over already-validated input). Everything here is purely lexical: no
ecosystem ordering policy lives in this module.
-}
module Ecluse.Core.Version.Token (
    VToken (..),
    parseNumSeg,
    numOr0,
    isAsciiAlphaNum,
    maxVersionLength,
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

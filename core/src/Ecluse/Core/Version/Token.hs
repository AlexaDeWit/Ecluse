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

{- | The maximum length of a version string the hand-rolled numeric grammars
("Ecluse.Core.Version.Pep440" and "Ecluse.Core.Version.Gem") parse. It bounds the cost
of reading numeric segments. 'readMaybe' turns a segment into an 'Integer', which is
quadratic in the digit count. An unbounded digit run in hostile registry metadata would
therefore be an algorithmic-complexity DoS. Real version strings are tiny, and npm caps
its own version field at 256 characters, so this far larger bound rejects only
adversarial input. A version past it fails to parse, and Écluse serves it raw without
an ordering key. That is the total-by-design @mkVersion@ path in
"Ecluse.Core.Version": a version is never dropped over a parser gap.
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

{- | ASCII-only \"alphanumeric\" predicate: an ASCII letter or ASCII digit. Use it,
not the Unicode-aware 'Data.Char.isAlphaNum', wherever the PEP 440 and
@Gem::Version@ grammars gate \"alphanumeric\" characters. Python's @packaging@ and
Ruby's @Gem::Version@ are ASCII-only. A Unicode-aware gate therefore over-accepts
(fullwidth\/Arabic-Indic digits, @1.0+café@) and mis-orders: it classifies a Unicode
\"digit\" that is not an ASCII digit as text. 'Data.Char.isDigit' is already
ASCII-only, so the grammars call it directly.
-}
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

{- | Low-level lexical atoms shared by the per-ecosystem version grammars.

A 'VToken' is a single numeric or textual run, with the ordering rule common to
the RubyGems and PEP 440-local grammars: numeric tokens outrank textual ones,
numerics compare numerically, text compares lexically. (The semver prerelease
rule is the opposite — numeric identifiers rank /below/ alphanumeric ones — so it
lives with the semver grammar, not here.)

The two segment readers, 'parseNumSeg' (validating) and 'numOr0' (total over
already-validated input), are used by more than one grammar. Everything here is
purely lexical: no ecosystem ordering policy lives in this module.
-}
module Ecluse.Version.Token (
    VToken (..),
    parseNumSeg,
    numOr0,
    isAsciiAlphaNum,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T

{- | A version token: a numeric run or a textual run. Its 'Ord' is the RubyGems
\/ PEP 440-local rule — numeric tokens outrank textual ones, numerics compare
numerically, text compares lexically. (Semver prerelease ordering is the
opposite and is handled in "Ecluse.Version.Semver".)
-}
data VToken = VNum Integer | VStr Text
    deriving stock (Eq, Show)

instance Ord VToken where
    compare (VNum m) (VNum n) = compare m n
    compare (VStr s) (VStr t) = compare s t
    compare (VNum _) (VStr _) = GT
    compare (VStr _) (VNum _) = LT

-- | Parse a non-empty, all-digit segment as an integer.
parseNumSeg :: Text -> Maybe Integer
parseNumSeg t
    | not (T.null t) && T.all isDigit t = readMaybe (toString t)
    | otherwise = Nothing

-- | Read an all-digit (already validated) run as an integer, defaulting to 0.
numOr0 :: Text -> Integer
numOr0 t = if T.null t then 0 else fromMaybe 0 (readMaybe (toString t))

{- | ASCII-only \"alphanumeric\" predicate: an ASCII letter or ASCII digit. Use
this — not 'Data.Char.isAlphaNum', which is Unicode-aware — wherever the PEP 440
and @Gem::Version@ grammars gate \"alphanumeric\" characters: Python's
@packaging@ and Ruby's @Gem::Version@ are ASCII-only, so a Unicode-aware gate
both over-accepts (fullwidth\/Arabic-Indic digits, @1.0+café@) and mis-orders
(a Unicode \"digit\" that is not an ASCII digit gets classified as text).
'Data.Char.isDigit' is already ASCII-only, so it is reused directly.
-}
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

{- | The @Gem::Version@ grammar and ordering (RubyGems).

Parses a gem version into a 'GemKey': a flat list of 'VToken's obtained by
splitting on dots and then into maximal digit and letter runs, then
__canonicalised__ the way @Gem::Version#canonical_segments@ is — trailing zeros are
dropped from the numeric release and from the prerelease tail independently, so
@2.0.a@ keys as @[2, "a"]@. Ordering compares the canonical token lists
position-by-position, zero-padding the shorter side, so @1.0 == 1.0.0@,
@2.0.a == 2.a@, @2.t > 2.0.a@, and a trailing letter (prerelease) segment sorts
below the bare release (a 'VStr' ranks below 'VNum 0'; see "Ecluse.Core.Version.Token").

A gem version is __stable__ iff every token is numeric — no letter segment, i.e.
no prerelease marker such as @.pre@ or @.rc1@.
-}
module Ecluse.Core.Version.Gem (
    GemKey (..),
    parseGem,
    compareGemTokens,
    isGemStable,
) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd)
import Data.Text qualified as T

import Ecluse.Core.Version.Token (VToken (..), isAsciiAlphaNum, maxVersionLength, numOr0)

{- | A parsed @Gem::Version@: a flat token list compared with zero-padding, with
numeric tokens outranking textual ones (see 'VToken').
-}
newtype GemKey = GemKey [VToken]
    deriving stock (Eq, Show)

instance Ord GemKey where
    compare (GemKey a) (GemKey b) = compareGemTokens a b

-- | Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

{- | Parse a @Gem::Version@: dot-separated alphanumeric segments, each split into
maximal digit and letter runs. Hyphens are first rewritten to a prerelease marker
(a global @gsub("-", ".pre.")@, the way @Gem::Version@ canonicalises), so
@1.0.0-1@ parses as @1.0.0.pre.1@ and orders below @1.0.0@. Fails on empty or
non-alphanumeric segments.
-}
parseGem :: Text -> Maybe GemKey
parseGem raw = do
    -- Bound the input length before any numeric parsing: a digit run is read into an
    -- 'Integer' with 'readMaybe', which is quadratic in the digit count, so an
    -- unbounded run in hostile metadata would be an algorithmic-complexity DoS.
    guard (T.compareLength raw maxVersionLength /= GT)
    let stripped = T.strip raw
        -- Gem::Version canonicalises hyphens to a prerelease marker via a global
        -- gsub("-", ".pre.") before segmenting, so e.g. "1.0.0-1" parses as the
        -- prerelease "1.0.0.pre.1" and orders below "1.0.0". Non-hyphenated input
        -- is untouched, so existing orderings are unchanged.
        segs = T.splitOn "." (T.replace "-" ".pre." stripped)
    guard (not (T.null stripped))
    guard (all validSeg segs)
    let toks = concatMap segTokens segs
    guard (not (null toks))
    pure (GemKey (canonicalSegments toks))
  where
    validSeg s = not (T.null s) && T.all isAsciiAlphaNum s
    segTokens = map classify . T.groupBy (\c1 c2 -> isDigit c1 == isDigit c2)
    classify g = if T.all isDigit g then VNum (numOr0 g) else VStr g

{- Canonicalise a gem token list the way @Gem::Version#canonical_segments@ does:
split it at the first textual (prerelease) token into a numeric release and a
prerelease tail, drop trailing zeros from /each/ part, then rejoin. So @2.0.a@ keys
as @[2, "a"]@ (the release's trailing zero is dropped before the prerelease) — which
is why @2.t > 2.0.a@ and @2.0.a == 2.a@. Comparing the un-canonicalised flat lists
would instead reach a numeric-vs-textual position and order them the other way.
-}
canonicalSegments :: [VToken] -> [VToken]
canonicalSegments toks =
    let (release, prerelease) = break isText toks
     in dropTrailingZeros release <> dropTrailingZeros prerelease
  where
    isText = \case
        VStr _ -> True
        VNum _ -> False
    dropTrailingZeros = dropWhileEnd (== VNum 0)

{- | Whether a gem version is stable: every token is numeric (no letter segment).
So @1.0.0@ is stable; @1.0.0.pre@ and @1.2.0.rc1@ are not.
-}
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumToken toks
  where
    isNumToken = \case
        VNum _ -> True
        VStr _ -> False

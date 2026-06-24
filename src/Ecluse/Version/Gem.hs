{- | The @Gem::Version@ grammar and ordering (RubyGems).

Parses a gem version into a 'GemKey': a flat list of 'VToken's obtained by
splitting on dots and then into maximal digit and letter runs. Ordering compares
the token lists position-by-position, zero-padding the shorter side, so
@1.0 == 1.0.0@ and a trailing letter (prerelease) segment sorts below the bare
release (a 'VStr' ranks below 'VNum 0'; see "Ecluse.Version.Token").

A gem version is __stable__ iff every token is numeric — no letter segment, i.e.
no prerelease marker such as @.pre@ or @.rc1@.
-}
module Ecluse.Version.Gem (
    GemKey (..),
    parseGem,
    compareGemTokens,
    isGemStable,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text qualified as T

import Ecluse.Version.Token (VToken (..), numOr0)

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
maximal digit and letter runs. Fails on empty or non-alphanumeric segments.
-}
parseGem :: Text -> Maybe GemKey
parseGem raw = do
    let trimmed = T.strip raw
        segs = T.splitOn "." trimmed
    guard (not (T.null trimmed))
    guard (all validSeg segs)
    let toks = concatMap segTokens segs
    guard (not (null toks))
    pure (GemKey toks)
  where
    validSeg s = not (T.null s) && T.all isAlphaNum s
    segTokens = map classify . T.groupBy (\c1 c2 -> isDigit c1 == isDigit c2)
    classify g = if T.all isDigit g then VNum (numOr0 g) else VStr g

{- | Whether a gem version is stable: every token is numeric (no letter segment).
So @1.0.0@ is stable; @1.0.0.pre@ and @1.2.0.rc1@ are not.
-}
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumToken toks
  where
    isNumToken = \case
        VNum _ -> True
        VStr _ -> False

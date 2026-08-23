-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @Gem::Version@ grammar and ordering (RubyGems).

'parseGem' reads a gem version into a 'GemKey': a flat list of 'VToken's, split on
dots and then into maximal digit and letter runs. It then __canonicalises__ that list
the way @Gem::Version#canonical_segments@ does. It drops trailing zeros from the numeric
release and from the prerelease tail independently, so @2.0.a@ keys as @[2, "a"]@.

Ordering compares the canonical token lists position-by-position and zero-pads the
shorter side, so @1.0 == 1.0.0@, @2.0.a == 2.a@, and @2.t > 2.0.a@. A trailing letter
(prerelease) segment sorts below the bare release, because a 'VStr' ranks below
'VNum 0' (see "Ecluse.Core.Version.Token").

A gem version is __stable__ iff every token is numeric: no letter segment, so no
prerelease marker such as @.pre@ or @.rc1@.
-}
module Ecluse.Core.Version.Gem (
    GemKey (..),
    parseGem,
    compareGemTokens,
    isGemStable,
) where

import Data.List (dropWhileEnd)
import Data.Text qualified as T

import Ecluse.Core.Version.Token (VToken (..), classifyRun, digitRuns, isAsciiAlphaNum, withinVersionLength)

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

{- | Parse a @Gem::Version@ into its ordering key. Fails on an empty or non-alphanumeric
segment.
-}
parseGem :: Text -> Maybe GemKey
parseGem raw = do
    guard (withinVersionLength raw)
    let stripped = T.strip raw
        -- Gem::Version canonicalises hyphens via a global gsub("-", ".pre.") before segmenting,
        -- so "1.0.0-1" is the prerelease "1.0.0.pre.1" and orders below "1.0.0".
        segs = T.splitOn "." (T.replace "-" ".pre." stripped)
    guard (not (T.null stripped))
    guard (all validSeg segs)
    let toks = concatMap segTokens segs
    guard (not (null toks))
    pure (GemKey (canonicalSegments toks))
  where
    validSeg s = not (T.null s) && T.all isAsciiAlphaNum s
    segTokens = map classifyRun . digitRuns

{- Mirror @Gem::Version#canonical_segments@: drop trailing zeros from the numeric release
and from the prerelease tail separately. So @2.0.a@ keys as @[2, "a"]@, which is why
@2.t > 2.0.a@ and @2.0.a == 2.a@. Comparing the flat, un-canonicalised lists orders them
the other way.
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
So @1.0.0@ is stable. @1.0.0.pre@ and @1.2.0.rc1@ are not.
-}
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumToken toks
  where
    isNumToken = \case
        VNum _ -> True
        VStr _ -> False

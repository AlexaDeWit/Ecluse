-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @Gem::Version@ grammar and ordering (RubyGems).

'parseGem' keys a version as a flat 'VToken' list canonicalised the way
@Gem::Version#canonical_segments@ is, so @2.0.a@ keys as @[2, "a"]@. Comparison zero-pads the
shorter side, and a 'VStr' ranks below @VNum 0@ (see "Ecluse.Core.Version.Token"), so a
trailing letter segment sorts below the bare release.
-}
module Ecluse.Core.Version.Gem (
    GemKey (..),
    parseGem,
    isGemStable,
) where

import Data.List (dropWhileEnd)
import Data.Text qualified as T

import Ecluse.Core.Version.Token (VToken (..), classifyRun, digitRuns, isAsciiAlphaNum, withinVersionLength)

-- | A parsed @Gem::Version@: a flat token list compared with zero-padding.
newtype GemKey = GemKey [VToken]
    deriving stock (Eq, Show)

instance Ord GemKey where
    compare (GemKey a) (GemKey b) = compareGemTokens a b

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

-- | Whether a gem version is stable: every token is numeric, so no prerelease marker.
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumeric toks

-- Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

{- Mirror @Gem::Version#canonical_segments@: trailing zeros drop from the numeric release and
from the prerelease tail separately, which is why @2.t > 2.0.a@ and @2.0.a == 2.a@. -}
canonicalSegments :: [VToken] -> [VToken]
canonicalSegments toks =
    let (release, prerelease) = break (not . isNumeric) toks
     in dropTrailingZeros release <> dropTrailingZeros prerelease
  where
    dropTrailingZeros = dropWhileEnd (== VNum 0)

isNumeric :: VToken -> Bool
isNumeric = \case
    VNum _ -> True
    VStr _ -> False

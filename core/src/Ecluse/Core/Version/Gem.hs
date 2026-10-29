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
    renderGem,
    isGemStable,
) where

import Data.List (dropWhileEnd)
import Data.Text qualified as T

import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version.Token (VToken (..), classifyRun, digitRuns, renderToken, withinVersionLength)

-- | A parsed @Gem::Version@: a flat token list compared with zero-padding.
newtype GemKey = GemKey [VToken]
    deriving stock (Eq, Show)

instance Ord GemKey where
    compare (GemKey a) (GemKey b) = compareGemTokens a b

-- | Parse and settle canonical tokens, refusing empty or non-alphanumeric segments.
parseGem :: Text -> Maybe GemKey
parseGem raw = do
    guard (withinVersionLength raw)
    let stripped = T.strip raw
        -- Gem::Version canonicalises hyphens via a global gsub("-", ".pre.") before segmenting,
        -- so "1.0.0-1" is the prerelease "1.0.0.pre.1" and orders below "1.0.0".
        segs = T.splitOn "." (T.replace "-" ".pre." stripped)
    guard (not (T.null stripped))
    guard (all validSeg segs)
    let segments = map segTokens segs
    guard (not (all null segments))
    pure (GemKey (strictElements (canonicalSegments segments)))
  where
    validSeg s = not (T.null s) && T.all isAsciiAlphaNum s
    segTokens = map classifyRun . digitRuns

-- | Render canonical Gem tokens, using @0@ for an all-zero release.
renderGem :: GemKey -> Text
renderGem (GemKey []) = "0"
renderGem (GemKey tokens) = T.intercalate "." (map renderToken tokens)

-- | Whether a gem version is stable: every token is numeric, so no prerelease marker.
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumeric toks

-- Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

-- Gem::Version#canonical_segments: drop the trailing zeros and, for a prerelease, the first run
-- of zeros that starts a segment and directly precedes a letter.
canonicalSegments :: [[VToken]] -> [VToken]
canonicalSegments segments = dropWhileEnd (== VNum 0) (if all isNumeric tokens then tokens else dropFirstZeroRun starts)
  where
    tokens = concat segments
    starts = concatMap (zip (True : repeat False)) segments
    dropFirstZeroRun = \case
        held@((True, VNum 0) : _) | (_, rest@((_, VStr _) : _)) <- span ((== VNum 0) . snd) held -> map snd rest
        (_, token) : rest -> token : dropFirstZeroRun rest
        [] -> []

isNumeric :: VToken -> Bool
isNumeric = \case
    VNum _ -> True
    VStr _ -> False

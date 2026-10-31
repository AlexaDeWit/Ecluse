-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @Gem::Version@ grammar and ordering (RubyGems).

'parseGem' keys a version as a flat 'VToken' list canonicalised the way
@Gem::Version#canonical_segments@ is, so @2.0.a@ keys as @[2, "a"]@ and @1.a.0.b@ as
@[1, "a", "b"]@. Comparison zero-pads the shorter side, and a 'VStr' ranks below @VNum 0@ (see
"Ecluse.Core.Version.Token"), so a trailing letter segment sorts below the bare release.
-}
module Ecluse.Core.Version.Gem (
    GemKey (..),
    parseGem,
    renderGem,
    isGemStable,
    isGemRequirementOperator,
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
    pure (GemKey (strictElements (canonicalSegments (map segTokens segs))))
  where
    validSeg s = not (T.null s) && T.all isAsciiAlphaNum s
    segTokens = map classifyRun . digitRuns

{- | Render canonical Gem tokens, using @0@ for an all-zero release. A key that keeps a zero before a
letter spells its first letter after a zero, which 'parseGem' drops again.
-}
renderGem :: GemKey -> Text
renderGem (GemKey []) = "0"
renderGem (GemKey tokens) = T.intercalate "." (map renderToken (if zeroBeforeLetter tokens then release <> (VNum 0 : prerelease) else tokens))
  where
    (release, prerelease) = span isNumeric tokens

-- | Whether a gem version is stable: every token is numeric, so no prerelease marker.
isGemStable :: GemKey -> Bool
isGemStable (GemKey toks) = all isNumeric toks

{- | Whether a @Gem::Requirement@ clause may use this comparison operator. INLINE keeps the
literals at each call site, because a shared operator list allocates on every parsed clause.
-}
isGemRequirementOperator :: Text -> Bool
isGemRequirementOperator operator = operator `elem` ["=", "!=", ">", "<", ">=", "<=", "~>"]
{-# INLINE isGemRequirementOperator #-}

-- Compare gem token lists, zero-padding the shorter side.
compareGemTokens :: [VToken] -> [VToken] -> Ordering
compareGemTokens [] [] = EQ
compareGemTokens (x : xs) (y : ys) = compare x y <> compareGemTokens xs ys
compareGemTokens (x : xs) [] = compare x (VNum 0) <> compareGemTokens xs []
compareGemTokens [] (y : ys) = compare (VNum 0) y <> compareGemTokens [] ys

{- Gem::Version#canonical_segments drops the trailing zeros, and the first run of zeros that starts
a segment and directly precedes a letter, wherever that run falls. -}
canonicalSegments :: [[VToken]] -> [VToken]
canonicalSegments = dropWhileEnd (== VNum 0) . go 0
  where
    -- The count is the zero segments of the run so far, kept unless a letter follows them.
    go :: Int -> [[VToken]] -> [VToken]
    go zeros = \case
        (VNum 0 : letters@(VStr _ : _)) : rest -> letters <> concat rest
        [VNum 0] : rest -> go (zeros + 1) rest
        rest@((VStr _ : _) : _) | zeros > 0 -> concat rest
        segment : rest -> replicate zeros (VNum 0) <> segment <> go 0 rest
        [] -> replicate zeros (VNum 0)

-- Whether a zero directly precedes a letter, which a plain dotted spelling would let parsing drop.
zeroBeforeLetter :: [VToken] -> Bool
zeroBeforeLetter tokens = or (zipWith (\token next -> token == VNum 0 && not (isNumeric next)) tokens (drop 1 tokens))

isNumeric :: VToken -> Bool
isNumeric = \case
    VNum _ -> True
    VStr _ -> False

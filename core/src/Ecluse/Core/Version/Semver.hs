-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The semver grammar and ordering (npm).

'SemverKey' wraps the [@versions@](https://hackage.haskell.org/package/versions) library's
'Data.Versions.SemVer', so precedence is the library's: semver 11 ordering, with @+build@
metadata excluded from it. 'parseSemver' scans the grammar itself. Within the length and
digit-run bounds it accepts and builds what the library's parser does. Among prerelease
identifiers numeric ones rank below alphanumeric ones, the opposite of the rule in
"Ecluse.Core.Version.Token". A semver version is stable iff it carries no prerelease.
-}
module Ecluse.Core.Version.Semver (
    SemverKey (..),
    parseSemver,
    isSemverStable,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Versions (Chunk (..), Release (..), SemVer (..))

import Ecluse.Core.Version.Token (withinVersionLength)

-- | A parsed semver version, ordered by the @versions@ library's semver 11 precedence.
newtype SemverKey = SemverKey SemVer
    deriving stock (Show)
    deriving newtype (Eq, Ord)

{- | Parse a semver version, or 'Nothing' so an ordering rule abstains rather than drops it.
The digit-run bound refuses a number that would overflow the key's fixed-width components.
-}
parseSemver :: Text -> Maybe SemverKey
parseSemver raw = do
    guard (withinVersionLength raw)
    guard (not (hasOverlongNumericRun raw))
    -- Evaluated in full, so a retained key holds no thunk over the scan.
    SemverKey . force <$> scanSemver raw

-- | Whether a semver version is stable: a final release with no prerelease component.
isSemverStable :: SemverKey -> Bool
isSemverStable (SemverKey sv) = isNothing (_svPreRel sv)

{- The longest digit run guaranteed to fit the @versions@ library's fixed-width numeric
components: 18 digits is at most @10^18 - 1 < 2^63@, and a longer run might overflow silently. -}
maxNumericRun :: Int
maxNumericRun = 18

hasOverlongNumericRun :: Text -> Bool
hasOverlongNumericRun raw = T.foldl' runLength 0 raw > maxNumericRun
  where
    -- The length of the digit run that ends at this character, held once it passes the bound.
    runLength run c
        | run > maxNumericRun = run
        | isDigit c = run + 1
        | otherwise = 0

-- The grammar of the library's @semver@ parser, in that parser's order. @SemverSpec@ checks the
-- two against each other.
scanSemver :: Text -> Maybe SemVer
scanSemver raw = do
    (major, afterMajor) <- leadingNumber raw
    (minor, afterMinor) <- leadingNumber =<< afterChar '.' afterMajor
    (patch, afterPatch) <- leadingNumber =<< afterChar '.' afterMinor
    (preRel, afterPreRel) <- markedPart '-' identifiers afterPatch
    (meta, afterMeta) <- markedPart '+' buildMetadata afterPreRel
    guard (T.null afterMeta)
    pure (SemVer major minor patch (Release <$> preRel) meta)

afterChar :: Char -> Text -> Maybe Text
afterChar c t = case T.uncons t of
    Just (lead, rest) | lead == c -> Just rest
    _ -> Nothing

-- Inlined, so the three numbers of a version pass no boxed pair between the steps.
leadingNumber :: Text -> Maybe (Word, Text)
leadingNumber t = do
    let (digits, rest) = T.span isDigit t
    guard (spellsNumber digits)
    pure (digitsValue digits, rest)
{-# INLINE leadingNumber #-}

-- Expects digits only. Semver allows a leading zero in @0@ alone.
spellsNumber :: Text -> Bool
spellsNumber digits = case T.uncons digits of
    Nothing -> False
    Just (lead, more) -> lead /= '0' || T.null more

-- Expects digits only. The digit-run bound keeps the value within a 'Word'.
digitsValue :: Text -> Word
digitsValue = T.foldl' (\value digit -> value * 10 + fromIntegral (ord digit - ord '0')) 0

markedPart :: Char -> (Text -> Maybe (a, Text)) -> Text -> Maybe (Maybe a, Text)
markedPart marker part t = case afterChar marker t of
    Nothing -> Just (Nothing, t)
    Just marked -> first Just <$> part marked

identifiers :: Text -> Maybe (NonEmpty Chunk, Text)
identifiers t = do
    let (piece, rest) = T.span isIdentifierChar t
    chunk <- identifier piece
    case afterChar '.' rest of
        Nothing -> Just (chunk :| [], rest)
        Just more -> first (NE.cons chunk) <$> identifiers more

identifier :: Text -> Maybe Chunk
identifier piece
    | T.any (\c -> isAlpha c || c == '-') piece = Just (Alphanum piece)
    | T.all isDigit piece && spellsNumber piece = Just (Numeric (digitsValue piece))
    | otherwise = Nothing

buildMetadata :: Text -> Maybe (Text, Text)
buildMetadata t = do
    let (body, rest) = T.span (\c -> isIdentifierChar c || c == '.') t
    guard (not (any T.null (T.split (== '.') body)))
    pure (body, rest)

-- 'isAlphaNum' and 'isAlpha' are the library's own Unicode-aware classes, so a non-ASCII letter
-- or numeral is an identifier character.
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '-'

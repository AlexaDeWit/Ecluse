-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Small pure text helpers shared across the codebase, so the blank-value,
URL-path-join, and last-path-segment idioms have a single definition rather than
several near-identical re-spellings. It also holds the hot-path ISO-8601 instant
renderer the serve path uses ('renderIso8601Utc'). This module depends on nothing else
in @Ecluse@, so any module may import it without risking an import cycle.
-}
module Ecluse.Core.Text (
    nonBlank,
    stripTrailingSlash,
    joinUrlPath,
    lastPathSegment,
    renderIso8601Utc,
    displayExceptionT,
) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Time (UTCTime (UTCTime), diffTimeToPicoseconds, toGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)

{- | The text trimmed of surrounding whitespace, or 'Nothing' when nothing remains.
An empty or all-whitespace value therefore counts as absent.
-}
nonBlank :: Text -> Maybe Text
nonBlank t =
    let trimmed = T.strip t
     in if T.null trimmed then Nothing else Just trimmed

-- | Drop a single trailing slash from a URL base when present.
stripTrailingSlash :: Text -> Text
stripTrailingSlash b = fromMaybe b (T.stripSuffix "/" b)

{- | Join a URL base and an already-encoded path with exactly one slash, tolerating one
trailing slash on the base. It appends the path verbatim, and neither encodes nor validates it.
-}
joinUrlPath :: Text -> Text -> Text
joinUrlPath b path = stripTrailingSlash b <> "/" <> path

{- | The text after the final slash, or the whole string when it carries none.
'Nothing' when that segment is empty, so a caller supplies its own fallback.
-}
lastPathSegment :: Text -> Maybe Text
lastPathSegment url =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then Nothing else Just afterLastSlash

{- | Render a 'UTCTime' byte for byte as 'iso8601Show' does, at a fraction of the
allocation cost. The packument serve path renders one instant per surviving version per
request, so this sits on a hot loop.

Years 0-9999 with a time-of-day below 86 400 seconds take the builder path. Anything
else delegates to 'iso8601Show', so parity is total.
-}
renderIso8601Utc :: UTCTime -> Text
renderIso8601Utc t@(UTCTime day dt)
    | year < 0 || year > 9999 || picos >= 86_400_000_000_000_000 = toText (iso8601Show t)
    | otherwise =
        TL.toStrict . TB.toLazyText $
            digits 4 year
                <> "-"
                <> digits 2 (fromIntegral month)
                <> "-"
                <> digits 2 (fromIntegral dayOfMonth)
                <> "T"
                <> digits 2 hh
                <> ":"
                <> digits 2 mm
                <> ":"
                <> digits 2 ss
                <> fraction
                <> "Z"
  where
    (year, month, dayOfMonth) = toGregorian day
    picos = diffTimeToPicoseconds dt
    (secondsOfDay, frac) = picos `divMod` 1_000_000_000_000
    (hh, rem') = secondsOfDay `divMod` 3600
    (mm, ss) = rem' `divMod` 60

    -- A non-negative integer, zero-padded to at least the given width (the
    -- inputs here never exceed it).
    digits :: Int -> Integer -> TB.Builder
    digits width n =
        let body = show n :: String
            pad = width - length body
         in TB.fromString (replicate pad '0') <> TBI.decimal n

    -- The fractional second as @iso8601Show@ renders it: nothing when zero,
    -- else a dot and the 12 picosecond digits with trailing zeros trimmed.
    fraction :: TB.Builder
    fraction
        | frac == 0 = mempty
        | otherwise =
            TB.fromText ("." <> T.dropWhileEnd (== '0') (T.justifyRight 12 '0' (show frac)))

-- | Render an exception as 'Text' for a log line or error value.
displayExceptionT :: (Exception e) => e -> Text
displayExceptionT = toText . displayException

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Small pure text helpers shared across the codebase, so the blank-value,
URL-path-join, and last-path-segment idioms have a single definition rather than
several near-identical re-spellings -- plus the hot-path ISO-8601 instant renderer
the serve path uses ('renderIso8601Utc'). This module depends on nothing else in
@Ecluse@, so any module may import it without risking an import cycle.
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
A value that is empty or all-whitespace is treated as absent -- the idiom an
environment lookup or an optional configuration field wants for "present but blank
means unset". The surviving text is returned __trimmed__, so a caller never has to
strip it a second time.
-}
nonBlank :: Text -> Maybe Text
nonBlank t =
    let trimmed = T.strip t
     in if T.null trimmed then Nothing else Just trimmed

{- | Drop a single trailing @\'\/\'@ from a URL base when present, leaving any other
base untouched. At most one slash is removed, and the function is idempotent on a
base that already carries none.
-}
stripTrailingSlash :: Text -> Text
stripTrailingSlash b = fromMaybe b (T.stripSuffix "/" b)

{- | Join a URL base and an already-encoded path with exactly one @\'\/\'@, tolerating
one trailing slash on the base so the join never doubles it. The path is appended
verbatim -- this neither encodes nor validates it.
-}
joinUrlPath :: Text -> Text -> Text
joinUrlPath b path = stripTrailingSlash b <> "/" <> path

{- | The last path segment of a slash-separated string: the text after the final
@\'\/\'@, or the whole string when it carries none. 'Nothing' when that segment is
empty (the string ends in a slash), so a caller supplies its own fallback rather
than forming a segmentless path. This neither decodes nor validates the segment.
-}
lastPathSegment :: Text -> Maybe Text
lastPathSegment url =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then Nothing else Just afterLastSlash

{- | Render a 'UTCTime' as the ISO-8601 instant 'iso8601Show' produces,
__byte-for-byte__, at a fraction of the allocation cost: a handful of digit
writes into a builder instead of the general @time@ formatting machinery. The
packument serve path re-renders one instant per surviving version per request
(the served @time@ map is rebuilt from the merge plan's normalised instants), so
the formatter sits on a hot loop where the general machinery's cost is paid
thousands of times per request.

The fast path covers the whole real-world domain: years 0-9999 and a
time-of-day below 86 400 s. An input outside it (an expanded-representation
year, a leap-second reading) __delegates to 'iso8601Show' itself__, so parity is
total by construction and property-tested byte-for-byte in @TextSpec@. The
fractional second renders exactly as @iso8601Show@ does: omitted when zero, else
the picosecond digits with trailing zeros trimmed.
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

{- | Render an exception as 'Text' for a log line or error value. relude's
'displayException' is over 'String'; this is the 'Text' form the log and error sites
want, defined once rather than re-spelled at each call site.
-}
displayExceptionT :: (Exception e) => e -> Text
displayExceptionT = toText . displayException

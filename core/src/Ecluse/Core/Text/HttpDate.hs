-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | HTTP timestamps for provenance and response retry hints.
module Ecluse.Core.Text.HttpDate (
    parseHttpDate,
    parseHttpDateAt,
) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), addGregorianYearsClip, defaultTimeLocale, formatTime, parseTimeM, toGregorian)

import Ecluse.Core.Text (readDecimalText)

-- | Read the provenance date format, retaining its timezone and whitespace acceptance.
parseHttpDate :: Text -> Maybe UTCTime
parseHttpDate raw = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (toString (T.strip raw))

-- | Read all RFC 9110 HTTP-date forms, resolving two-digit years against the supplied current time.
parseHttpDateAt :: UTCTime -> Text -> Maybe UTCTime
parseHttpDateAt now raw = do
    guard (T.length raw <= 33)
    modern <|> obsolete <|> asctime
  where
    modern = do
        guard (T.length raw == 29)
        parseHttpDate raw >>= exact "%a, %d %b %Y %H:%M:%S GMT" raw
    asctime = do
        guard (T.length raw == 24)
        stamp <- parseDate "%a %b %e %H:%M:%S %Y" raw
        exact "%a %b %e %H:%M:%S %Y" raw stamp <|> exact "%a %b %d %H:%M:%S %Y" raw stamp
    obsolete = do
        [weekday, date, clock, "GMT"] <- pure (T.words raw)
        [day, month, shortYear] <- pure (T.splitOn "-" date)
        guard (T.length shortYear == 2)
        year <- readDecimalText shortYear
        let latestYear = limitYear - (limitYear - year) `mod` 100
            dated fullYear = T.unwords [weekday, day <> "-" <> month <> "-" <> show fullYear, clock, "GMT"]
            candidates = mapMaybe (parseDate "%A, %d-%b-%Y %H:%M:%S GMT" . dated) [latestYear, latestYear - 100]
        stamp <- find (<= futureLimit) candidates
        exact "%A, %d-%b-%y %H:%M:%S GMT" raw stamp
    futureLimit = now{utctDay = addGregorianYearsClip 50 (utctDay now)}
    (limitYear, _, _) = toGregorian (utctDay futureLimit)

parseDate :: String -> Text -> Maybe UTCTime
parseDate format = parseTimeM False defaultTimeLocale format . toString

exact :: String -> Text -> UTCTime -> Maybe UTCTime
exact format raw stamp = stamp <$ guard (toText (formatTime defaultTimeLocale format stamp) == raw)

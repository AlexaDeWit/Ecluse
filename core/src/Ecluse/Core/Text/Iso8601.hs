-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | ISO 8601 UTC time stamps, read and rendered without the library's parser or a text builder.
A registry document holds one for each release or file, and an npm listing renders one for each release.
The read takes one layout and declines every other, and the render leaves what it does not write to 'iso8601Show'.
-}
module Ecluse.Core.Text.Iso8601 (
    readIso8601Utc,
    renderIso8601Utc,
) where

import Control.Monad.ST (ST)
import Data.Char (isDigit)
import Data.Primitive.ByteArray (MutableByteArray, createByteArray, setByteArray, writeByteArray)
import Data.Text qualified as T
import Data.Text.Internal qualified as TI
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime), diffTimeToPicoseconds, fromGregorian, picosecondsToDiffTime, toModifiedJulianDay)
import Data.Time.Format.ISO8601 (iso8601Show)

import Ecluse.Core.Text (writeDigits)

{- | The instant of a stamp written @YYYY-MM-DDTHH:MM:SS@, an optional fraction of 1 to 12 digits, then @Z@.
'Nothing' for every other text, which includes stamps that a wider parser reads.
-}
readIso8601Utc :: Text -> Maybe UTCTime
readIso8601Utc raw@(TI.Text _ _ bytes)
    | bytes > longestStamp = Nothing
    | otherwise = case T.foldl' scanStamp (Scan 0 0 0 100_000_000_000) raw of
        Scan at digits fraction _ | at == scanFinished -> stampInstant digits fraction
        _ -> Nothing

-- The bytes of a stamp with a fraction of twelve digits. A longer text is refused unread.
longestStamp :: Int
longestStamp = 33

{- A scan's place in a stamp: the offset of the next character, the date and time digits read so far as
one number, the fraction in picoseconds, and the picoseconds one unit of the next fraction digit is worth. -}
data Scan = Scan Int Int Int Int

-- Offsets no character sits at: the zone has ended the stamp, and a character has broken the layout.
scanFinished, scanRefused :: Int
scanFinished = -1
scanRefused = -2

-- Inlined into the fold, so the scan passes between characters without a heap object.
scanStamp :: Scan -> Char -> Scan
scanStamp (Scan at digits fraction worth) c
    | at < 0 = refused
    | at == 4 || at == 7 = punctuation '-'
    | at == 10 = punctuation 'T'
    | at == 13 || at == 16 = punctuation ':'
    | at < 19 = if isDigit c then Scan (at + 1) (digits * 10 + digitValue) fraction worth else refused
    | c == 'Z' && at /= 20 = Scan scanFinished digits fraction worth
    | at == 19 = punctuation '.'
    | isDigit c && worth > 0 = Scan (at + 1) digits (fraction + worth * digitValue) (worth `quot` 10)
    | otherwise = refused
  where
    refused = Scan scanRefused 0 0 0
    punctuation expected = if c == expected then Scan (at + 1) digits fraction worth else refused
    digitValue = ord c - ord '0'
{-# INLINE scanStamp #-}

-- The instant of fourteen date and time digits and a fraction, when they name a date and a time of day below 24 h.
stampInstant :: Int -> Int -> Maybe UTCTime
stampInstant digits fraction
    | hour > 23 || minute > 59 || secondOfMinute > 59 = Nothing
    | otherwise = do
        day <- gregorianDay year month dayOfMonth
        let !time = picosecondsToDiffTime (toInteger (((hour * 60 + minute) * 60 + secondOfMinute) * 1_000_000_000_000 + fraction))
        pure (UTCTime day time)
  where
    (date, clock) = digits `quotRem` 1_000_000
    (yearMonth, dayOfMonth) = date `quotRem` 100
    (year, month) = yearMonth `quotRem` 100
    (hourMinute, secondOfMinute) = clock `quotRem` 100
    (hour, minute) = hourMinute `quotRem` 100

{- The day of a date in years 0 to 9999, by the arithmetic of the time library's 'Data.Time.fromGregorianValid',
or 'Nothing' for a month or a day that the calendar lacks. -}
gregorianDay :: Int -> Int -> Int -> Maybe Day
gregorianDay !year !month !dayOfMonth
    | month < 1 || month > 12 || dayOfMonth < 1 || dayOfMonth > monthLength = Nothing
    | otherwise = Just $! ModifiedJulianDay (toInteger (dayOfYear + 365 * before + before `div` 4 - before `div` 100 + before `div` 400 - 678_576))
  where
    leap = year `rem` 4 == 0 && (year `rem` 100 /= 0 || year `rem` 400 == 0)
    monthLength
        | month == 2 = if leap then 29 else 28
        | month == 4 || month == 6 || month == 9 || month == 11 = 30
        | otherwise = 31
    before = year - 1
    dayOfYear = (367 * month - 362) `div` 12 + dayOfMonth - (if month <= 2 then 0 else if leap then 1 else 2)

{- | The text of 'iso8601Show', written to one buffer for years 0 to 9999 and a time of day below 86 400 s.
In those years a negative time of day keeps a signed hour, where 'iso8601Show' borrows from the day.
-}
renderIso8601Utc :: UTCTime -> Text
renderIso8601Utc t@(UTCTime day dt)
    | mjd < firstDayOfYear0 || mjd > lastDayOfYear9999 || picos >= picosPerDay = toText (iso8601Show t)
    | picos < 0 = T.take 11 pastHourStamp <> show hour <> T.drop 13 pastHourStamp
    | otherwise = stampText (fromInteger mjd) (fromInteger picos)
  where
    mjd = toModifiedJulianDay day
    picos = diffTimeToPicoseconds dt
    (hour, pastHour) = picos `divMod` picosPerHour
    -- The stamp of the time past the hour: every field of the result but the hour.
    pastHourStamp = stampText (fromInteger mjd) (fromInteger pastHour)

firstDayOfYear0, lastDayOfYear9999 :: Integer
firstDayOfYear0 = toModifiedJulianDay (fromGregorian 0 1 1)
lastDayOfYear9999 = toModifiedJulianDay (fromGregorian 9999 12 31)

picosPerDay, picosPerHour :: Integer
picosPerDay = 24 * picosPerHour
picosPerHour = 3_600_000_000_000_000

{- The stamp of a day number in years 0 to 9999 and the picoseconds of a time of day from zero
below 86 400 s. The date is Howard Hinnant's @civil_from_days@, over days since 0000-03-01. -}
stampText :: Int -> Int -> Text
stampText !mjd !picos = TI.text (createByteArray len fill) 0 len
  where
    sinceMarch = mjd + 678_881
    era = sinceMarch `div` 146_097
    dayOfEra = sinceMarch `mod` 146_097
    yearOfEra = (dayOfEra - dayOfEra `quot` 1460 + dayOfEra `quot` 36_524 - dayOfEra `quot` 146_096) `quot` 365
    dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra `quot` 4 - yearOfEra `quot` 100)
    marchMonth = (5 * dayOfYear + 2) `quot` 153
    dayOfMonth = dayOfYear - (153 * marchMonth + 2) `quot` 5 + 1
    month = if marchMonth < 10 then marchMonth + 3 else marchMonth - 9
    year = yearOfEra + era * 400 + (if month <= 2 then 1 else 0)

    (secondOfDay, fraction) = picos `quotRem` 1_000_000_000_000
    (minuteOfDay, secondOfMinute) = secondOfDay `quotRem` 60
    (hourOfDay, minuteOfHour) = minuteOfDay `quotRem` 60
    !(fractionDigits, significant) = withoutTrailingZeros 12 fraction
    len = if fraction == 0 then 20 else 21 + fractionDigits

    -- Every digit starts as a zero, so a field written from its last digit backwards is padded.
    fill :: MutableByteArray st -> ST st ()
    fill target = do
        setByteArray target 0 len (ascii '0')
        writeDigits target 3 (fromIntegral year)
        writeByteArray target 4 (ascii '-')
        writeDigits target 6 (fromIntegral month)
        writeByteArray target 7 (ascii '-')
        writeDigits target 9 (fromIntegral dayOfMonth)
        writeByteArray target 10 (ascii 'T')
        writeDigits target 12 (fromIntegral hourOfDay)
        writeByteArray target 13 (ascii ':')
        writeDigits target 15 (fromIntegral minuteOfHour)
        writeByteArray target 16 (ascii ':')
        writeDigits target 18 (fromIntegral secondOfMinute)
        when (fraction /= 0) $ do
            writeByteArray target 19 (ascii '.')
            writeDigits target (19 + fractionDigits) (fromIntegral significant)
        writeByteArray target (len - 1) (ascii 'Z')

ascii :: Char -> Word8
ascii = fromIntegral . ord

-- A count of digits and the value they spell, less every trailing zero of the value.
withoutTrailingZeros :: Int -> Int -> (Int, Int)
withoutTrailingZeros !count !value
    | value /= 0 && value `rem` 10 == 0 = withoutTrailingZeros (count - 1) (value `quot` 10)
    | otherwise = (count, value)

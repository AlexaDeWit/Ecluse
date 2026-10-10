-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The stamp read against the library's decoder, and the stamp render against the builder it replaces.
module Ecluse.Core.Text.Iso8601Spec (spec) where

import Data.Aeson (Value (String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime), diffTimeToPicoseconds, fromGregorian, gregorianMonthLength, isLeapYear, picosecondsToDiffTime, toGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hedgehog (Gen, LabelName, MonadTest, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Text.Iso8601 (readIso8601Utc, renderIso8601Utc)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), captureTexts, corpusPackages, pypiCorpusPackages)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    readIso8601Spec
    renderIso8601Spec

readIso8601Spec :: Spec
readIso8601Spec = describe "readIso8601Utc" $ do
    it "reads the layouts the registries write" $ do
        readIso8601Utc "2015-01-11T00:23:27.114Z" `shouldBe` Just (at 2015 1 11 ((23 * 60 + 27) * picosPerSecond + 114_000_000_000))
        readIso8601Utc "2026-05-14T19:25:26.443051Z" `shouldBe` Just (at 2026 5 14 ((19 * 3600 + 25 * 60 + 26) * picosPerSecond + 443_051_000_000))
        readIso8601Utc "2026-06-01T00:00:00Z" `shouldBe` Just (at 2026 6 1 0)

    it "declines a stamp that only the library parser reads" $
        for_ ["2020-01-01 00:00:00Z", "2020-01-01T00:00:00+00:00", "2016-12-31T23:59:60Z", "2020-01-01T24:00:00Z", "+2020-01-01T00:00:00Z", "12020-01-01T00:00:00Z", "2020-01-01T00:00Z"] $ \raw -> do
            readIso8601Utc raw `shouldBe` Nothing
            libraryInstant raw `shouldSatisfy` isRight

    modifyMaxSuccess (const 5000) $
        it "reads the library parser's instant from every stamp it takes, and takes the classes of stamp it should" $
            hedgehog $ do
                (drawn, takes, raw) <- forAll (Gen.choice [(,,) name verdict <$> gen | StampCase name verdict gen <- stampCases])
                for_ stampCases $ \(StampCase name _ _) -> cover 1 name (name == drawn)
                let instant = readIso8601Utc raw
                for_ takes (isJust instant ===)
                for_ instant ((libraryInstant raw ===) . Right)

    it "agrees with the library parser on days 00, 01 and 28 to 32 of months 00 to 13 of every year 0000 to 9999" $
        firstMisread [(raw, rightToMaybe (libraryInstant raw)) | year <- yearSpellings, month <- take 14 twoDigits, day <- ["00", "01", "28", "29", "30", "31", "32"], let raw = year <> "-" <> month <> "-" <> day <> "T12:34:56.789Z"]
            `shouldBe` []

    it "takes every time of day below 24:00:00 as the library parser reads it, and no hour 24, minute 60 or second 60" $ do
        let readings = [(raw, guard (hour < "24" && minute < "60" && secondOfMinute < "60") *> rightToMaybe (libraryInstant raw)) | hour <- take 25 twoDigits, minute <- take 61 twoDigits, secondOfMinute <- take 61 twoDigits, let raw = "2024-02-29T" <> hour <> ":" <> minute <> ":" <> secondOfMinute <> "Z"]
        firstMisread readings `shouldBe` []
        length (filter (isJust . snd) readings) `shouldBe` 86_400

    it "never reads another instant than the library parser, with any ASCII character at any position of a stamp" $
        take 5 [(raw, instant) | offset <- [0 .. T.length plainStamp], c <- ['\0' .. '\x7f'], raw <- [replaceAt offset (one c) plainStamp, T.take offset plainStamp <> one c <> T.drop offset plainStamp], Just instant <- [readIso8601Utc raw], libraryInstant raw /= Right instant]
            `shouldBe` []

    for_ corpusPackages (readsCapture npmStamps)
    for_ pypiCorpusPackages (readsCapture pypiStamps)

-- The first few stamps whose reading is not the expected one. One names the fault.
firstMisread :: [(Text, Maybe UTCTime)] -> [(Text, Maybe UTCTime, Maybe UTCTime)]
firstMisread readings = take 5 [(raw, instant, expected) | (raw, expected) <- readings, let instant = readIso8601Utc raw, instant /= expected]

-- Every publish time of the capture is in the layout the scan takes, and reads as the library parser reads it.
readsCapture :: J.Parser [Text] -> CorpusPackage -> Spec
readsCapture stampsOf package =
    it ("reads every publish time of the capture " <> cpPath package <> " as the library parser does") $ do
        stamps <- concat <$> captureTexts 1 stampsOf package
        take 5 [(raw, instant) | raw <- stamps, let { instant = readIso8601Utc raw }, fmap Right instant /= Just (libraryInstant raw)] `shouldBe` []

twoDigits, yearSpellings :: [Text]
twoDigits = [padded 2 n | n <- [0 .. 99 :: Int]]
yearSpellings = [padded 4 n | n <- [0 .. 9999 :: Int]]

padded :: (Show a) => Int -> a -> Text
padded width = T.justifyRight width '0' . show

plainStamp :: Text
plainStamp = "2023-10-09T08:07:06.123456Z"

-- A stamp in spelled parts, so a case can respell one of them.
data Stamp = Stamp {stampYear, stampMonth, stampDay, stampHour, stampMinute, stampSecond, stampFraction :: Text}

spell :: Stamp -> Text
spell stamp = stampYear stamp <> "-" <> stampMonth stamp <> "-" <> stampDay stamp <> "T" <> stampHour stamp <> ":" <> stampMinute stamp <> ":" <> stampSecond stamp <> stampFraction stamp <> "Z"

-- A date the calendar holds in years 0000 to 9999, a time of day below 24:00:00, and the given fraction.
genStamp :: Gen Text -> Gen Stamp
genStamp genFraction = do
    year <- Gen.integral (Range.linear 0 9999)
    month <- Gen.int (Range.linear 1 12)
    day <- Gen.int (Range.linear 1 (gregorianMonthLength year month))
    Stamp (padded 4 year) (padded 2 month) (padded 2 day) <$> below 24 <*> below 60 <*> below 60 <*> genFraction

-- A two-digit number below the bound, and one from the bound up.
below, atLeast :: Int -> Gen Text
below bound = padded 2 <$> Gen.int (Range.linear 0 (bound - 1))
atLeast bound = padded 2 <$> Gen.int (Range.linear bound 99)

fractionOf :: Int -> Gen Text
fractionOf count = T.cons '.' <$> Gen.text (Range.singleton count) Gen.digit

anyFraction :: Gen Text
anyFraction = Gen.choice [pure "", Gen.int (Range.linear 1 12) >>= fractionOf]

replaceAt :: Int -> Text -> Text -> Text
replaceAt offset with raw = T.take offset raw <> with <> T.drop (offset + 1) raw

-- A class of stamp: its coverage label, whether the scan takes it where the class fixes that, and its generator.
data StampCase = StampCase LabelName (Maybe Bool) (Gen Text)

taken, declined, undecided :: LabelName -> Gen Text -> StampCase
taken name = StampCase name (Just True)
declined name = StampCase name (Just False)
undecided name = StampCase name Nothing

stampCases :: [StampCase]
stampCases =
    [taken "no fraction" (spell <$> genStamp (pure ""))]
        <> [taken (fromString ("a " <> show count <> "-digit fraction")) (spell <$> genStamp (fractionOf count)) | count <- [1 .. 12]]
        <> [ taken "29 February of a leap year" . respelled $ \stamp -> do
                year <- Gen.filter isLeapYear (Gen.integral (Range.linear 0 9999))
                pure stamp{stampYear = padded 4 year, stampMonth = "02", stampDay = "29"}
           , taken "year 0000 or 9999" . respelled $ \stamp -> (\year -> stamp{stampYear = year, stampDay = "28"}) <$> Gen.element ["0000", "9999"]
           , taken "midnight or the last second of a day" . respelled $ \stamp ->
                (\(hour, minute) -> stamp{stampHour = hour, stampMinute = minute, stampSecond = minute}) <$> Gen.element [("00", "00"), ("23", "59")]
           , declined "a space in place of the T" (replaceAt 10 " " <$> usual)
           , declined "a lower-case t or z" (Gen.element [replaceAt 10 "t", (<> "z") . T.dropEnd 1] <*> usual)
           , declined "a numeric zone" ((<>) . T.dropEnd 1 <$> usual <*> Gen.element ["+00:00", "-00:00", "+01:30", "+0000", "+00", "-0800"])
           , declined "no zone" (T.dropEnd 1 <$> usual)
           , declined "text after the zone" ((<>) <$> usual <*> Gen.element ["Z", "0", " ", "\n", "+00:00"])
           , declined "leading or trailing white space" (Gen.element [(<>), flip (<>)] <*> Gen.element [" ", "\t", "\n", "\r", "\x00a0"] <*> usual)
           , declined "hour 24 or above" (respelled (\stamp -> (\hour -> stamp{stampHour = hour}) <$> atLeast 24))
           , declined "minute 60 or above" (respelled (\stamp -> (\minute -> stamp{stampMinute = minute}) <$> atLeast 60))
           , declined "second 60 or above" (respelled (\stamp -> (\secondOfMinute -> stamp{stampSecond = secondOfMinute}) <$> atLeast 60))
           , declined "month 00, or 13 or above" (respelled (\stamp -> (\month -> stamp{stampMonth = month}) <$> Gen.choice [pure "00", atLeast 13]))
           , declined "day 00, or 32 or above" (respelled (\stamp -> (\day -> stamp{stampDay = day}) <$> Gen.choice [pure "00", atLeast 32]))
           , declined "day 31 of a 30-day month" (respelled (\stamp -> (\month -> stamp{stampMonth = month, stampDay = "31"}) <$> Gen.element ["04", "06", "09", "11"]))
           , declined "29 February of a common year, or 30 February" . respelled $ \stamp ->
                Gen.choice
                    [ (\year -> stamp{stampYear = padded 4 year, stampMonth = "02", stampDay = "29"}) <$> Gen.filter (not . isLeapYear) (Gen.integral (Range.linear 0 9999))
                    , pure stamp{stampMonth = "02", stampDay = "30"}
                    ]
           , declined "a fraction of no digits" (respelled (\stamp -> pure stamp{stampFraction = "."}))
           , declined "a fraction of 13 digits or more" (respelled (\stamp -> (\fraction -> stamp{stampFraction = fraction}) <$> (Gen.int (Range.linear 13 20) >>= fractionOf)))
           , declined "a comma before the fraction" (respelled (\stamp -> (\count -> stamp{stampFraction = T.cons ',' (T.replicate count "5")}) <$> Gen.int (Range.linear 1 12)))
           , declined "a digit outside ASCII" (replaceAt <$> Gen.element digitOffsets <*> (one <$> Gen.element otherDigits) <*> usual)
           , declined "another character between the date fields" (replaceAt <$> Gen.element [4, 7] <*> Gen.element ["/", ".", " ", ":", "_"] <*> usual)
           , declined "another character between the time fields" (replaceAt <$> Gen.element [13, 16] <*> Gen.element [".", "-", " ", ";"] <*> usual)
           , declined "a field one digit short" (respelled (reshape (T.drop 1)))
           , declined "a field one digit long" (respelled (reshape (T.cons '1')))
           , declined "a signed year" (T.cons <$> Gen.element ("+-" :: String) <*> usual)
           , declined "seconds left out" (respelled (\stamp -> pure stamp{stampSecond = "", stampFraction = ""}) <&> T.replace ":Z" "Z")
           , declined "a date alone or a time alone" (Gen.element [T.take 10, T.drop 11] <*> usual)
           , declined "the empty text" (pure "")
           , undecided "one character replaced by any character" (replaceAt <$> Gen.int (Range.linear 0 32) <*> (one <$> Gen.choice [Gen.ascii, Gen.unicode]) <*> usual)
           , undecided "the characters of a stamp in any order" (Gen.text (Range.linear 0 34) (Gen.element ("0123456789-T:.Z" :: String)))
           , undecided "arbitrary text" (Gen.text (Range.linear 0 40) Gen.unicode)
           ]
  where
    usual = spell <$> genStamp anyFraction
    respelled change = spell <$> (genStamp anyFraction >>= change)
    -- One of the six date and time fields, respelled.
    reshape :: (Text -> Text) -> Stamp -> Gen Stamp
    reshape change stamp =
        Gen.element
            [ stamp{stampYear = change (stampYear stamp)}
            , stamp{stampMonth = change (stampMonth stamp)}
            , stamp{stampDay = change (stampDay stamp)}
            , stamp{stampHour = change (stampHour stamp)}
            , stamp{stampMinute = change (stampMinute stamp)}
            , stamp{stampSecond = change (stampSecond stamp)}
            ]
    digitOffsets = [0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18]
    -- The digit zero to nine of the Arabic-Indic, Devanagari and fullwidth blocks.
    otherDigits = ['\x0660' .. '\x0669'] <> ['\x0966' .. '\x096f'] <> ['\xff10' .. '\xff19']

renderIso8601Spec :: Spec
renderIso8601Spec = describe "renderIso8601Utc" $ do
    modifyMaxSuccess (const 3000) $
        it "gives the reference text for every instant, and iso8601Show's for a time of day from zero" $
            hedgehog $ do
                (dayClass, day) <- forAll (genClass dayClasses)
                (timeClass, picos) <- forAll (genClass timeClasses)
                coverEach dayClasses dayClass
                coverEach timeClasses timeClass
                let instant = UTCTime day (picosecondsToDiffTime picos)
                renderIso8601Utc instant === referenceRender instant
                when (picos >= 0) (renderIso8601Utc instant === toText (iso8601Show instant))

    it "gives the reference text at every second of a day" $
        firstMisrendered [at 2024 2 29 (secondOfDay * picosPerSecond) | secondOfDay <- [0 .. 86_399]] `shouldBe` []

    it "gives the reference text on the first and last day of every month of years 0 to 9999" $
        firstMisrendered [UTCTime day 45_296.789 | year <- [0 .. 9999], month <- [1 .. 12], day <- [fromGregorian year month 1, fromGregorian year month 31]]
            `shouldBe` []

    for_ corpusPackages (rendersCapture npmStamps)
    for_ pypiCorpusPackages (rendersCapture pypiStamps)

    it "renders the canonical npm shapes" $ do
        renderIso8601Utc (at 2015 1 11 ((0 * 3600 + 23 * 60 + 27) * picosPerSecond + 114_000_000_000))
            `shouldBe` "2015-01-11T00:23:27.114Z"
        renderIso8601Utc (at 2026 6 1 0) `shouldBe` "2026-06-01T00:00:00Z"
        renderIso8601Utc (at 44 12 31 (86_399 * picosPerSecond + 1))
            `shouldBe` "0044-12-31T23:59:59.000000000001Z"

    it "trims trailing fraction zeros without dropping significant ones" $
        renderIso8601Utc (at 2020 2 29 100_000_000_000) `shouldBe` "2020-02-29T00:00:00.1Z"

    it "renders a leap-second reading as iso8601Show does" $ do
        let instant = at 2016 12 31 (picosPerDay + 500_000_000_000)
        renderIso8601Utc instant `shouldBe` toText (iso8601Show instant)

    it "keeps a signed hour for a negative time of day" $ do
        renderIso8601Utc (at 2020 1 1 (-1)) `shouldBe` "2020-01-01T-1:59:59.999999999999Z"
        renderIso8601Utc (at 2020 1 1 (negate (10 * picosPerHour))) `shouldBe` "2020-01-01T-10:00:00Z"

at :: Integer -> Int -> Int -> Integer -> UTCTime
at year month day picos = UTCTime (fromGregorian year month day) (picosecondsToDiffTime picos)

picosPerSecond, picosPerHour, picosPerDay :: Integer
picosPerSecond = 1_000_000_000_000
picosPerHour = 3600 * picosPerSecond
picosPerDay = 24 * picosPerHour

{- The rendering in its plain form: 'iso8601Show' outside years 0 to 9999 and from 86 400 s, and a text
builder with 'String' padding for the rest, where a negative time of day gives a signed hour. -}
referenceRender :: UTCTime -> Text
referenceRender t@(UTCTime day dt)
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

    fraction :: TB.Builder
    fraction
        | frac == 0 = mempty
        | otherwise =
            TB.fromText ("." <> T.dropWhileEnd (== '0') (T.justifyRight 12 '0' (show frac)))

    digits :: Int -> Integer -> TB.Builder
    digits width n =
        let body = show n :: String
            pad = width - length body
         in TB.fromString (replicate pad '0') <> TBI.decimal n

-- The first few instants the renderer and the reference render differently. One names the fault.
firstMisrendered :: [UTCTime] -> [(UTCTime, Text, Text)]
firstMisrendered instants =
    take 5 [(instant, rendered, reference) | instant <- instants, let rendered = renderIso8601Utc instant, let reference = referenceRender instant, rendered /= reference]

-- A class of input: the coverage label a property must reach, and its generator.
type Class a = (LabelName, Gen a)

genClass :: [Class a] -> Gen (LabelName, a)
genClass classes = Gen.choice [(,) name <$> gen | (name, gen) <- classes]

coverEach :: (MonadTest m) => [Class a] -> LabelName -> m ()
coverEach classes drawn = for_ classes $ \(name, _) -> cover 1 name (name == drawn)

dayClasses :: [Class Day]
dayClasses =
    [ ("a year from 1000 to 9999", genDayIn 1000 9999)
    , ("a year from 1 to 999", genDayIn 1 999)
    , ("year 0", genDayIn 0 0)
    , ("the first day of year 0", pure (fromGregorian 0 1 1))
    , ("the last day of year 9999", pure (fromGregorian 9999 12 31))
    , ("29 February", (\year -> fromGregorian year 2 29) <$> Gen.filter isLeapYear (Gen.integral (Range.linear 0 9999)))
    , ("the day before year 0, rendered by iso8601Show", pure (fromGregorian (-1) 12 31))
    , ("the day after year 9999, rendered by iso8601Show", pure (fromGregorian 10_000 1 1))
    , ("a negative year, rendered by iso8601Show", genDayIn (-20_000) (-1))
    , ("a year above 9999, rendered by iso8601Show", genDayIn 10_000 30_000)
    , ("a day number far from years 0 to 9999, rendered by iso8601Show", ModifiedJulianDay <$> (Gen.element [id, negate] <*> Gen.integral (Range.linear 10_000_000 1_000_000_000_000)))
    ]

genDayIn :: Integer -> Integer -> Gen Day
genDayIn firstYear lastYear = fromGregorian <$> Gen.integral (Range.linear firstYear lastYear) <*> Gen.int (Range.linear 1 12) <*> Gen.int (Range.linear 1 31)

-- A time of day in picoseconds. The type also holds the values outside a day.
timeClasses :: [Class Integer]
timeClasses =
    [ ("midnight", pure 0)
    , ("whole seconds", wholeSeconds)
    , ("the last picosecond of a day", pure (picosPerDay - 1))
    , ("a fraction that starts with a zero", (+) <$> wholeSeconds <*> Gen.integral (Range.linear 1 99_999_999_999))
    ]
        <> [(fromString ("a " <> show count <> "-digit fraction"), genFraction count) | count <- [1 .. 12]]
        <> [ ("a leap second, rendered by iso8601Show", Gen.integral (Range.linear picosPerDay (picosPerDay + picosPerSecond - 1)))
           , ("past a leap second, rendered by iso8601Show", Gen.integral (Range.linear (picosPerDay + picosPerSecond) (10 ^ (21 :: Int))))
           , ("a negative time of day within an hour", negate <$> Gen.integral (Range.linear 1 picosPerHour))
           , ("a negative time of day within a day", negate <$> Gen.integral (Range.linear (picosPerHour + 1) picosPerDay))
           , ("a negative time of day past a day", negate <$> Gen.integral (Range.linear (picosPerDay + 1) (10 ^ (21 :: Int))))
           ]
  where
    wholeSeconds = (* picosPerSecond) <$> Gen.integral (Range.linear 0 86_399)
    -- A fraction whose last digit of the count is not a zero, so the rule prints exactly the count.
    genFraction :: Int -> Gen Integer
    genFraction count = do
        whole <- wholeSeconds
        leading <- Gen.integral (Range.linear 0 (10 ^ (count - 1) - 1))
        final <- Gen.integral (Range.linear 1 9)
        pure (whole + (leading * 10 + final) * 10 ^ (12 - count))

-- The instant the library parser reads from a stamp, as a JSON decode reads it.
libraryInstant :: Text -> Either String UTCTime
libraryInstant = parseEither parseJSON . String

-- Every publish time of the capture, read by the library parser, renders as the reference does.
rendersCapture :: J.Parser [Text] -> CorpusPackage -> Spec
rendersCapture stampsOf package =
    it ("gives the reference text for every publish time of the capture " <> cpPath package) $ do
        stamps <- concat <$> captureTexts 1 stampsOf package
        instants <- traverse (expectRight . libraryInstant) stamps
        firstMisrendered instants `shouldBe` []

-- One list for each member of an npm packument's @time@ and for each file of a PEP 691 index (@upload-time@).
npmStamps, pypiStamps :: J.Parser [Text]
npmStamps = "time" J..: J.objectValues (many J.string)
pypiStamps = "files" J..: J.arrayOf (many ("upload-time" J..: J.string))

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A comparison with the run on @main@ shows a change with its sign, says when a whole set
moved one way, and turns every missing or unreadable side into a note.
-}
module Ecluse.BenchReport.AgainstMainSpec (spec) where

import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.BenchReport.AgainstMain (
    Baseline (Baseline, NoBaseline),
    Origin (Origin),
    Paired (Paired),
    Report (..),
    Spread (..),
    groupedBy,
    pairBy,
    parseOrigin,
    percentChange,
    renderAgainstMain,
    signedPercent,
    spreadLine,
    spreadOf,
    unpairedLines,
 )

spec :: Spec
spec = do
    describe "parseOrigin" $ do
        it "reads the commit, the run, and its creation time" $
            parseOrigin "commit=abc123\nrun=https://example.test/runs/7\ncreated=2026-01-02T03:04:05Z\n" `shouldBe` Right origin
        it "keeps an equals sign inside a value" $
            parseOrigin "commit=abc123\nrun=https://example.test/runs?id=7\ncreated=now" `shouldBe` Right (Origin "abc123" "https://example.test/runs?id=7" "now")
        it "returns the reason of a fetch that found no baseline" $
            parseOrigin "unavailable=No successful run of bench.yml on main was found.\n"
                `shouldBe` Left "No successful run of bench.yml on main was found."
        it "names a key the record lacks" $
            parseOrigin "commit=abc123\ncreated=now" `shouldBe` Left "The baseline record names no run."
        it "counts an empty value as lacking" $
            parseOrigin "commit=\nrun=r\ncreated=now" `shouldBe` Left "The baseline record names no commit."

    describe "percentChange" $ do
        it "is the change in percent of the figure on main" $ do
            percentChange 200 150 `shouldBe` Just (-25)
            percentChange 200 250 `shouldBe` Just 25
        it "has no value over a zero on main" $
            percentChange 0 5 `shouldBe` Nothing

    describe "signedPercent" $ do
        it "signs a rise and a fall to one decimal place" $ do
            signedPercent 3.14 `shouldBe` "+3.1%"
            signedPercent (-12.36) `shouldBe` "-12.4%"
        it "gives a change that rounds to zero no sign" $ do
            signedPercent 0.04 `shouldBe` "0.0%"
            signedPercent (-0.04) `shouldBe` "0.0%"
            signedPercent 0 `shouldBe` "0.0%"

    describe "spreadOf" $ do
        it "counts the rows that rose, fell, and held, with the extremes" $
            spreadOf (3 :| [-1, 0, 7, -5]) `shouldBe` Spread{spreadRose = 2, spreadFell = 2, spreadHeld = 1, spreadMedian = 0, spreadLowest = -5, spreadHighest = 7}
        it "takes the mean of the middle two of an even count" $
            spreadMedian (spreadOf (10 :| [1, 4, 2])) `shouldBe` 3
        it "is the value itself for one row" $
            spreadOf (4 :| []) `shouldBe` Spread 1 0 0 4 4 4

    describe "spreadLine" $ do
        it "counts each direction and gives the median and the range" $
            spreadLine "Scenarios" "Successes" [3, -1, 0, 7, -5]
                `shouldBe` "Scenarios compared: 5. Successes rose in 2, fell in 2, and held in 1. The median change is 0.0%, and the changes run from -5.0% to +7.0%."
        it "says so when no row moved against the others" $
            spreadLine "Legs" "Time" [3, 1, 0, 7]
                `shouldSatisfy` T.isSuffixOf "from 0.0% to +7.0%. None moved the other way. One runner measured them all, so the runner can be the cause."
        it "says the same of a set that only fell" $
            spreadLine "Legs" "Time" [-3, -1] `shouldSatisfy` T.isInfixOf "None moved the other way."
        it "does not say it of one row that moved, or of rows that all held" $ do
            spreadLine "Legs" "Time" [3, 0] `shouldNotSatisfy` T.isInfixOf "None moved the other way."
            spreadLine "Legs" "Time" [0, 0] `shouldNotSatisfy` T.isInfixOf "None moved the other way."
        it "has a line for a set with no change to show" $
            spreadLine "Legs" "Time" [] `shouldBe` "Legs compared: 0. No row has a figure on both sides."

    describe "pairBy" $ do
        it "matches rows by key in this run's order, and names the rows only one side holds" $
            pairBy fst [("a", 1), ("b", 2), ("gone", 3)] [("b", 20), ("new", 40), ("a", 10 :: Int)]
                `shouldBe` Paired [(("b" :: Text, 2), ("b", 20)), (("a", 1), ("a", 10))] ["new"] ["gone"]
        it "groups the matched rows by this run's row, in order of first appearance" $
            groupedBy fst [(("b", 1), ("b", 2)), (("a", 3), ("a", 4)), (("b", 5), ("b", 6 :: Int))]
                `shouldBe` [("b" :: Text, [(("b", 1), ("b", 2)), (("b", 5), ("b", 6))]), ("a", [(("a", 3), ("a", 4))])]
        it "names both sides' own rows in a line each, and nothing when every row matched" $ do
            unpairedLines id (Paired [] ["new", "newer"] ["gone"] :: Paired Text ())
                `shouldBe` ["Only in this run: `new`, `newer`.", "", "Only on `main`: `gone`.", ""]
            unpairedLines id (Paired [] [] [] :: Paired Text ()) `shouldBe` []

    describe "renderAgainstMain" $ do
        let rendered baseline current = T.lines (renderAgainstMain numbers baseline current)
        it "sets this run's reading beside the one on main, under the run it came from" $
            rendered (Baseline origin "2") (Right "3")
                `shouldBe` [ "## Numbers against main"
                           , ""
                           , "Compared with `main` at `abc123` ([run](https://example.test/runs/7) created 2026-01-02T03:04:05Z)."
                           , ""
                           , "2 then 3"
                           , "note"
                           ]
        it "shortens the commit to nine characters" $
            rendered (Baseline (Origin "0123456789abcdef" "u" "t") "2") (Right "3") `shouldSatisfy` any (T.isInfixOf "`main` at `012345678` ")
        it "prints no baseline, with the reason, when the fetch found none" $
            rendered (NoBaseline "No successful run of bench.yml on main was found.") (Right "3")
                `shouldBe` [ "## Numbers against main"
                           , ""
                           , "**No baseline.** No successful run of bench.yml on main was found."
                           , ""
                           , "Nothing is compared, and the job does not fail on it."
                           , ""
                           ]
        it "prints no baseline when the report on main does not parse" $
            rendered (Baseline origin "two") (Right "3")
                `shouldSatisfy` elem "**No baseline.** The report of `main` at `abc123` ([run](https://example.test/runs/7) created 2026-01-02T03:04:05Z) could not be read. Not a number: two."
        it "says so when this run's report is missing or does not parse, whatever the baseline" $ do
            rendered (Baseline origin "2") (Left "A file is missing.")
                `shouldBe` ["## Numbers against main", "", "**Nothing to compare.** This run's report could not be read. A file is missing.", ""]
            rendered (NoBaseline "None.") (Right "three")
                `shouldBe` ["## Numbers against main", "", "**Nothing to compare.** This run's report could not be read. Not a number: three.", ""]

    describe "properties" $
        it "a spread's median lies between its extremes, and its counts cover every row" $
            hedgehog $ do
                changes <- forAll (Gen.nonEmpty (Range.linear 1 40) (Gen.double (Range.linearFrac (-100) 400)))
                let spread = spreadOf changes
                (spreadLowest spread <= spreadMedian spread && spreadMedian spread <= spreadHighest spread) === True
                spreadRose spread + spreadFell spread + spreadHeld spread === length changes

origin :: Origin
origin = Origin "abc123" "https://example.test/runs/7" "2026-01-02T03:04:05Z"

-- A report that is one number, compared in one line.
numbers :: Report Int
numbers =
    Report
        { reportTitle = "Numbers against main"
        , reportRead = \raw -> maybeToRight ("Not a number: " <> raw <> ".") (readMaybe (toString raw))
        , reportBody = \onMain here -> [show onMain <> " then " <> show here]
        , reportNotes = ["note"]
        }

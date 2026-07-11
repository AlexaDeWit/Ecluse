module Ecluse.BenchReportSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchReport (
    BenchRow (BenchRow, rowBench),
    ReportInput (ReportInput, riConsoleLog, riCsv),
    formatBytes,
    formatPs,
    groupRows,
    parseCsv,
    renderReport,
    splitName,
    stripAnsi,
 )

spec :: Spec
spec = do
    describe "parseCsv" $ do
        it "parses a GC-stats row into its group, bench, and columns" $
            parseCsv (gcHeader <> "\nAll.ecluse-core (work-per-request).rules.evalRules.react,3210000000,240000000,8800000,12000,157286400")
                `shouldBe` Right
                    [BenchRow "rules.evalRules" "react" 3210000000 240000000 (Just 8800000) (Just 12000) (Just 157286400)]
        it "unquotes an RFC-4180 name carrying commas" $
            parseCsv (gcHeader <> "\n\"All.ecluse-core (work-per-request).security guards.boundedRead (8 MiB body, 64 KiB chunks)\",214718604,35681628,8459277,7377,289406976")
                `shouldBe` Right
                    [BenchRow "security guards" "boundedRead (8 MiB body, 64 KiB chunks)" 214718604 35681628 (Just 8459277) (Just 7377) (Just 289406976)]
        it "reads a doubled quote inside a quoted name as a literal quote" $
            parseCsv (gcHeader <> "\n\"All.group.say \"\"hi\"\"\",1,2,3,4,5")
                `shouldBe` Right [BenchRow "group" "say \"hi\"" 1 2 (Just 3) (Just 4) (Just 5)]
        it "accepts the plain time-only shape, leaving the GC columns absent" $
            parseCsv "Name,Mean (ps),2*Stdev (ps)\nAll.g.b,10,2"
                `shouldBe` Right [BenchRow "g" "b" 10 2 Nothing Nothing Nothing]
        it "tolerates CRLF line endings" $
            parseCsv (gcHeader <> "\r\nAll.g.b,10,2,3,4,5\r\n")
                `shouldBe` Right [BenchRow "g" "b" 10 2 (Just 3) (Just 4) (Just 5)]
        it "rejects an empty file" $
            parseCsv "" `shouldSatisfy` isLeft
        it "rejects an unrecognised header" $
            parseCsv "Name,Mean (ps)\nAll.g.b,10" `shouldSatisfy` isLeft
        it "rejects a row whose arity does not match the header" $
            parseCsv (gcHeader <> "\nAll.g.b,10,2") `shouldSatisfy` isLeft
        it "rejects an unreadable number" $
            parseCsv (gcHeader <> "\nAll.g.b,ten,2,3,4,5") `shouldSatisfy` isLeft
        it "rejects an unterminated quote" $
            parseCsv (gcHeader <> "\n\"All.g.b,10,2,3,4,5") `shouldSatisfy` isLeft

    describe "splitName" $ do
        it "strips the tier prefix and takes the last dot segment as the bench" $
            splitName "All.ecluse-core (work-per-request).rules.evalRules.react"
                `shouldBe` ("rules.evalRules", "react")
        it "falls back to stripping the bare All. root" $
            splitName "All.route.classify.mixed npm requests"
                `shouldBe` ("route.classify", "mixed npm requests")
        it "files a dotless name under the explicit ungrouped heading" $
            splitName "solo" `shouldBe` ("(ungrouped)", "solo")

    describe "groupRows" $
        it "groups by heading, preserving first-appearance and row order" $
            map (second (map rowBench . toList)) (groupRows [mkRow "b" "x", mkRow "a" "y", mkRow "b" "z"])
                `shouldBe` [("b", ["x", "z"]), ("a", ["y"])]

    describe "formatPs" $ do
        it "keeps sub-nanosecond figures in picoseconds" $
            formatPs 950 `shouldBe` "950 ps"
        it "scales through nanoseconds" $
            formatPs 7320 `shouldBe` "7.32 ns"
        it "scales through milliseconds at three significant figures" $ do
            formatPs 1420827437 `shouldBe` "1.42 ms"
            formatPs 51446034200 `shouldBe` "51.4 ms"
            formatPs 240000000 `shouldBe` "240 us"
        it "tops out at seconds" $
            formatPs 2500000000000 `shouldBe` "2.50 s"
        it "renders zero bare" $
            formatPs 0 `shouldBe` "0 ps"

    describe "formatBytes" $ do
        it "keeps small figures in bytes" $
            formatBytes 512 `shouldBe` "512 B"
        it "scales through KiB and MiB at three significant figures" $ do
            formatBytes 12000 `shouldBe` "11.7 KiB"
            formatBytes 4566371 `shouldBe` "4.35 MiB"
            formatBytes 157286400 `shouldBe` "150 MiB"
        it "tops out at GiB" $
            formatBytes 1288490188 `shouldBe` "1.20 GiB"

    describe "stripAnsi" $ do
        it "drops colour and reset sequences" $
            stripAnsi "\ESC[32mtask: bench\ESC[0m done" `shouldBe` "task: bench done"
        it "drops an unterminated sequence to the end" $
            stripAnsi "a\ESC[12" `shouldBe` "a"
        it "leaves escape-free text alone" $
            stripAnsi "plain" `shouldBe` "plain"

    describe "renderReport" $ do
        let reportOf consoleLog = renderReport (ReportInput{riCsv = parseCsv sampleCsv, riConsoleLog = consoleLog})
            report = reportOf (Just "\ESC[32mAll 3 tests passed\ESC[0m")
        it "leads with the inform-only preamble" $
            ("## Benchmarks -- work-per-request over ecluse-core" `T.isInfixOf` report) `shouldBe` True
        it "names the operating point" $
            ("3 benches in 2 groups" `T.isInfixOf` report) `shouldBe` True
        it "links each at-a-glance group to its section, headlined by its slowest bench" $
            ("| [rules.evalRules](#rulesevalrules) | 2 | webpack | 4.21 ms | 9.35 MiB |" `T.isInfixOf` report)
                `shouldBe` True
        it "renders a detail section per group" $ do
            ("### rules.evalRules" `T.isInfixOf` report) `shouldBe` True
            ("| react | 3.21 ms | 240 us | 8.39 MiB | 11.7 KiB | 150 MiB |" `T.isInfixOf` report)
                `shouldBe` True
        it "carries the ANSI-stripped console output in a collapsed section" $ do
            ("<details>" `T.isInfixOf` report) `shouldBe` True
            ("All 3 tests passed" `T.isInfixOf` report) `shouldBe` True
            ("\ESC[" `T.isInfixOf` report) `shouldBe` False
        it "notes an uncaptured console log instead of the collapsed section" $ do
            let noLog = reportOf Nothing
            ("console log was not captured" `T.isInfixOf` noLog) `shouldBe` True
            ("<details>" `T.isInfixOf` noLog) `shouldBe` False
        it "closes with the reading notes" $
            ("### Reading the numbers" `T.isInfixOf` report) `shouldBe` True
        it "renders a failed CSV as a loud note, never a silently empty summary" $ do
            let failed = renderReport (ReportInput{riCsv = Left "could not read bench-results.csv", riConsoleLog = Nothing})
            ("**No benchmark results to render** -- could not read bench-results.csv." `T.isInfixOf` failed)
                `shouldBe` True
        it "renders a row-less CSV as the loud note too" $
            ("No benchmark results to render" `T.isInfixOf` renderReport (ReportInput{riCsv = Right [], riConsoleLog = Nothing}))
                `shouldBe` True

gcHeader :: Text
gcHeader = "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory"

-- Two groups, three benches; webpack is the slowest of its group.
sampleCsv :: Text
sampleCsv =
    T.unlines
        [ gcHeader
        , "All.ecluse-core (work-per-request).rules.evalRules.react,3210000000,240000000,8800000,12000,157286400"
        , "All.ecluse-core (work-per-request).rules.evalRules.webpack,4210000000,240000000,9800000,12000,157286400"
        , "All.ecluse-core (work-per-request).route.classify.mixed npm requests,4042481275,243185584,18465548,10143,157286400"
        ]

mkRow :: Text -> Text -> BenchRow
mkRow grp leaf = BenchRow grp leaf 1 1 Nothing Nothing Nothing

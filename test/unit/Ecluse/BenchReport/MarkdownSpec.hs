-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The table reader takes back what the row writer prints, and finds each table of a report with its heading.
module Ecluse.BenchReport.MarkdownSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.BenchReport.Markdown (Table (Table, tableHeading), cells, hasColumns, linkText, records, tables)

spec :: Spec
spec = do
    describe "tables" $ do
        it "reads a table with the heading above it, its header, and its rows" $
            tables (T.unlines ["### npm", "", "| Package | Time (ms) |", "|---|--:|", "| lodash | 4.646 |", "| react | 146.246 |"])
                `shouldBe` [Table (Just "npm") ["Package", "Time (ms)"] [["lodash", "4.646"], ["react", "146.246"]]]
        it "skips the rule under the header, whatever its alignment marks" $
            tables "| a | b |\n| :-- | --: |\n| 1 | 2 |" `shouldBe` [Table Nothing ["a", "b"] [["1", "2"]]]
        it "drops a row whose cell count differs from the header's" $
            tables "| a | b |\n| --- | --- |\n| 1 |\n| 1 | 2 | 3 |\n| 4 | 5 |" `shouldBe` [Table Nothing ["a", "b"] [["4", "5"]]]
        it "gives each table the last heading before it" $
            map tableHeading (tables "# one\n## two\n| a |\n| 1 |\n\nprose\n\n| b |\n| 2 |\n### three\n| c |\n| 3 |")
                `shouldBe` [Just "two", Just "two", Just "three"]
        it "ends a table at the first line that is not a row" $
            tables "| a |\n| 1 |\nprose\n| 2 |" `shouldBe` [Table Nothing ["a"] [["1"]], Table Nothing ["2"] []]
        it "reads rows that end a line with a carriage return" $
            tables "| a | b |\r\n| --- | --- |\r\n| 1 | 2 |\r\n" `shouldBe` [Table Nothing ["a", "b"] [["1", "2"]]]
        it "does not take a hash without a space after it for a heading" $
            map tableHeading (tables "#1305\n| a |\n| 1 |") `shouldBe` [Nothing]
        it "finds no table in prose" $
            tables "No table here.\n\n- a list item | with a bar" `shouldBe` []

    describe "hasColumns" $ do
        let table = Table Nothing ["Package", "Leg", "Time (ms)"] []
        it "compares the header without case" $
            hasColumns ["package", "time (ms)"] table `shouldBe` True
        it "fails on one missing column" $
            hasColumns ["package", "successes"] table `shouldBe` False

    describe "records" $
        it "keys each cell by its lower-cased header cell" $
            records (Table Nothing ["Package", "Time (ms)"] [["lodash", "4.646"]])
                `shouldBe` [Map.fromList [("package", "lodash"), ("time (ms)", "4.646")]]

    describe "linkText" $ do
        it "takes the text of a cell that is one link" $
            linkText "[npm/merge-cold](#npm/merge-cold)" `shouldBe` "npm/merge-cold"
        it "leaves a plain cell unchanged" $
            linkText "npm/merge-cold" `shouldBe` "npm/merge-cold"
        it "leaves a cell that only opens like a link unchanged" $
            linkText "[half" `shouldBe` "[half"

    describe "properties" $
        it "reads back every table the row writer prints" $
            hedgehog $ do
                width <- forAll (Gen.int (Range.linear 1 6))
                header <- forAll (Gen.list (Range.singleton width) cell)
                rows <- forAll (Gen.list (Range.linear 0 8) (Gen.list (Range.singleton width) cell))
                tables (T.unlines (cells header : cells (replicate width "---") : map cells rows))
                    === [Table Nothing header rows]

-- A cell as the reports print one: words with single spaces between them, and no bar.
cell :: Gen Text
cell = T.unwords <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 1 8) Gen.alphaNum)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.MarkdownSpec (spec) where

import Test.Hspec

import Ecluse.Site.Markdown (
    Alignment (AlignLeft, AlignRight),
    attributedHeading,
    bold,
    code,
    escapeCell,
    heading,
    htmlSpan,
    link,
    slugify,
    table,
 )

spec :: Spec
spec = do
    describe "heading" $ do
        it "repeats the hash marker once per level" $
            heading 3 "Threat detail" `shouldBe` "### Threat detail"
        it "clamps a level below one" $
            heading 0 "Threat detail" `shouldBe` "# Threat detail"

    describe "attributedHeading" $ do
        it "closes the line with the anchor and every class" $
            attributedHeading 3 "threat-7" ["threat"] "7. A title"
                `shouldBe` "### 7. A title {#threat-7 .threat}"
        it "emits the anchor alone when no class is given" $
            attributedHeading 2 "schema-packument" [] "Packument"
                `shouldBe` "## Packument {#schema-packument}"

    describe "table" $ do
        it "renders the header, the alignment rule, then one line per row" $
            table [(AlignRight, "#"), (AlignLeft, "Threat")] [["1", "A title"], ["2", "Another"]]
                `shouldBe` [ "| # | Threat |"
                           , "| --: | :-- |"
                           , "| 1 | A title |"
                           , "| 2 | Another |"
                           ]
        it "renders the header and rule alone for an empty body" $
            table [(AlignLeft, "URL")] [] `shouldBe` ["| URL |", "| :-- |"]

    describe "inlines" $ do
        it "wraps bold text" $ bold "Threat." `shouldBe` "**Threat.**"
        it "wraps inline code" $ code "/npm/{package}" `shouldBe` "`/npm/{package}`"
        it "wraps a link" $ link "14" "#threat-14" `shouldBe` "[14](#threat-14)"
        it "wraps a span with its classes" $
            htmlSpan ["badge", "severity-high"] "High"
                `shouldBe` "<span class=\"badge severity-high\">High</span>"

    describe "escapeCell" $ do
        it "escapes a pipe that would end the cell" $
            escapeCell "a | b" `shouldBe` "a \\| b"
        it "collapses a line break that would end the row" $
            escapeCell "first\nsecond" `shouldBe` "first second"
        it "collapses a run of whitespace" $
            escapeCell "  spaced   out  " `shouldBe` "spaced out"

    describe "slugify" $ do
        it "lowercases a single word" $ slugify "High" `shouldBe` "high"
        it "lowercases an abbreviation" $ slugify "NA" `shouldBe` "na"
        it "turns each run of other characters into one hyphen" $
            slugify "npm.packument" `shouldBe` "npm-packument"
        it "trims a leading and trailing run" $
            slugify "/npm/{package}" `shouldBe` "npm-package"

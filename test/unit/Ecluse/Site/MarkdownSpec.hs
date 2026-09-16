-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.MarkdownSpec (spec) where

import Test.Hspec

import Ecluse.Site.Markdown (
    attributedHeading,
    bold,
    code,
    escapeCell,
    heading,
    link,
    slugify,
    table,
 )

spec :: Spec
spec = do
    describe "heading" $ do
        it "repeats the hash marker once per level" $
            heading 3 "Schemas" `shouldBe` "### Schemas"
        it "clamps a level below one" $
            heading 0 "Schemas" `shouldBe` "# Schemas"

    describe "attributedHeading" $ do
        it "closes the line with the anchor and every class" $
            attributedHeading 3 "get-npm-package" ["operation"] "GET /npm/{package}"
                `shouldBe` "### GET /npm/{package} {#get-npm-package .operation}"
        it "emits the anchor alone when no class is given" $
            attributedHeading 2 "schema-packument" [] "Packument"
                `shouldBe` "## Packument {#schema-packument}"

    describe "table" $ do
        it "renders the header, the alignment rule, then one line per row" $
            table ["Status", "Description"] [["200", "The packument"], ["404", "Not found"]]
                `shouldBe` [ "| Status | Description |"
                           , "| :-- | :-- |"
                           , "| 200 | The packument |"
                           , "| 404 | Not found |"
                           ]
        it "renders the header and rule alone for an empty body" $
            table ["URL"] [] `shouldBe` ["| URL |", "| :-- |"]

    describe "inlines" $ do
        it "wraps bold text" $ bold "Parameters" `shouldBe` "**Parameters**"
        it "wraps inline code" $ code "/npm/{package}" `shouldBe` "`/npm/{package}`"
        it "wraps a link" $ link "Packument" "#schema-packument" `shouldBe` "[Packument](#schema-packument)"

    describe "escapeCell" $ do
        it "escapes a pipe that would end the cell" $
            escapeCell "a | b" `shouldBe` "a \\| b"
        it "collapses a line break that would end the row" $
            escapeCell "first\nsecond" `shouldBe` "first second"
        it "collapses a run of whitespace" $
            escapeCell "  spaced   out  " `shouldBe` "spaced out"

    describe "slugify" $ do
        it "lowercases a single word" $ slugify "Packument" `shouldBe` "packument"
        it "lowercases an abbreviation" $ slugify "GET" `shouldBe` "get"
        it "turns each run of other characters into one hyphen" $
            slugify "npm.packument" `shouldBe` "npm-packument"
        it "trims a leading and trailing run" $
            slugify "/npm/{package}" `shouldBe` "npm-package"

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.SearchSpec (spec) where

import Ecluse.Site.Search (prepareSearchMetadata)
import Test.Hspec

spec :: Spec
spec = describe "prepareSearchMetadata" $ do
    it "preserves escaped title text, content and anchors" $
        prepareSearchMetadata "Module" "<title>A &amp; B</title><p id=\"x\">Body</p>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">A &amp; B</title><p id=\"x\">Body</p>"
    it "retains explicit metadata and the automatic heading" $
        prepareSearchMetadata "Module" "<title>Source</title><h1>Guide</h1><b data-pagefind-meta=\"title\">Override</b>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">Source</title><h1>Guide</h1><b data-pagefind-meta=\"title\">Override</b>"
    it "leaves existing fallback attributes unchanged" $
        prepareSearchMetadata "Module" "<title data-pagefind-default-meta=\"title\">Source</title>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">Source</title>"
    it "is idempotent" $ do
        for_ ["<head><title>Module</title></head><body>Source</body>", "<head></head><body>Source</body>"] $ \page ->
            prepareSearchMetadata "Module" (prepareSearchMetadata "Module" page) `shouldBe` prepareSearchMetadata "Module" page
    it "titles a Haddock source page from its module filename" $
        prepareSearchMetadata "Ecluse.Core.Text (source)" "<html><head><link href=\"style.css\" /></head><body><pre><span id=\"line-1\">source</span></pre></body></html>"
            `shouldBe` "<html><head><link href=\"style.css\" /><title data-pagefind-default-meta=\"title\">Ecluse.Core.Text (source)</title></head><body><pre><span id=\"line-1\">source</span></pre></body></html>"
    it "escapes a derived title" $
        prepareSearchMetadata "A & <B>" "<head></head>"
            `shouldBe` "<head><title data-pagefind-default-meta=\"title\">A &amp; &lt;B&gt;</title></head>"
    it "leaves an attributed title intact" $
        prepareSearchMetadata "Module" "<title class=\"custom\">Existing</title>"
            `shouldBe` "<title class=\"custom\">Existing</title>"

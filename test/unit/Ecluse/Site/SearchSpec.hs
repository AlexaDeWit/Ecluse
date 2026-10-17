-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.SearchSpec (spec) where

import Ecluse.Site.Search (prepareSearchMetadata)
import Test.Hspec

spec :: Spec
spec = describe "prepareSearchMetadata" $ do
    it "preserves escaped title text, content and anchors" $
        prepareSearchMetadata "<title>A &amp; B</title><p id=\"x\">Body</p>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">A &amp; B</title><p id=\"x\">Body</p>"
    it "retains explicit metadata and the automatic heading" $
        prepareSearchMetadata "<title>Source</title><h1>Guide</h1><b data-pagefind-meta=\"title\">Override</b>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">Source</title><h1>Guide</h1><b data-pagefind-meta=\"title\">Override</b>"
    it "leaves existing fallback attributes unchanged" $
        prepareSearchMetadata "<title data-pagefind-default-meta=\"title\">Source</title>"
            `shouldBe` "<title data-pagefind-default-meta=\"title\">Source</title>"
    it "is idempotent" $ do
        let page = "<html><title>Module</title><body>Source</body></html>"
        prepareSearchMetadata (prepareSearchMetadata page) `shouldBe` prepareSearchMetadata page
    it "leaves pages without a generated title unchanged" $
        prepareSearchMetadata "<h1>Guide</h1>" `shouldBe` "<h1>Guide</h1>"

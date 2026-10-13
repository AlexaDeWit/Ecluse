-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Literal formatting expectations shared by every report consumer.
module Ecluse.Report.MarkdownSpec (spec) where

import Ecluse.Report.Markdown (cells, fixed, signedPercent)
import Test.Hspec

spec :: Spec
spec = do
    describe "signedPercent" $
        for_ [(12.34, "+12.3%"), (-12.34, "-12.3%"), (0, "0.0%"), (0.001, "0.0%"), (-0.001, "0.0%")] $ \(value, expected) ->
            it ("renders " <> show value) $ signedPercent value `shouldBe` expected
    describe "cells" $ do
        it "keeps the row delimiters and spaces" $
            cells ["one", "two"] `shouldBe` "| one | two |"
        it "renders an empty row" $
            cells [] `shouldBe` "|  |"
    describe "fixed" $
        it "keeps the requested decimal places" $
            fixed 3 5 `shouldBe` "5.000"

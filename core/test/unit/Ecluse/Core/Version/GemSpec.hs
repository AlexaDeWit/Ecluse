-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Canonical Gem artifact spelling over the existing ordering tokens.
module Ecluse.Core.Version.GemSpec (spec) where

import Test.Hspec

import Ecluse.Core.Version.Gem (parseGem, renderGem)

spec :: Spec
spec = describe "renderGem" $
    for_ [("1.19.0", "1.19"), ("2.0.a.0", "2.a"), ("1.0.0-1", "1.pre.1"), ("0.0.0", "0"), ("0.a", "a")] $ \(raw, expected) ->
        it ("renders " <> toString raw <> " as " <> toString expected) $ do
            renderGem <$> parseGem raw `shouldBe` Just expected
            parseGem expected `shouldBe` parseGem raw

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exact-byte snapshot scope survives projection without hashing the projected view.
module Ecluse.Core.SnapshotSpec (spec) where

import Test.Hspec

import Ecluse.Core.Snapshot (Snapshot (..), digestOf)

-- | Pin byte-level identity independently of decoded JSON equality.
spec :: Spec
spec = describe "snapshot scope" $ do
    it "distinguishes upstream bodies that differ only in JSON whitespace" $
        digestOf "{\"files\":[]}" `shouldNotBe` digestOf "{ \"files\": [] }"
    it "preserves the original digest when a view changes" $ do
        let source = Snapshot (digestOf "upstream bytes") (1 :: Int)
            projected = show <$> source :: Snapshot Text
        snapshotDigest projected `shouldBe` snapshotDigest source
        snapshotValue projected `shouldBe` "1"

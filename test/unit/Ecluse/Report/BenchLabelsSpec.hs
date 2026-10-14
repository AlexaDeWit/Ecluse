-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Literal row names protect historical benchmark identities.
module Ecluse.Report.BenchLabelsSpec (spec) where

import Ecluse.BenchReport (splitEcosystem, splitName)
import Ecluse.Report.BenchLabels (ecosystemPrefix, tierName, tierPrefix)
import Test.Hspec

spec :: Spec
spec = do
    it "keeps the top-level group unchanged" $
        tierName `shouldBe` "ecluse-core (work-per-request)"
    it "keeps the CSV prefix unchanged" $
        tierPrefix `shouldBe` "All.ecluse-core (work-per-request)."
    it "keeps the ecosystem prefix including its trailing space" $
        ecosystemPrefix `shouldBe` "ecosystem: "
    for_ ["npm", "pypi"] $ \ecosystem ->
        it ("retains the complete " <> toString ecosystem <> " row name") $ do
            let groupName = ecosystemPrefix <> ecosystem <> ".cold production reads (per package)"
                rowName = tierPrefix <> groupName <> ".full document"
            rowName `shouldBe` ("All.ecluse-core (work-per-request).ecosystem: " <> ecosystem <> ".cold production reads (per package).full document")
            splitName rowName `shouldBe` (groupName, "full document")
            splitEcosystem groupName `shouldBe` (Just ecosystem, "cold production reads (per package)")

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Negative audit observations must not certify a successful revocation.
module Ecluse.Test.Contract.Sweep.AdvisorySpec (spec) where

import Ecluse.Test.Contract.Sweep.Advisory (reportsRevocation)
import Test.Hspec

-- | A forged or incomplete audit must fail the shared revocation observation.
spec :: Spec
spec = describe "advisory revocation audit evidence" $ do
    it "accepts the exact deletion and tally without pinning the generation" $
        reportsRevocation deletion tally [deletion <> "; advisory generation generated-at-runtime", tally] `shouldBe` True
    it "rejects an extra deletion even when the requested version and tally appear" $
        reportsRevocation deletion tally [deletion, "deleting sibling@2: blocked", tally] `shouldBe` False
    it "rejects a duplicate deletion of the requested version" $
        reportsRevocation deletion tally [deletion, deletion, tally] `shouldBe` False
    it "rejects a missing or different cycle tally" $ do
        reportsRevocation deletion tally [deletion] `shouldBe` False
        reportsRevocation deletion tally [deletion, "mirror sweep cycle complete: deleted 2"] `shouldBe` False
    it "rejects a different version with the same advisory reason" $
        reportsRevocation deletion tally ["deleting affected@2: blocked by advisory", tally] `shouldBe` False

deletion :: Text
deletion = "deleting affected@1: blocked by advisory"

tally :: Text
tally = "mirror sweep cycle complete: examined 2, deleted 1, kept 1, guard-skipped 0"

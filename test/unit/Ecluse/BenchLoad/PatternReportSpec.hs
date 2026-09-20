-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.PatternReportSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.PatternReport

spec :: Spec
spec = describe "pattern cache evidence" $ do
    it "keeps retention, collapsed waiters, and refused leaders separate" $ do
        let report = renderStoreEvidence [StoreEvidence "full" 1000 2000 900 2 3 5 1]
        report `shouldSatisfy` T.isInfixOf "| full | 1000 | 2000 | 2.0 | 900 | 0.2 | 0.5 | 2 / 3 / 5 | 1 |"
    it "does not report perfect hit or collapse fractions for an untouched store" $
        renderStoreEvidence [StoreEvidence "version" 100 2000 0 0 0 0 0]
            `shouldSatisfy` T.isInfixOf "| 0 | n/a | n/a | 0 / 0 / 0 | 0 |"

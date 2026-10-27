-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Child argv contracts include malformed arguments and unchanged RTS positions.
module Ecluse.Core.Server.MemoryModel.ProbeModeResidencySpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.MemoryModel.ProbeMode

-- | Pin each command independently of its renderer and preserve delayed validation.
spec :: Spec
spec = describe "metadata child modes" $ do
    for_ cases $ \(mode, arguments) -> it (show mode) $ do
        renderProbeMode mode `shouldBe` arguments
        parseProbeMode (arguments <> ["capture.json"]) `shouldBe` Just (mode, "capture.json")
        parseProbeMode arguments `shouldBe` Nothing
        parseProbeMode (arguments <> ["capture.json", "extra"]) `shouldBe` Nothing
    it "keeps malformed values for the existing probe error paths" $ do
        parseProbeMode ["--metadata-probe", "invalid", "missing"] `shouldBe` Just (Retained "invalid", "missing")
        parseProbeMode ["--metadata-source-probe", "invalid", "invalid", "", "", "-1", "missing"]
            `shouldBe` Just (Source "invalid" "invalid" "" "" "-1", "missing")
        parseProbeMode ["--metadata-selected-retention-probe", "invalid", "", "", "no", "missing"]
            `shouldBe` Just (SelectedRetention "invalid" "" "" "no", "missing")
    it "leaves unrelated, unknown and removed legacy modes to Hspec" $ do
        for_ [[], ["--archive-probe", "mode", "path"], ["--gzip-probe", "mode", "path"], ["--mirror-state-probe", "mode", "path"], ["--mirror-read-probe", "phase", "directory"], ["--unknown"], ["--metadata-source-probe", "mode", "name", "version", "limit", "path"]] $ \arguments ->
            parseProbeMode arguments `shouldBe` Nothing
    it "keeps both listing RTS groups in their original positions" $ do
        probeArguments Listing "capture.json" `shouldBe` ["--metadata-listing-probe", "+RTS", "-F1", "-A128k", "-RTS", "capture.json", "+RTS", "-T", "-N1", "-RTS"]
        probeArguments (Merge "private" "public") "capture.json" `shouldBe` ["--metadata-merge-probe", "private", "public", "+RTS", "-F1", "-A128k", "-RTS", "capture.json", "+RTS", "-T", "-N1", "-RTS"]
    it "uses the same major sampling for explicit named sources" $ do
        probeArguments (ListingFor "rubygems" "bench-gem") "generated.info" `shouldBe` ["--metadata-listing-for", "rubygems", "bench-gem", "+RTS", "-F1", "-A128k", "-RTS", "generated.info", "+RTS", "-T", "-N1", "-RTS"]
        probeArguments (MergeFor "rubygems" "bench-gem" "private.info") "public.info" `shouldBe` ["--metadata-merge-for", "rubygems", "bench-gem", "private.info", "+RTS", "-F1", "-A128k", "-RTS", "public.info", "+RTS", "-T", "-N1", "-RTS"]
    it "keeps the default RTS arguments for retained, evaluation and census probes" $ do
        probeArguments (Retained "Shared") "capture.json" `shouldBe` ["--metadata-probe", "Shared", "capture.json", "+RTS", "-T", "-N1", "-RTS"]
        probeArguments Evaluation "capture.json" `shouldBe` ["--metadata-evaluation-probe", "capture.json", "+RTS", "-T", "-N1", "-RTS"]
        probeArguments ListingCensus "capture.json" `shouldBe` ["--metadata-listing-census-probe", "capture.json", "+RTS", "-T", "-N1", "-RTS"]
  where
    cases =
        [ (Retained "Shared", ["--metadata-probe", "Shared"])
        , (Evaluation, ["--metadata-evaluation-probe"])
        , (Listing, ["--metadata-listing-probe"])
        , (ListingFor "rubygems" "bench-gem", ["--metadata-listing-for", "rubygems", "bench-gem"])
        , (MergeFor "rubygems" "bench-gem" "private", ["--metadata-merge-for", "rubygems", "bench-gem", "private"])
        , (ListingCensus, ["--metadata-listing-census-probe"])
        , (Merge "private" "public", ["--metadata-merge-probe", "private", "public"])
        , (Source "npm" "Full" "react" "1" "42", ["--metadata-source-probe", "npm", "Full", "react", "1", "42"])
        , (SelectedRetention "pypi" "numpy" "1" "42", ["--metadata-selected-retention-probe", "pypi", "numpy", "1", "42"])
        , (SelectedControl "pypi" "numpy" "1" "42", ["--metadata-selected-control-probe", "pypi", "numpy", "1", "42"])
        ]

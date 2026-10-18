-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Public request labels retain operation identity and exact HTTP outcomes.
module Ecluse.Core.Telemetry.UpstreamSpec (spec) where

import Test.Hspec

import Ecluse.Core.Telemetry.Upstream

spec :: Spec
spec = describe "public request vocabulary" $ do
    it "names each supported public operation" $
        map publicReadKindName [Listing, SelectedRead, Artifact, Companion, WorkerMetadata, WorkerArtifact]
            `shouldBe` ["listing", "selected_read", "artifact", "companion", "worker_metadata", "worker_artifact"]
    it "distinguishes throttling, other HTTP responses, transport failure, and cancellation" $
        map exchangeOutcomeName [ResponseStatus 200, ResponseStatus 403, ResponseStatus 429, ResponseStatus 503, TransportFailure, Cancelled]
            `shouldBe` ["200", "403", "429", "503", "transport_failure", "cancelled"]

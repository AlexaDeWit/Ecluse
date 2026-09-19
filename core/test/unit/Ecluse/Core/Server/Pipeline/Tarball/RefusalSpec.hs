-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The answer a first-party name's private miss renders. One miss decides one status, one metric
class, and one pair of denial labels, wherever the leg dispatch reached it.
-}
module Ecluse.Core.Server.Pipeline.Tarball.RefusalSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Pipeline.Internal (denialLabels, serveDecisionClass)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactOutcomeStatus, firstPartyMissRefusal)
import Ecluse.Core.Server.Response (ArtifactStatus (NotFound, Unavailable'))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Test.Server.Response (reasonOf)

spec :: Spec
spec = describe "the private miss a first-party name answers" $ do
    it "renders an origin that holds no such artifact as a 404 the first-party rule decided" $ do
        artifactOutcomeStatus (firstPartyMissRefusal MissAbsent) `shouldBe` NotFound
        serveDecisionClass (firstPartyMissRefusal MissAbsent) `shouldBe` Metric.Deny

    it "renders an origin that was never read as a 503, suggesting no delay" $ do
        artifactOutcomeStatus (firstPartyMissRefusal MissUnresolved) `shouldBe` Unavailable' Nothing
        serveDecisionClass (firstPartyMissRefusal MissUnresolved) `shouldBe` Metric.Unavailable

    it "carries the denial labels the artifact path records for each miss" $ do
        fmap denialLabels (reasonOf (firstPartyMissRefusal MissAbsent))
            `shouldBe` Just (Just "first-party", Metric.ReasonPolicy)
        fmap denialLabels (reasonOf (firstPartyMissRefusal MissUnresolved))
            `shouldBe` Just (Nothing, Metric.ReasonUnavailable)

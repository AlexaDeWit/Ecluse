-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The answer a first-party name's private miss renders. One miss decides one status, one metric
class, and one pair of denial labels, wherever the leg dispatch reached it.
-}
module Ecluse.Core.Server.Pipeline.Tarball.RefusalSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Context (PackumentDeps (pdFirstParty))
import Ecluse.Core.Server.Pipeline.Internal (denialLabels, serveDecisionClass)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactOutcomeStatus, privateMissRefusal)
import Ecluse.Core.Server.Response (ArtifactStatus (NotFound, Unavailable'), ServeDecision)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Test.Package (leftpadName)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
import Ecluse.Test.Server.Response (reasonOf)

spec :: Spec
spec = describe "the private miss a first-party name answers" $ do
    it "renders an origin that holds no such artifact as a 404 the first-party rule decided" $ do
        fmap artifactOutcomeStatus (firstPartyMiss MissAbsent) `shouldBe` Just NotFound
        fmap serveDecisionClass (firstPartyMiss MissAbsent) `shouldBe` Just Metric.Deny

    it "renders an origin that was never read as a 503, suggesting no delay" $ do
        fmap artifactOutcomeStatus (firstPartyMiss MissUnresolved) `shouldBe` Just (Unavailable' Nothing)
        fmap serveDecisionClass (firstPartyMiss MissUnresolved) `shouldBe` Just Metric.Unavailable

    it "carries the denial labels the artifact path records for each miss" $ do
        fmap denialLabels (reasonOf =<< firstPartyMiss MissAbsent)
            `shouldBe` Just (Just "first-party", Metric.ReasonPolicy)
        fmap denialLabels (reasonOf =<< firstPartyMiss MissUnresolved)
            `shouldBe` Just (Nothing, Metric.ReasonUnavailable)

-- The decision for a name the deployment declares first-party.
firstPartyMiss :: OriginMiss -> Maybe ServeDecision
firstPartyMiss = privateMissRefusal inertPackumentDeps{pdFirstParty = (== leftpadName)} leftpadName

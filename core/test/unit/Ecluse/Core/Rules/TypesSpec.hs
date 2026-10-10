-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What the two evidence builders carry, and what a reason's advisory list guarantees.
A determined absence and an unread fact must stay distinguishable.
-}
module Ecluse.Core.Rules.TypesSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Package (
    CodeExecSignal (RunsCodeOnInstall),
    PackageDetails (pkgInstallCode, pkgPublishedAt),
 )
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss),
    Fact (Known, Unread),
    Reason (AffectedBy, FixesButStillAffected, Remediates),
    RuleEvidence (evInstallCode, evName, evPublishedAt, evVersion),
    completeEvidence,
    identityEvidence,
    mkAdvisoryIds,
    unAdvisoryIds,
 )
import Ecluse.Test.Package (sampleDetails, unscopedNpm, v1_0_0)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

published :: UTCTime
published = UTCTime (fromGregorian 2026 3 1) 0

spec :: Spec
spec = do
    completeSpec
    identitySpec
    distinctionSpec
    advisoryIdsSpec

completeSpec :: Spec
completeSpec = describe "completeEvidence" $ do
    it "carries every fact the rules read as Known" $ do
        let details =
                (sampleDetails (unscopedNpm "left-pad") v1_0_0)
                    { pkgPublishedAt = Just published
                    , pkgInstallCode = RunsCodeOnInstall "postinstall hook"
                    }
            evidence = completeEvidence details
        evName evidence `shouldBe` unscopedNpm "left-pad"
        evVersion evidence `shouldBe` v1_0_0
        evPublishedAt evidence `shouldBe` Known (Just published)
        evInstallCode evidence `shouldBe` Known (RunsCodeOnInstall "postinstall hook")

    it "keeps an absent publish time as a reading, not as an absent reading" $
        evPublishedAt (completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0))
            `shouldBe` Known Nothing

identitySpec :: Spec
identitySpec = describe "identityEvidence" $ do
    it "carries the identity a store listing establishes" $ do
        let evidence = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evName evidence `shouldBe` unscopedNpm "left-pad"
        evVersion evidence `shouldBe` v1_0_0

    it "carries no reading of any fact a manifest would supply" $ do
        let evidence = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt evidence `shouldBe` Unread
        evInstallCode evidence `shouldBe` Unread

{- A rule reads these two cases differently: it abstains on the first and refuses on the second,
so nothing may collapse them. -}
distinctionSpec :: Spec
distinctionSpec = describe "a determined absence against an unread fact" $
    it "gives the two builders different entries for the same package" $ do
        let read' = completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0)
            unread = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt read' `shouldNotBe` evPublishedAt unread
        evInstallCode read' `shouldNotBe` evInstallCode unread

laterAdvisory :: TestContractEscape
laterAdvisory = TestContractEscape "a later advisory was evaluated"

-- | Reasons whose advisory lists hold a later identifier, or a later stretch of list, that throws.
deferredReasons :: [(String, Reason)]
deferredReasons =
    [ ("a later identifier", Remediates (mkAdvisoryIds ("GHSA-a" :| [impureThrow laterAdvisory])))
    , ("the rest of the list", AffectedBy Cvss 7.0 (mkAdvisoryIds ("GHSA-a" :| "GHSA-b" : impureThrow laterAdvisory)))
    , ("the advisories still affecting a fix", FixesButStillAffected (mkAdvisoryIds ("GHSA-a" :| [])) (mkAdvisoryIds ("GHSA-b" :| [impureThrow laterAdvisory])))
    ]

{- A rule finds the later advisories by matching ranges lazily. Left unevaluated, that matching
would run where the reason is rendered, outside the handler that evaluated the verdict. -}
advisoryIdsSpec :: Spec
advisoryIdsSpec = describe "mkAdvisoryIds" $ do
    it "keeps the identifiers in the order given" $
        unAdvisoryIds (mkAdvisoryIds ("GHSA-a" :| ["GHSA-b"])) `shouldBe` "GHSA-a" :| ["GHSA-b"]

    for_ deferredReasons $ \(label, reason) ->
        it ("evaluates every advisory with the reason that names it: " <> label) $
            evaluate reason `shouldThrow` (== laterAdvisory)

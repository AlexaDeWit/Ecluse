-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What the two evidence builders carry.
A determined absence and an unread fact must stay distinguishable.
-}
module Ecluse.Core.Rules.TypesSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse.Core.Package (
    CodeExecSignal (RunsCodeOnInstall),
    PackageDetails (pkgInstallCode, pkgPublishedAt),
 )
import Ecluse.Core.Rules.Types (
    Fact (Known, Unavailable),
    RuleEvidence (evInstallCode, evName, evPublishedAt, evVersion),
    completeEvidence,
    identityEvidence,
 )
import Ecluse.Test.Package (sampleDetails, unscopedNpm, v1_0_0)

published :: UTCTime
published = UTCTime (fromGregorian 2026 3 1) 0

spec :: Spec
spec = do
    completeSpec
    identitySpec
    distinctionSpec

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
        evPublishedAt evidence `shouldBe` Unavailable
        evInstallCode evidence `shouldBe` Unavailable

{- A rule reads these two cases differently: it abstains on the first and refuses on the second,
so nothing may collapse them. -}
distinctionSpec :: Spec
distinctionSpec = describe "a determined absence against an unread fact" $
    it "gives the two builders different entries for the same package" $ do
        let read' = completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0)
            unread = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt read' `shouldNotBe` evPublishedAt unread
        evInstallCode read' `shouldNotBe` evInstallCode unread

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory laws require native artifact identity and complete refusal evidence.
module Ecluse.Test.Contract.Install.AdvisorySpec (spec) where

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec

import Ecluse.Test.Contract.Binding (allCaseIds)
import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Install.Advisory

spec :: Spec
spec = do
    describe "advisory admission evidence" $ do
        it "accepts the exact advertised artifact with a successful installation" $
            advisoryAdmits admitted
        for_ [("an absent", []), ("a substituted", [("other.whl", "digest")])] $ \(label, artifacts) ->
            it ("rejects " <> label <> " artifact before a successful installer runs") $ do
                called <- newIORef False
                let driver =
                        admitted
                            { advisoryAdvertisedArtifacts = artifacts
                            , advisoryInstall = writeIORef called True >> advisoryInstall admitted
                            }
                advisoryAdmits driver `shouldThrow` anyException
                readIORef called `shouldReturn` False
    describe "advisory refusal evidence" $ do
        for_ [("CVE", "DenyIfCve", "GHSA-fixture"), ("MAL", "DenyIfKnownMalicious", "MAL-derived-1")] $ \(label, rule, advisory) ->
            describe label $ do
                let expected = Denial "package" "1.0.0" rule (Just advisory)
                    refused = refusedDriver expected
                it "accepts a denied client with the exact structured denial and no installed release" $
                    advisoryRefuses "native refusal" expected refused
                it "rejects a refusal whose artifact still serves" $
                    advisoryRefuses "native refusal" expected refused{advisoryArtifactStatus = pure 200} `shouldThrow` anyException
                it "rejects an advisory record for another version" $
                    advisoryRefuses "native refusal" expected refused{advisoryAwaitDenials = const (pure [expected{denialVersion = "2.0.0"}])} `shouldThrow` anyException
                it "rejects an advisory record from another source" $
                    advisoryRefuses "native refusal" expected refused{advisoryAwaitDenials = const (pure [expected{denialAdvisory = Just "other-source"}])} `shouldThrow` anyException
                it "rejects files left behind by a failed client" $
                    advisoryRefuses "native refusal" expected refused{advisoryInstall = pure (result (ExitFailure 1), True)} `shouldThrow` anyException
                it "rejects a successful client in a required refusal" $
                    advisoryRefuses "native refusal" expected refused{advisoryInstall = pure (result ExitSuccess, False)} `shouldThrow` anyException
        describe "MAL session evidence" $ do
            it "requires the unaffected listing and an empty retaining store" $
                advisoryDenies maliciousCase "DenyIfKnownMalicious" maliciousSession
            it "rejects a lost unaffected release" $
                advisoryDenies maliciousCase "DenyIfKnownMalicious" maliciousSession{advisoryListedVersions = pure []} `shouldThrow` anyException
            it "rejects a denied release still present in the listing" $
                advisoryDenies maliciousCase "DenyIfKnownMalicious" maliciousSession{advisoryListedVersions = pure ["1.0.0", "1.2.0"]} `shouldThrow` anyException
            it "rejects a missing required mirror observation" $
                advisoryDenies maliciousCase "DenyIfKnownMalicious" maliciousSession{advisoryMirrorContains = Nothing} `shouldThrow` anyException
            it "rejects a denied release published to the mirror" $
                advisoryDenies maliciousCase "DenyIfKnownMalicious" maliciousSession{advisoryMirrorContains = Just (const $ pure True)} `shouldThrow` anyException
            it "permits a truthful read-only session without claiming mirror evidence" $
                advisoryDenies maliciousCase{advisoryRequiresMirror = False} "DenyIfKnownMalicious" maliciousSession{advisoryMirrorContains = Nothing}
        it "registers every advisory law without acquiring a session" $ do
            acquired <- newIORef False
            let driver = AdvisoryScenarioDriver (\_ action -> writeIORef acquired True >> action maliciousSession) (const maliciousCase)
            registered <- allCaseIds (beforeAll (pure driver) (advisoryContract "example/advisories"))
            registered `shouldMatchList` ["example/advisories/deny-cve", "example/advisories/deny-epss", "example/advisories/mal-denial", "example/advisories/exact-fix", "example/advisories/partial-fix"]
            readIORef acquired `shouldReturn` False
  where
    result code = ClientResult "client" code "" "native refusal"
    admitted =
        (refusedDriver (Denial "package" "1.0.0" "DenyIfCve" (Just "GHSA-fixture")))
            { advisoryAdvertisedArtifacts = [("expected.whl", "digest")]
            , advisoryInstall = pure (result ExitSuccess, True)
            }
    refusedDriver expected =
        AdvisoryDriver
            { advisoryExpectedArtifact = "expected.whl"
            , advisoryAdvertisedArtifacts = []
            , advisoryInstall = pure (result (ExitFailure 1), False)
            , advisoryArtifactStatus = pure 403
            , advisoryAwaitDenials = const (pure [expected])
            }
    maliciousCase = AdvisoryCase "package" "1.0.0" "1.2.0" "MAL-derived-1" (const "native refusal") True
    maliciousSession =
        AdvisorySession
            (pure ["1.2.0"])
            (const $ pure $ refusedDriver (Denial "package" "1.0.0" "DenyIfKnownMalicious" (Just "MAL-derived-1")))
            (Just (const $ pure False))

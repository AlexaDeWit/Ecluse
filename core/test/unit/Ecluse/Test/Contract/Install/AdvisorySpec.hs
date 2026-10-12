-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory laws reject a refusal that loses coordinate, artifact or installed-state evidence.
module Ecluse.Test.Contract.Install.AdvisorySpec (spec) where

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec

import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Install.Advisory

spec :: Spec
spec = describe "advisory refusal evidence" $ do
    it "accepts a denied client with the exact structured denial and no installed release" $
        advisoryRefuses "native refusal" expected refused
    it "rejects a refusal whose artifact still serves" $
        advisoryRefuses "native refusal" expected refused{advisoryArtifactStatus = pure 200} `shouldThrow` anyException
    it "rejects an advisory record for another version" $
        advisoryRefuses "native refusal" expected refused{advisoryAwaitDenials = const (pure [expected{denialVersion = "2.0.0"}])} `shouldThrow` anyException
    it "rejects files left behind by a failed client" $
        advisoryRefuses "native refusal" expected refused{advisoryInstall = pure (result (ExitFailure 1), True)} `shouldThrow` anyException
    it "rejects a successful client in a required refusal" $
        advisoryRefuses "native refusal" expected refused{advisoryInstall = pure (result ExitSuccess, False)} `shouldThrow` anyException
  where
    expected = Denial "package" "1.0.0" "DenyIfCve" (Just "GHSA-fixture")
    result code = ClientResult "client" code "" "native refusal"
    refused =
        AdvisoryDriver
            { advisoryInstall = pure (result (ExitFailure 1), False)
            , advisoryArtifactStatus = pure 403
            , advisoryAwaitDenials = const (pure [expected])
            }

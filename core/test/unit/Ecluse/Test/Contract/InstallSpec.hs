-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Contract.InstallSpec (spec) where

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec

import Ecluse.Test.Contract.Install

spec :: Spec
spec = describe "installation contract rejects broken observations" $ do
    it "accepts success with the independently expected installation" $
        installsObserved (driver ExitSuccess True True)
    it "rejects a successful client with no installation" $
        installsObserved (driver ExitSuccess False True) `shouldThrow` anyException
    it "rejects a failed client even when files exist" $
        installsObserved (driver (ExitFailure 1) True True) `shouldThrow` anyException
    it "rejects effects left behind by a refused installation" $
        refusesObserved (driver (ExitFailure 1) True False) `shouldThrow` anyException
    it "accepts refusal without an installation" $
        refusesObserved (driver (ExitFailure 1) False False)
  where
    driver code actual expected =
        InstallDriver
            { installRun = pure (ClientResult "fake client" code "" "", actual)
            , installExpected = expected
            }

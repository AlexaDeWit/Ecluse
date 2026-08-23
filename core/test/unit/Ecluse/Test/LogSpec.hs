-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.LogSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import UnliftIO.Exception (throwIO)

import Ecluse.Test.Log (captureStdout)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = describe "captureStdout" $ do
    it "returns what the action wrote to stdout" $
        captureStdout (putText "captured line") `shouldReturn` "captured line"

    it "restores stdout when the action throws, so a later capture still reads" $ do
        let escaped (TestContractEscape m) = m == "boom"
        captureStdout (throwIO (TestContractEscape "boom")) `shouldThrow` escaped
        captureStdout (putText "after the throw") `shouldReturn` "after the throw"

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.LogSpec (spec) where

import Data.Text.IO qualified as TIO
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import UnliftIO.Exception (throwIO)

import Ecluse.Test.Log (captureStderr, captureStdout)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "captureStdout" $ do
        it "returns what the action wrote to stdout" $
            captureStdout (putText "captured line") `shouldReturn` "captured line"

        it "restores stdout when the action throws, so a later capture still reads" $ do
            captureStdout (throwIO (TestContractEscape "boom")) `shouldThrow` escaped
            captureStdout (putText "after the throw") `shouldReturn` "after the throw"

    describe "captureStderr" $ do
        it "returns what the action wrote to stderr" $
            captureStderr (TIO.hPutStr stderr "captured line") `shouldReturn` "captured line"

        it "restores stderr when the action throws, so a later capture still reads" $ do
            captureStderr (throwIO (TestContractEscape "boom")) `shouldThrow` escaped
            captureStderr (TIO.hPutStr stderr "after the throw") `shouldReturn` "after the throw"

-- | The escape a capture must let through rather than swallow.
escaped :: TestContractEscape -> Bool
escaped (TestContractEscape message) = message == "boom"

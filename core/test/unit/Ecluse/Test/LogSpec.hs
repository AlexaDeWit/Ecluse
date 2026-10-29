-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.LogSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Katip (Severity (WarningS), logFM, ls)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)
import UnliftIO.Exception (throwIO)

import Ecluse.Test.Log (captureStderr, captureStdout, lineMessage, runJsonLog)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "lineMessage" $ do
        it "reads the exact message emitted by the native Katip scribe" $ do
            let original = "first line\nsecond line"
            logged <- runJsonLog (logFM WarningS (ls original))
            map lineMessage (T.lines logged) `shouldBe` [Just original]

        it "rejects malformed JSON and absent or non-text messages" $
            map lineMessage ["not JSON", "{}", "{\"message\":42}", "{\"msg\":42}"]
                `shouldBe` replicate 4 Nothing

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

escaped :: TestContractEscape -> Bool
escaped (TestContractEscape message) = message == "boom"

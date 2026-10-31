-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.LogSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Katip (Severity (WarningS), logFM, ls)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)
import UnliftIO.Exception (throwIO)

import Ecluse.Test.Log (captureStderr, captureStdout, lineField, lineMessage, lineWhere, moduleOfLineWhere, runJsonLog, untaggedLines)
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

    describe "lineField" $ do
        it "reads the string at a nested key path" $
            lineField ["data", "module"] "{\"data\":{\"module\":\"Ecluse.Server.Pipeline\"}}" `shouldBe` Just "Ecluse.Server.Pipeline"

        it "reads nothing from malformed JSON, an absent key, or a value that is not a string" $
            map (lineField ["data", "module"]) ["not JSON", "{}", "{\"data\":{}}", "{\"data\":{\"module\":42}}"]
                `shouldBe` replicate 4 Nothing

    describe "lineWhere" $ do
        let captured = T.unlines ["not JSON", "{\"msg\":\"first\",\"n\":\"1\"}", "{\"msg\":\"second\"}", "{\"msg\":\"first\",\"n\":\"2\"}"]
        it "finds the first line whose key path holds the value" $
            (lineField ["n"] =<< lineWhere ["msg"] "first" captured) `shouldBe` Just "1"

        it "finds nothing when no line holds the value" $
            lineWhere ["msg"] "third" captured `shouldBe` Nothing

    describe "moduleOfLineWhere" $ do
        let captured = T.unlines ["{\"msg\":\"first\",\"data\":{\"module\":\"A\"}}", "{\"msg\":\"second\"}", "{\"msg\":\"first\",\"data\":{\"module\":\"B\"}}"]
        it "reads the module of the first line whose key path holds the value" $
            moduleOfLineWhere ["msg"] "first" captured `shouldBe` Just "A"

        it "reads nothing when that line names no module, or no line holds the value" $
            map (\message -> moduleOfLineWhere ["msg"] message captured) ["second", "third"] `shouldBe` [Nothing, Nothing]

    describe "untaggedLines" $
        it "keeps the lines that name no module, a line that is not JSON included" $
            untaggedLines (T.unlines ["not JSON", "{\"data\":{\"module\":\"A\"}}", "{\"data\":{}}", "{\"data\":{\"module\":42}}"])
                `shouldBe` ["not JSON", "{\"data\":{}}", "{\"data\":{\"module\":42}}"]

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

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared framing preserves blank records, charges fragments and closes final unterminated rows.
module Ecluse.Core.Registry.Metadata.RecordStreamSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.JsonStream (Steps (..), StreamResult (..), readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps, recordSteps)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyArtifacts))
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Support (expectRight)

-- | Pin physical LF boundaries and propagate a consumer's structural refusal.
spec :: Spec
spec = describe "recordSteps" $ do
    it "preserves every physical record at each byte split" $ do
        let body = "one\n\ntwo\nthree"
        for_ [1 .. BS.length body - 1] $ \cut -> do
            next <- heldChunks [BS.take cut body, BS.drop cut body]
            let steps = recordSteps (\_ previous bytes -> pure (previous + BS.length bytes)) (\rows _ bytes -> Right (bytes : rows)) (Finished . reverse) []
            result <- expectRight =<< readStepsWithEnd id finishRecordSteps (MetadataBodyLimit 1024) steps next
            streamValue result `shouldBe` Right ["one", "", "two", "three"]
            streamBytes result `shouldBe` BS.length body
    it "returns a consumer's refusal without accepting a partial successful result" $ do
        next <- heldChunks ["one\ntwo\nthree"]
        let consume rows _ bytes = if length rows == 1 then Left (TooManyArtifacts 2 1) else Right (bytes : rows)
            steps = recordSteps (\_ previous bytes -> pure (previous + BS.length bytes)) consume Finished []
        readStepsWithEnd id finishRecordSteps (MetadataBodyLimit 1024) steps next `shouldReturn` Left (TooManyArtifacts 2 1)

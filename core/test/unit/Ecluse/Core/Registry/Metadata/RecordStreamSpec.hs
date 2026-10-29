-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared framing preserves blank records, charges fragments and closes final unterminated rows.
module Ecluse.Core.Registry.Metadata.RecordStreamSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.JsonStream (Steps (..), StreamResult (..), readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps, recordFragmentCharge, recordSteps)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyArtifacts))
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge)
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Support (expectRight)

-- | Pin physical LF boundaries, a consumer's structural refusal, and the shared fragment charge.
spec :: Spec
spec = do
    framingSpec
    fragmentChargeSpec

framingSpec :: Spec
framingSpec = describe "recordSteps" $ do
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

fragmentChargeSpec :: Spec
fragmentChargeSpec = describe "recordFragmentCharge" $ do
    it "adds the fragment's decode and buffer charges to what the record already reserved" $ do
        let fragment = "{\"a\":1}"
        recordFragmentCharge 7 fragment `shouldBe` 7 + decodedDocumentCharge fragment + bufferedChunkCharge (BS.length fragment)
    it "saturates at maxBound instead of wrapping" $
        recordFragmentCharge maxBound "x" `shouldBe` maxBound

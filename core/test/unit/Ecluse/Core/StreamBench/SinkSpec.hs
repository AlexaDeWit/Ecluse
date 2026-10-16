-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Every streaming bench row sends exactly the body it reads, so the rows compare equal output work.
module Ecluse.Core.StreamBench.SinkSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Ecluse.Core.StreamBench.Sink (bodyChunks, chunkSizes, newSinkBuffer, sentBody, sentBytes, strategies, streamBody)
import Ecluse.Test.Snapshot (digestOf)

spec :: Spec
spec = do
    describe "bodyChunks" $
        for_ chunkSizes $ \chunkSize ->
            it ("cuts the body into distinct " <> show chunkSize <> "-byte chunks") $ do
                let chunks = bodyChunks chunkSize
                ordNub (map BS.length chunks) `shouldBe` [chunkSize]
                length (ordNub chunks) `shouldBe` length chunks

    describe "the streaming bench's Warp sink" $
        for_ strategies $ \(name, pump) ->
            for_ chunkSizes $ \chunkSize -> do
                let chunks = bodyChunks chunkSize
                it (toString name <> " sends the whole body, in order, from " <> show chunkSize <> "-byte chunks") $ do
                    buffer <- newSinkBuffer
                    sent <- sentBody buffer pump chunks
                    -- A length and a digest keep a failure readable, where the bodies are 8 MiB each.
                    (BS.length sent, digestOf sent) `shouldBe` (BS.length streamBody, digestOf streamBody)
                    sentBytes buffer pump chunks `shouldReturn` BS.length streamBody

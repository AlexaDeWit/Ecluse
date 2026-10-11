-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pending declarations preserve tree-reader values and failures across chunks and duplicate members.
module Ecluse.Core.Registry.PyPI.PendingSidecarSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Json.Shape (Mode (Keep), Shape (ObjectWith, Scalar), Trees (..), knownMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.PyPI.PendingSidecar (PendingSidecar (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (readOutcome, testTable, walkJsonChunks)

spec :: Spec
spec = describe "PendingSidecar" $
    for_ declarations $ \body ->
        for_ [0 .. 3] $ \depth ->
            it ("matches the eager tree for " <> decodeUtf8 body <> " at depth " <> show depth) $
                for_ [0 .. BS.length body] $ \split -> do
                    let shape = ObjectWith depth (knownMembers ["sha256"] (Scalar (depth - 1))) (Scalar depth)
                        table = testTable []
                        bound = MetadataBodyLimit (BS.length body)
                        chunks = filter (not . BS.null) [BS.take split body, BS.drop split body]
                        deferred tokens = withElement tokens $ \element rest ->
                            readShape PendingSidecar shape Keep table element rest (\value _ _ -> Finished value)
                        eager tokens = withElement tokens $ \element rest ->
                            readShape Trees shape Keep table element rest (\value _ _ -> Finished value)
                    readOutcome (walkJsonChunks bound deferred chunks) `shouldBe` readOutcome (walkJsonChunks bound eager chunks)

declarations :: [ByteString]
declarations =
    [ "true"
    , "false"
    , "null"
    , "7"
    , "[]"
    , "{}"
    , "{\"sha256\":\"digest\"}"
    , "{\"custom\":\"first\",\"sha256\":\"second\",\"other\":\"third\"}"
    , "{\"sha256\":null,\"sha256\":\"repair\"}"
    , "{\"sha256\":\"first\",\"sha256\":null}"
    , "{\"custom\":\"first\",\"\\u0063ustom\":\"second\"}"
    , "{\"sha256\":{\"nested\":[[false]]}}"
    , "{\"sha256\":\"\\u00e9\"}"
    , "{\"sha256\":\"\\uQQQQ\"}"
    , "{\"sha256\":\"unfinished"
    ]

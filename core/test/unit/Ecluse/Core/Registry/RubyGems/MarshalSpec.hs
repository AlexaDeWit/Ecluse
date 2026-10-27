-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Marshal captures and malformed controls cover bounded data interpretation.
module Ecluse.Core.Registry.RubyGems.MarshalSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.RubyGems.Marshal
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Ruby Marshal data" $ do
    it "decodes the native Ruby-generated dependency response" $ do
        bytes <- readFileBS "test/fixtures/rubygems/dependencies.marshal"
        value <- expectRight (decodeMarshal defaultLimits bytes)
        case value of
            MArray rows -> do
                rows `shouldSatisfy` (not . null)
                for_ rows $ \case
                    MHash fields -> map fst fields `shouldBe` map MSymbol ["name", "number", "platform", "dependencies"]
                    other -> expectationFailure ("unexpected native row: " <> show other)
            other -> expectationFailure ("unexpected native response: " <> show other)
    it "emits symbol-key fields, native pairs and the empty capability probe" $ do
        encodeDependencies 4 [] `shouldBe` Right "\x04\x08[\x00"
        let row = DependencyRow "owned" "1.0" "ruby" [("child", ">= 1, < 2")]
        bytes <- expectRight (encodeDependencies 1024 [row])
        decodeMarshal defaultLimits bytes `shouldBe` Right (MArray [MHash [(MSymbol "name", MString "owned"), (MSymbol "number", MString "1.0"), (MSymbol "platform", MString "ruby"), (MSymbol "dependencies", MArray [MArray [MString "child", MString ">= 1, < 2"]])]])
        encodeDependencies (BS.length bytes - 1) [row] `shouldSatisfy` isLeft
    it "rejects trailing data, truncation, cyclic and forward references, and executable tags" $ do
        for_ ["\x04\x08\x30junk", "\x04\x08\x22\x09x", "\x04\x08[\x06@\x00", "\x04\x08@\x06", "\x04\x08;\x00", "\x04\x08\&e", "\x04\x08\&C", "\x04\x09\x30"] $ \bytes ->
            decodeMarshal defaultLimits bytes `shouldSatisfy` isLeft
    it "resolves completed native references without accepting cycles" $ do
        decodeMarshal defaultLimits "\x04\x08[\x07\x22\x06x@\x06" `shouldBe` Right (MArray [MString "x", MString "x"])
    it "refuses body, depth, node, collection and string bounds" $ do
        let nested = "\x04\x08[\x06[\x06\x30"
        decodeMarshal defaultLimits{maxNestingDepth = 1} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits{maxArtifactCount = 1} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits{maxMetadataBytes = 2} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits "\x04\x08[\x7f" `shouldSatisfy` isLeft

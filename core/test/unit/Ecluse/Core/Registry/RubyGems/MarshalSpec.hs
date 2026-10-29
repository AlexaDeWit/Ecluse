-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Marshal captures and malformed controls cover bounded data interpretation.
module Ecluse.Core.Registry.RubyGems.MarshalSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
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
    it "rejects trailing data, truncation, cyclic and forward references, and executable tags" $ do
        for_ ["\x04\x08\x30junk", "\x04\x08\x22\x09x", "\x04\x08[\x06@\x00", "\x04\x08@\x06", "\x04\x08;\x00", "\x04\x08\&e", "\x04\x08\&C", "\x04\x09\x30"] $ \bytes ->
            decodeMarshal defaultLimits bytes `shouldSatisfy` isLeft
    it "rejects malformed class-name chains before recursing or exhausting the node budget" $ do
        let chain = BS.replicate 100000 111
            limits = defaultLimits{maxArtifactCount = 2, maxNestingDepth = 1}
        for_ [111, 117, 85] $ \tag ->
            decodeMarshal limits ("\x04\x08" <> BS.singleton tag <> chain) `shouldBe` Left (ParseError "Marshal class or field name is not a symbol")
        decodeMarshal limits{maxArtifactCount = 3} ("\x04\x08o:\x06X\x06" <> chain) `shouldBe` Left (ParseError "Marshal class or field name is not a symbol")
    it "resolves completed native references without accepting cycles" $ do
        decodeMarshal defaultLimits "\x04\x08[\x07\x22\x06x@\x06" `shouldBe` Right (MArray [MString "x", MString "x"])
    it "counts expanded reference work and depth at the reference position" $ do
        decodeMarshal defaultLimits{maxArtifactCount = 7} "\x04\x08[\x07[\x07\x30\x30@\x06" `shouldBe` Left (ParseError "Marshal reference work exceeds its bound")
        decodeMarshal defaultLimits{maxNestingDepth = 3} "\x04\x08[\x07[\x06\x30[\x06[\x06@\x06" `shouldBe` Left (ParseError "Marshal referenced depth exceeds its bound")
    it "refuses body, depth, node, collection and string bounds" $ do
        let nested = "\x04\x08[\x06[\x06\x30"
        decodeMarshal defaultLimits{maxNestingDepth = 1} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits{maxArtifactCount = 1} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits{maxMetadataBytes = 2} nested `shouldSatisfy` isLeft
        decodeMarshal defaultLimits "\x04\x08[\x7f" `shouldSatisfy` isLeft

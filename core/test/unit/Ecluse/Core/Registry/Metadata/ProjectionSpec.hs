-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validation precedence and error payloads shared by metadata adapters.
module Ecluse.Core.Registry.Metadata.ProjectionSpec (spec) where

import Data.Aeson (Value (Null, Number, String))
import Test.Hspec

import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, streamError, validateReportedName)
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (LimitError (TooDeeplyNested), Limits (maxNestingDepth), defaultLimits)
import Ecluse.Test.Package (thingName, unscopedNpm)

-- | Pin validation precedence without depending on either ecosystem's wire grammar.
spec :: Spec
spec = do
    describe "validateReportedName" $ do
        for_ [Nothing, Just Null, Just (Number 1)] $ \value ->
            it ("rejects a missing or non-string name: " <> show value) $
                validateReportedName (Right . unscopedNpm) value `shouldBe` Left MetadataUndecodable

        it "maps a rejected ecosystem name to an undecodable document" $
            validateReportedName (const (Left (ParseError "invalid name"))) (Just (String "bad"))
                `shouldBe` Left MetadataUndecodable

        it "passes the reported string to the ecosystem name parser" $
            validateReportedName (Right . unscopedNpm) (Just (String "thing")) `shouldBe` Right thingName

    describe "projectionResult" $ do
        it "retains the accepted payload" $
            projectionResult (Projected (7 :: Int)) `shouldBe` Right 7
        it "retains the exact mismatched display name" $
            (projectionResult (NameMismatch "Other.Project") :: Either MetadataError Int)
                `shouldBe` Left (MetadataNameMismatch "Other.Project")

    describe "streamError" $ do
        it "maps malformed JSON" $
            streamError defaultLimits (ParseError "unusable metadata") `shouldBe` MetadataUndecodable
        it "retains the configured depth ceiling" $
            streamError defaultLimits{maxNestingDepth = 3} (ParseError "retained JSON nesting limit")
                `shouldBe` MetadataBoundExceeded (TooDeeplyNested 3)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which private-origin metadata outcomes leave the artifact leg nothing to stream.
module Ecluse.Core.Server.Pipeline.Tarball.PrivateSpec (spec) where

import Test.Hspec

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved), ReadFailure (ReadInvalid, ReadUnavailable))
import Ecluse.Core.Server.Pipeline.Tarball.Private (privateMetadataMiss)

spec :: Spec
spec = describe "privateMetadataMiss -- the metadata faults the artifact leg answers from" $ do
    it "settles only a 404 as an absence, and leaves an access refusal to its own reply" $ do
        privateMetadataMiss MetadataAbsent `shouldBe` Just MissAbsent
        privateMetadataMiss (MetadataAuthorisationFailure 403) `shouldBe` Nothing

    it "refuses a private identity fault as an invalid reply, so no public namesake serves" $ do
        privateMetadataMiss (MetadataNameMismatch "other-package") `shouldBe` Just (MissUnresolved ReadInvalid)
        privateMetadataMiss MetadataUndecodable `shouldBe` Just (MissUnresolved ReadInvalid)
        privateMetadataMiss (MetadataHttpFailure 302 Nothing) `shouldBe` Just (MissUnresolved ReadInvalid)

    it "keeps an unavailable private registry's delay" $
        privateMetadataMiss (MetadataHttpFailure 503 (Just (RetryAfter 30))) `shouldBe` Just (MissUnresolved (ReadUnavailable (Just (RetryAfter 30))))

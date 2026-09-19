-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which private-origin metadata outcomes leave the artifact leg nothing to stream.
module Ecluse.Core.Server.Pipeline.Tarball.PrivateSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry.Metadata (MetadataError (MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Tarball.Private (privateMetadataMiss)

spec :: Spec
spec = describe "privateMetadataMiss -- the metadata faults the artifact leg answers from" $
    it "settles a private identity fault rather than inviting a retry" $ do
        -- The packument pipeline renders this fault as a 502. This path has no such arm, so
        -- the name keeps the answer an absence gets.
        privateMetadataMiss (MetadataNameMismatch "other-package") `shouldBe` Just MissAbsent
        privateMetadataMiss MetadataUndecodable `shouldBe` Just MissUnresolved

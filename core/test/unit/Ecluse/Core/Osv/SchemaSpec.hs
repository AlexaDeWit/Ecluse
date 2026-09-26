-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pin the artifact's published object key.
Metadata keys must remain distinct.
-}
module Ecluse.Core.Osv.SchemaSpec (spec) where

import Prelude hiding (universe)

import Data.Universe.Class (Universe (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Osv.Schema (EpssEvidence (..), EpssStatus (..), MetaKey, decodeEpssEvidence, osvDbFileName, renderEpssStatus, renderMetaKey)

spec :: Spec
spec = do
    describe "decodeEpssEvidence" $ do
        it "recognises only the exact success marker" $
            decodeEpssEvidence (Just "available") `shouldBe` EpssAvailable
        for_ [Nothing, Just "", Just "unavailable", Just "unknown", Just "AVAILABLE", Just " available", Just "available "] $ \marker ->
            it ("does not establish enrichment from " <> show marker) $
                decodeEpssEvidence marker `shouldBe` EpssNotEstablished

    describe "renderEpssStatus" $
        -- The literals pin the stored values a consumer reads, so a rename breaks published artifacts.
        it "stores each outcome under its published spelling, and only success establishes enrichment" $ do
            map renderEpssStatus [EnrichmentAvailable, EnrichmentUnavailable] `shouldBe` ["available", "unavailable"]
            map (decodeEpssEvidence . Just . renderEpssStatus) [EnrichmentAvailable, EnrichmentUnavailable]
                `shouldBe` [EpssAvailable, EpssNotEstablished]

    describe "osvDbFileName" $ do
        -- The literal pins the published object key. A change here changes the
        -- writer and reader contract, so it must be a deliberate epoch bump.
        it "names the artifact by ecosystem and schema epoch" $
            osvDbFileName "npm" `shouldBe` "npm-osv-schema4.db"

    describe "renderMetaKey" $ do
        -- The literals pin the stored keys a consumer reads by name, each distinct from the
        -- rest. A rename here breaks every artifact already published under the epoch.
        it "renders each key to its published spelling" $
            map renderMetaKey (universe :: [MetaKey])
                `shouldBe` [ "pilot_version"
                           , "ecosystem"
                           , "built_at"
                           , "source_url"
                           , "epss_source_url"
                           , "osv_source"
                           , "osv_last_modified"
                           , "osv_newest_modified"
                           , "epss_source"
                           , "epss_last_modified"
                           , "epss_score_date"
                           , "epss_model_version"
                           , "epss_status"
                           , "row_count"
                           ]

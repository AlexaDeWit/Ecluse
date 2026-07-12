-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Osv.SchemaSpec (spec) where

import Prelude hiding (universe)

import Data.Universe.Class (Universe (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Osv.Schema (MetaKey, osvDbFileName, renderMetaKey)

spec :: Spec
spec = do
    describe "osvDbFileName" $ do
        -- The literal pins the published object key: a change here is a change
        -- to the writer/reader contract and must be a conscious epoch bump.
        it "names the artifact by ecosystem and schema epoch" $
            osvDbFileName "npm" `shouldBe` "npm-osv-schema3.db"

    describe "renderMetaKey" $ do
        it "renders every meta key to a distinct stored form" $ do
            let keys = map renderMetaKey (universe :: [MetaKey])
            ordNub keys `shouldBe` keys

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native cache diagnostics preserve source fields without inventing a JSON wire protocol.
module Ecluse.Test.Server.CacheSpec (spec) where

import Data.Aeson (object, (.=))
import Test.Hspec

import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus.RubyGems (benchGem, projectRubyGemsInfo)
import Ecluse.Test.Server.Cache (diagnosticDocumentValue)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native cache diagnostics" $
    it "retains package, original header and every complete raw platform row" $ do
        let header = "opaque_header:retained\n---\n"
            source = "1.0.0-java |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef,opaque:preserved" :: Text
        (_, document) <- expectRight (projectRubyGemsInfo "https://rubygems.org" defaultLimits benchGem (encodeUtf8 (header <> source <> "\n")))
        diagnosticDocumentValue document
            `shouldBe` object
                [ "name" .= ("bench-gem" :: Text)
                , "header" .= (header :: Text)
                , "rows" .= [source]
                ]

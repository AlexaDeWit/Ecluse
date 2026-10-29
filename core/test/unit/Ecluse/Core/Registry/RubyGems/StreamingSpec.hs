-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Frozen native outputs remain required before corpus performance evidence can qualify.
module Ecluse.Core.Registry.RubyGems.StreamingSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Test.Corpus (cpName, readCapture, rubyGemsCaptureUpstream, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Outputs (CorpusRead (..), captureOutputs, recordedOutputs, rendered, selectedFacts)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems corpus captures" $ for_ rubyGemsCorpusPackages $ \package ->
    it ("reproduces recorded native outputs for " <> toString (cpName package)) $ do
        bytes <- readCapture package
        let provider =
                CorpusRead
                    { crUpstream = rubyGemsCaptureUpstream
                    , crRead = gemReadWith (const pass) (const pass) "https://rubygems.org"
                    , crMetadata = MetadataRendering assembleRubyGemsDocument serialiseRubyGemsDocument
                    , crVersionReads = \selected _ _ -> [("selected", rendered (selectedFacts <$> selected))]
                    , crDocumentReads = \_ _ -> []
                    }
        actual <- captureOutputs provider package bytes >>= expectRight
        expected <- recordedOutputs package
        actual `shouldBe` expected

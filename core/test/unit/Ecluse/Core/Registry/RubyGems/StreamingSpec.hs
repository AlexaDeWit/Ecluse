-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete native captures pin production outputs and match the independent reference reader.
module Ecluse.Core.Registry.RubyGems.StreamingSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (cpName, cpPackage, readCapture, rubyGemsCaptureUpstream, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Outputs (CorpusRead (..), captureOutputs, recordedOutputs, rendered, selectedFacts)
import Ecluse.Test.Corpus.RubyGems (projectRubyGemsInfo, projectRubyGemsTree)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems corpus captures" $ for_ rubyGemsCorpusPackages $ \package -> do
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
    -- Recorded outputs alone would pin a regenerated wrong reading, so the reference reader judges it too.
    it ("agrees with the independent reference reader for " <> toString (cpName package)) $ do
        bytes <- readCapture package
        reference <- expectRight (projectRubyGemsTree defaultLimits (cpPackage package) bytes)
        actual <- expectRight (projectRubyGemsInfo "https://rubygems.org" defaultLimits (cpPackage package) bytes)
        actual `shouldBe` reference

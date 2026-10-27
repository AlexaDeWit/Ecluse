-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native transforms retain source framing and complete platform coverage.
module Ecluse.Test.Corpus.RubyGemsSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (cpName, cpPackage, readCapture, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.RubyGems (decodeRubyGemsValues, encodeRubyGemsValues, heavyRubyGemsBase, keepRubyGemsVersions, projectRubyGemsInfo, projectRubyGemsTree, rubyGemsVersionKeys, syntheticInfoBytes)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems corpus transforms" $ do
    for_ rubyGemsCorpusPackages $ \package ->
        it ("preserves every captured byte: " <> toString (cpName package)) $ do
            raw <- readCapture package
            document <- expectRight (decodeRubyGemsValues raw)
            encodeRubyGemsValues document `shouldBe` Right raw
            oracle <- expectRight (projectRubyGemsTree defaultLimits (cpPackage package) raw)
            native <- expectRight (projectRubyGemsInfo "https://rubygems.org" defaultLimits (cpPackage package) raw)
            native `shouldBe` oracle
    it "preserves an opaque header ending with delimiter-like text" $ do
        let raw = "opaque---\n" <> platformHistory
        document <- expectRight (decodeRubyGemsValues raw)
        encodeRubyGemsValues document `shouldBe` Right raw
    it "keeps all native platforms for an exact raw version" $ do
        document <- expectRight (decodeRubyGemsValues platformHistory)
        kept <- expectRight (encodeRubyGemsValues (keepRubyGemsVersions (Set.singleton "1.0.0") document))
        rows <- expectRight (referenceInfo kept)
        map rowVersion rows `shouldBe` ["1.0.0", "1.0.0"]
        map rowPlatform rows `shouldBe` ["ruby", "x86_64-linux-musl"]
        map rowFields rows `shouldBe` [[("checksum", checksum), ("opaque", "retained")], [("checksum", checksum)]]
    it "adds heavy metadata without changing native installation fields" $ do
        document <- expectRight (decodeRubyGemsValues platformHistory)
        enlarged <- expectRight (encodeRubyGemsValues (heavyRubyGemsBase 4096 document))
        originalRows <- expectRight (referenceInfo platformHistory)
        enlargedRows <- expectRight (referenceInfo enlarged)
        map installationFields enlargedRows `shouldBe` map installationFields originalRows
        all (any ((== "ecluse_benchmark") . fst) . rowFields) enlargedRows `shouldBe` True
    it "generates the requested distinct native releases" $
        for_ [1, 8, 64] $ \count -> do
            versions <- expectRight (rubyGemsVersionKeys (syntheticInfoBytes count))
            length versions `shouldBe` count
    it "refuses a missing header or incomplete final record" $ do
        decodeRubyGemsValues "1.0.0 |checksum:abc\n" `shouldSatisfy` isLeft
        decodeRubyGemsValues "---\n1.0.0 |checksum:abc" `shouldSatisfy` isLeft
  where
    installationFields row = (rowPosition row, rowVersion row, rowPlatform row, rowDependencies row, filter ((/= "ecluse_benchmark") . fst) (rowFields row))

platformHistory :: ByteString
platformHistory =
    "created_at:2020-01-01T00:00:00Z\n---\n"
        <> "1.0.0 dependency:>= 0|checksum:"
        <> encodeUtf8 checksum
        <> ",opaque:retained\n"
        <> "1.0.0-x86_64-linux-musl |checksum:"
        <> encodeUtf8 checksum
        <> "\n"
        <> "2.0.0.pre |checksum:"
        <> encodeUtf8 checksum
        <> "\n"

checksum :: Text
checksum = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
